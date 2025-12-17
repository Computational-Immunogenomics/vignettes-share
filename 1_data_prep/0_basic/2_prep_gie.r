source("../../mission_control/shortcuts.r")
source("../../mission_control/map.r")

ploidy <- query("execute_sql_on_prod 'select sampleId, ploidy from hmfpatients.purity'")

gie_somatic <- fread( paste0(QUERY_DATA,"gie_somatic.txt"))
gie_cn <- fread( paste0(QUERY_DATA,"gie_cn.txt"))
gie_hr <- fread( paste0(QUERY_DATA,"gie_hr.txt"))

gie_cn_ready <- 
ploidy %>% 
 lj( gie_cn, by = "sampleId" ) %>% 
 mu(minMajoAlleleCopyNumber = minCopyNumber - minMinorAlleleCopyNumber) %>% 
 tm(
  sampleId,
  gene,
  homozygous_del = ifelse(minCopyNumber < .5, TRUE, FALSE),
  loh =  ifelse( minMinorAlleleCopyNumber < 0.3 & minMajoAlleleCopyNumber > 0.7, TRUE, FALSE),
  amp = ifelse( minCopyNumber > 3 * ploidy, TRUE, FALSE),
  minCopyNumber,
  minMinorAlleleCopyNumber,
  minMajoAlleleCopyNumber, 
  ploidy)

gie_somatic_ready <-
gie_somatic %>% 
 fi( subclonalLikelihood < .5 ) %>% 
 mu( 
  lof = ifelse( canonicalCodingEffect %in% c("NONSENSE_OR_FRAMESHIFT", "SPLICE"), TRUE, FALSE),
  bi_non_syn = ifelse( biallelic == 1 & worstCodingEffect %in% c("MISSENSE"), TRUE, FALSE)) %>% 
 tm( sampleId, gene, lof, bi_non_syn) %>% 
 gb( sampleId, gene) %>% 
 su( lof = ifelse(sum(lof) > 0, TRUE, FALSE),
     bi_non_syn = ifelse(sum(bi_non_syn) > 0, TRUE, FALSE), .groups = 'drop') 

gie <-
gie_cn_ready %>% 
 lj(gie_somatic_ready, by = c("sampleId", "gene")) %>% 
 lj(gie_hr %>% transmute(sampleId, gene, hd = TRUE ), by = c("sampleId", "gene")) %>% 
 se(sampleId, gene, homozygous_del, loh, amp, lof, bi_non_syn, hd) %>% 
 replace_na(list("lof" = FALSE, "bi_non_syn" = FALSE, hd = FALSE)) %>% 
 drop_na(gene)

gie_mechanisms <- list(
    "HLA" = c("HLA-A", "HLA-B", "HLA-C"),
    "APM" = c("B2M", "CALR", "TAP1", "TAP2", "TAPBP", "NLRC5", "CIITA", "RFX5"),
    "IFG" = c("JAK1", "JAK2", "IRF2", "IFNGR1", "IFNGR2", "APLNR", "STAT1"),
    "PDL1" = c("CD274"),
    "CD58" = c("CD58"),
    "SETDB1" = c("SETDB1")
)

flip <- list()
for( i in names(gie_mechanisms)){
 genes <- gie_mechanisms[[i]]
 for( j in genes ){
  flip[[j]] <- i
 }
}

gie$pathway <- unlist(lapply(gie$gene, function(i) flip[[i]]))

gie_go <-
gie %>% 
 gb(sampleId, pathway) %>% 
 su(
  h_del = sum(homozygous_del), 
  loh = sum(loh), 
  amp = sum(amp),
  lof = sum(lof),
  bi_non_syn = sum(bi_non_syn),
  hd = sum(hd), .groups = 'drop') %>% 
 mu( 
  gie_hla = ifelse( pathway == "HLA" & ( loh > 0 | h_del > 0 | lof > 0 | bi_non_syn > 0), TRUE, FALSE), 
  gie_hla_lof = ifelse( pathway == "HLA" & ( bi_non_syn > 0 | lof > 0 | hd > 0), TRUE, FALSE),
  gie_hla_loh = ifelse( pathway == "HLA" & ( loh > 0 | h_del > 0 ), TRUE, FALSE),
  gie_apm = ifelse( pathway == "APM" & ( h_del > 0 | lof > 0 | bi_non_syn > 0 | hd > 0), TRUE, FALSE),
  gie_ifg = ifelse( pathway == "IFG" & ( h_del > 0 | lof > 0 | bi_non_syn > 0 | hd > 0), TRUE, FALSE),
  gie_cd58 = ifelse( pathway == "CD58" & ( h_del > 0 | lof > 0 | bi_non_syn > 0 | hd > 0), TRUE, FALSE),
  gie_pdl1 = ifelse( pathway == "PDL1" & ( amp ), TRUE, FALSE), 
  gie_setdb1 = ifelse( pathway == "SETDB1" & ( amp ), TRUE, FALSE)) %>% 
 gb(sampleId) %>% 
 su( 
  gie_hla = sum(gie_hla), 
  gie_hla_lof = sum(gie_hla_lof),
  gie_hla_loh = sum(gie_hla_loh),
  gie_apm = sum(gie_apm),
  gie_ifg = sum(gie_ifg), 
  gie_pdl1 = sum(gie_pdl1), 
  gie_cd58 = sum(gie_cd58),
  gie_setdb1 = sum(gie_setdb1), .groups = 'drop')

gie_go$gie <- ifelse(apply( gie_go %>% se(contains("gie")), 1, sum) > 0, 1, 0)

fwrite( gie_go, file = paste0( READY_DATA, "0_basic-2_prep_gie.txt") )
