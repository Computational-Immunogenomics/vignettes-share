source("../../mission_control/shortcuts.r")
source("../../mission_control/map.r")

clin <- fread(paste0( READY_DATA, "0_basic-0_prep_clin.txt"))
cn <- fread(  paste0( READY_DATA, "0_basic-1_prep_cn.txt"))
gie <- fread( paste0( READY_DATA, "0_basic-2_prep_gie.txt") )

som_var <- 
fread( paste0(QUERY_DATA,'somatic_variant.txt')) %>% se(-worstCodingEffect)

som_clonal <- 
som_var %>% 
 gb(sampleId) %>% 
 su( clonal = mean(clonal > .8), .groups = 'drop')

som_variant_types <-
som_var %>%
 gb( sampleId, type) %>% 
 su( ct = sum(ct), .groups = 'drop' ) %>% 
 sp( type, ct )

purity <- 
query("execute_sql_on_prod 'select * from purity'") %>% 
 se(sampleId, tmbPerMb, tmbStatus, msIndelsPerMb, msStatus, wholeGenomeDuplication, 
    purity, ploidy, diploidProportion, svTmb)

telomere <- 
query("execute_sql_on_prod 'select * from telomereLength'") %>% 
 mu( telomere_tumor = ifelse(somaticTelomereLength <= 0, 0, somaticTelomereLength) ) %>% ### some are negative
 tm(sampleId, telomere_ratio = telomere_tumor/germlineTelomereLength+.025)

cdr3 <- 
query("execute_sql_on_prod 'select * from cdr3Sequence'") %>% 
 fi(filter == "PASS") %>% 
 gb(sampleId) %>% 
 su(cdr3_seqs = n(), .groups = 'drop') %>% 
 mu( cdr3_seqs = ifelse(is.na(cdr3_seqs), 1, cdr3_seqs + 1))

chord <- 
query("execute_sql_on_prod 'select * from chord'") %>% 
 se(sampleId, hrStatus)

viral <- 
query("execute_sql_on_prod 'select * from hmfpatients.virusAnnotation'") %>% 
 fi(reported == 1) %>% 
 tm(sampleId, viral_insertion = ifelse( integrations > 0, 1, 0) )

combined <- 
purity %>%
 lj(som_clonal, by = "sampleId" ) %>% 
 lj(som_variant_types, by = "sampleId") %>% 
 lj(clin, by = "sampleId") %>% 
 lj(cn, by = "sampleId") %>% 
 lj(gie, by = "sampleId") %>% 
 lj(telomere, by = "sampleId") %>% 
 lj(cdr3, by = "sampleId") %>% 
 lj(viral, by = "sampleId") %>% 
 lj(chord, by = "sampleId") %>% 
 mu(tmbStatus = ifelse(tmbStatus == "HIGH", 1, 0), 
    msStatus = ifelse(msStatus == "MSI", 1, 0), 
    hrStatus = ifelse(hrStatus == "HR_DEFICIENT", 1, 0))

fwrite( combined, file = paste0( READY_DATA, "0_basic-3_prep_ready.txt") )
