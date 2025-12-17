source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")

drivers <- 
fread(paste0( READY_DATA, "1_driver-0_onco_base.txt")) %>% 
 fi(driver_simple %in% c("AMPLIFICATION", "DELETION")) %>% 
 tm(sampleId, gene, status = driver_simple)

gene_locations <- fread(paste0( REFS_DIR, "gene_locations.txt")) %>% se(gene, chromosome, start)

cn <- 
fread( paste0(QUERY_DATA,"cn_amp_del.txt") ) %>% 
 mu( status = ifelse(high_amp == 1, "AMPLIFICATION", "DELETION") ) %>% 
 se( sampleId, gene, status )

k <- 1000000

gene_map <-
gene_locations %>% 
 mu( 
  base = floor(start/k)*k, 
  start = str_trim(format(base, scientific = F)), 
  end = str_trim(format(base+k, scientific = F)),
  chr = paste0("hs", chromosome)) %>% 
 se( gene, chr, start, end)

cn_drivers <- cohorts %>% ij(drivers, by = "sampleId", relationship = "many-to-many")
cn <- cohorts %>% ij(cn, by = "sampleId", relationship = "many-to-many")

k <- 10

ready <- 
cn_drivers %>% 
 group_by( cohort, status, gene ) %>% 
 summarise( ct = n() ) %>% 
 group_by(cohort, status) %>% 
 mutate( rk = row_number(desc(ct))) %>% 
 arrange(cohort, status, desc(ct)) %>% 
 ungroup() %>% 
 left_join(gene_map, by = "gene", relationship = "many-to-many") %>% 
 unique() %>% 
 drop_na(chr) %>% 
 group_by( cohort, status, chr) %>% 
 mutate( rk_chr = row_number(desc(ct))) %>% 
 filter( rk < k, rk_chr < 3, ct > 1) %>% 
 select( cohort, chr, start, end, gene, status, rk, rk_chr, ct) %>% 
 mutate( color = ifelse(status == "AMPLIFICATION", "color=dred", "color=vdblue"))

writer <- function( df, file) { fwrite( df, file, , sep = " ", col.names = FALSE)}
circos_writer <- function( i ){
    setwd(READY_DATA)
    base <- ready %>% filter(cohort == i) %>% ungroup()
    writer( base %>% transmute(chr, start, end, gene, color), paste0("4_circos_amp_del_genes.circos"))
}

for( i in unique(ready$cohort)) { 
    circos_writer(i) 
}
