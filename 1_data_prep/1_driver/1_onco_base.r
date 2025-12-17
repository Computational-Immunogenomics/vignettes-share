source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")

driver <-  fread(  paste0( READY_DATA, "1_driver-0_driver.txt") )

ready <- 
cohorts %>% 
 se(sampleId, cohort) %>% 
 lj( driver, by = c("sampleId"), relationship = "many-to-many") %>% 
 unique() %>% 
 mu(event=ifelse(type =="GERMLINE" & event == "MUTATION", "SMALL_VARIANT", event)) %>% 
 mu(event=ifelse(type =="GERMLINE" & event != "SMALL_VARIANT", "STRUCTURAL_VARIANT", event)) 

ref <-
ready %>% 
 gb( cohort, type, sampleId, gene) %>% mu(rn = row_number()) %>% fi(rn == 1) %>% ### remove duplicates
 gb( cohort, type, gene ) %>% 
 su( ct = n(), .groups = "drop" ) %>% 
 gb( cohort, type) %>% 
 mu(rk = row_number(desc(ct))) %>% 
 se(-ct) %>% 
 ug()

tall_ref <- 
ref %>%
 lj(ready %>% 
     se(cohort,sampleId) %>% 
     unique(), by="cohort", 
    relationship = "many-to-many")

system.time(
onco_gene_base <-
lazy_dt(tall_ref) %>% 
 lj( 
  lazy_dt(ready) %>% 
    se(sampleId, cohort, gene, category, driver_simple, type, event), 
  by = c("sampleId", "cohort", "gene", "type")
 ) %>% 
 replace_na(list(driver_simple = 'none', 
                 category = 'none', 
                 type = "none", 
                 event = "none")) %>% 
 gb(cohort, sampleId, type, gene) %>% 
 mu(rn = row_number()) %>% fi(rn == 1) %>% ug() %>% se(-rn) %>% 
 drop_na(gene) %>% 
 as_tibble()
)

fwrite( onco_gene_base, file = paste0( READY_DATA, "1_driver-0_onco_base.txt"))
