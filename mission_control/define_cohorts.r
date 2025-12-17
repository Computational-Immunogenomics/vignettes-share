source("/data/repos/cancer-vignettes/mission_control/shortcuts.r")

### Functions to create Cohorts ### 
q <- function( query = "execute_sql_on_prod 'select * from hmfpatients.sample'" ){
    system_call <- system(query, intern = TRUE)
    as.data.frame(fread(text = system_call))
}

### Needed queries ### 
clinical <- q("execute_sql_on_prod 'select * from hmfpatients.clinical'") %>% se(sampleId, patientId, primaryTumorLocation, primaryTumorType)
purity <- q("execute_sql_on_prod 'select * from hmfpatients.purity'")
doids <- q("execute_sql_on_prod 'select * from hmfpatients.doidNode'")
patient <- q("execute_sql_on_prod 'select * from hmfpatients.patient'") 
dr <- q("execute_sql_on_prod 'select sampleId from hmfpatients.datarequest_all'")

### Define cohorts ### 
define_cohort <- function(cohort = "GLOW", base_cohort = "All", primaryTumorLocation = "a", acronym = "b", name = "c", test_samples = NA, base_samples = NA) {
    if (is.na(base_cohort)){ base_cohort <- paste0("Non_", cohort)}
    meta <- data.frame(cohort = c(base_cohort, cohort),primaryTumorLocation = primaryTumorLocation, acronymn = acronym, name = name)
    samples <-
    rbind(
     data.frame( cohort = base_cohort, ref = TRUE, sampleId = base_samples),
     data.frame( cohort = cohort, ref = FALSE, sampleId = test_samples)
    )
    samples %>%
      lj(meta, by = "cohort") %>%
      lj(clinical %>% se(sampleId, patientId), by = "sampleId")
}


### Filter samples with highest purity per patient ### 
high_purity <-
clinical %>% 
 ij(purity %>% select(sampleId, purity), by = "sampleId") %>% 
 gb( patientId ) %>% 
 mu( max_purity = max( purity ) ) %>% 
 fi( purity == max_purity, sampleId %in% dr$sampleId ) %>%
 se(sampleId, patientId) %>% 
 ug()

### Ids to store for listing DOIDs in output ### 
ids <-
patient %>%
 se(id, donorId, cohortId) %>%
 lj(doids %>% se(patientId, doid, doidTerm), by = c("id" = "patientId"))

### Save cohorts ### 
save_cohorts <- function(i) {
 fp <- paste0(COHORTS_DIR, paste0(i, ".rds"))
 saveRDS( list("cohorts" = cohorts, "id" = ids), file = fp)
 print(paste0("File saved: ", fp))
}
