source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source("../../mission_control/actionable_rules.r")

protect <-  fread( paste0( READY_DATA, "2_actionable-0_protect.txt") ) 

evidence_levels <- c("FDA Approved", "Drug Repurposing", "Experimental/Other Guidelines", "Other", "None")

ready <- 
cohorts %>% 
 lj( protect, by = c("sampleId"), relationship = "many-to-many") %>% 
 mu( top_level = ifelse( is.na(top_level), "None", top_level),
         event = ifelse(is.na(event), "None", event)) %>% 
 se( sampleId, cohort, event, top_level) %>% 
 mu( top_level = factor(top_level, levels = evidence_levels)) %>% 
 gb( sampleId, cohort, event, top_level) %>% 
 mu( rk = row_number(top_level)) %>% ### removing any dups
 fi(rk == 1) %>% 
 ug()

ref <- 
ready %>% 
 gb( cohort, event ) %>% 
 su( ct = sum(top_level != "None"), .groups = "drop") %>% 
 gb( cohort ) %>% 
 mu( rk = row_number(desc(ct)) ) %>% 
 se(-ct) %>% 
 ug()

actionable_base <- 
ref %>% 
 lj(ready %>% se(sampleId, cohort) %>% unique(), by = "cohort", relationship = "many-to-many") %>% 
 lj(ready %>% select(sampleId, cohort, event, top_level), 
     by = c("sampleId", "cohort", "event"),relationship = "many-to-many") %>%  
 replace_na(list( top_level = 'None'))

fwrite( actionable_base, file = paste0( READY_DATA,"2_actionable-1_actionable_base.txt") )
