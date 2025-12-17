source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")

go <- fread( paste0( READY_DATA, "1_driver-0_onco_base.txt") ) 

grouper1 <- function(i){
 if(i == 0){ "None"} 
 else if (i < 6){ "1-5" } 
 else if (i >= 6){ "6+" } 
}

grouper2 <- function(i){
 if(i == 0){ "None" } 
 else if (i < 3){ "1-2" } 
 else if (i >= 3){ "3+" }
}

grouper3 <- function(i){
 if(i == 0){ "None" } 
 else if (i >= 1) { "1+" } 
}

collector <- function( df, grouper = grouper1, label){
 df %>% 
  gb(cohort, sampleId) %>% 
  su( tot = sum(driver_simple != "none"), .groups = "drop") %>% 
  rw() %>% 
  mu(tot_summary = grouper(tot)) %>% 
  gb(cohort) %>% 
  mu(tissue_ct = n()) %>% 
  gb(cohort, tot_summary) %>% 
  reframe(pct = n()/tissue_ct) %>% 
  unique() %>% 
  mu(label = label)
}

events_per_sample <- 
rbind(
 collector( go %>% filter(type == "SOMATIC"), grouper1, label = "Overall"), 
 collector( go %>% filter(type == "SOMATIC", category == "TSG" | driver_simple == "none"), grouper2, label = "TSG"),  
 collector( go %>% filter(type == "SOMATIC", category == "ONCO" | driver_simple == "none"), grouper2, label = "ONCO"),
 collector( go %>% filter(type == "GERMLINE" | driver_simple == "none"), grouper3, label = "GERMLINE")
) %>% ug()

saveRDS( events_per_sample, paste0( READY_DATA, "1_driver-4_events.rds"))
