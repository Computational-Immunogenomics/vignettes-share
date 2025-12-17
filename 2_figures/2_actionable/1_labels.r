source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")

onco_base <- readRDS( paste0( READY_DATA, "2_actionable-2_actionable_ready.rds") )
onco_labels <- readRDS( paste0( READY_DATA, "2_actionable-3_labels.rds") ) 

onco_labels <- 
onco_labels %>% 
 fi(event %in% onco_base$event) %>% 
 mu(event = factor(event, levels = levels(onco_base$event))) %>% 
 fi(!is.na(event))

plot_labels <- function( ) {
 ggplot( data = onco_labels, aes( x = field2, y = event, label = val)) + 
 geom_text(size = 3) 
}

actionable_labels <- plot_labels()

saveRDS(actionable_labels, paste0( READY_FIG, "2_actionable-1_labels.rds"))
