source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/actionable_rules.r")
source("../../mission_control/set_cohort.r")
source("../../mission_control/colors.r")

onco_base <- readRDS( paste0( READY_DATA, "2_actionable-2_actionable_ready.rds") )

refs <- 
onco_base %>% 
 gb(sampleId) %>% 
 su( actionable_event = (sum(top_level != "None") != 0) ) %>% 
 mu( pct_actionable = mean(actionable_event), tot_actionable = sum(actionable_event))

onco_base_events<- onco_base %>% lj(refs, by = c("sampleId")) %>% fi(event != "None")

plot_onco <- function ( df = onco_base_events ) {
 pct_actionable <- df$pct_actionable[1]
 tot_actionable <- df$tot_actionable[1]

 ggplot(data = df %>% filter(actionable_event), 
        aes(x = sampleId, y = event, fill = top_level)) + 
 geom_tile() + 
 scale_fill_manual(
  values = unlist(actionable_fill_map),
  breaks = names(actionable_fill_map)[-which(names(actionable_fill_map) == "None")]) + 
 labs(x = paste0( tot_actionable, " Patients"))
}

actionable_onco <- plot_onco(onco_base_events) 

saveRDS(actionable_onco, paste0( READY_FIG, "2_actionable-0_onco.rds"))
