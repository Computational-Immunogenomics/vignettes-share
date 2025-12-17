source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source("../../mission_control/colors.r")

onco_base <- readRDS( paste0( READY_DATA, "1_driver-2_onco_ready.rds"))

refs <- list()
for ( i in names(onco_base)){
 if(is.data.frame(onco_base[[i]])){   
    refs[[i]] <- 
     onco_base[[i]] %>% 
      gb(sampleId) %>% 
      su(top_driver_event = sum(driver_simple != "none")!= 0) %>% 
      mu( pct_driver = mean(top_driver_event), tot_driver = sum(top_driver_event))
  } else {
   refs[[i]] <- NA
}}

onco_base_driver <- list()
for ( i in names(onco_base)){
 if(is.data.frame(onco_base[[i]])){      
  onco_base_driver[[i]] <- onco_base[[i]] %>% left_join(refs[[i]], by = c("sampleId"))
 } else {
  onco_base_driver[[i]] <- NA  
 }
}

plot_oncodriver <- function (i = "germline") {
 df <- onco_base_driver[[i]]
 pct_driver <- df$pct_driver[1]
 tot_driver <- df$tot_driver[1]
 x_label <- paste0( tot_driver, " Patients")

 if( i == "germline") { y_label = "Germline Drivers"} 
 else if (i == "somatic") { y_label = "Somatic Drivers" }

 ggplot(data = df %>% filter(top_driver_event), 
        aes(x = sampleId, y = gene, fill =  event)) +
 geom_tile() +
 scale_fill_manual(
    values = driver_fill_map, 
    breaks = names(driver_fill_map)[-which(names(driver_fill_map) == "none")]
 ) +  
 labs( x = x_label, y = y_label)
}

plts <- list()
for (i in c("somatic", "germline")){
  if(is.data.frame(onco_base[[i]])){plts[[i]] <- plot_oncodriver(i) } 
  else {plts[[i]] <- NA }
}

saveRDS(plts, paste0( READY_FIG, "1_driver-0_oncodriver.rds"))
