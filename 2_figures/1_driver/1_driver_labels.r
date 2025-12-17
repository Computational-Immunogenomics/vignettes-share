source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source("../../mission_control/colors.r")

labels_base <- readRDS( paste0( READY_DATA, "1_driver-3_driver_labels.rds")) 

plot_driver_labels <- function( i = "SOMATIC" ) {  
  df <- labels_base[[i]]
  levels <- rev( df %>% fi(field == plt_cohort) %>% ar(desc(val0)) %>% pu(gene))
  df <- df %>% mu(gene = factor(gene, levels = levels))
  ggplot( data = df, aes( x = field2, y = gene, label = val)) + 
  geom_text(size = 3) 
}

ready <- list()
for ( i in c("GERMLINE", "SOMATIC")){
  if(is.data.frame(labels_base[[i]])){ready[[i]] <- plot_driver_labels(i)} 
  else {ready[[i]] <- NA}
}

saveRDS(ready, paste0( READY_FIG, "1_driver-1_driver_labels.rds"))
