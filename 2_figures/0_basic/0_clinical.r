source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source(paste0(REF_DIR, "colors.r"))
color <- hartwig_red

base <- readRDS(paste0( READY_DATA, "0_basic-4_compute.rds"))

alphas = list()
for( i in as.character(unique(base$clin_avail$field))){
 if(i %in% c("Age, Sex", "Pretreatment", "RNASeq", "Biopsy site", "WGS")){ alphas[[i]] <- 1} 
 else { alphas[[i]] <- .5}
}

availability_plot <- function( i ){
 base_data <- base$clin_avail %>% filter(cohort == i) 

 trt <- base_data %>% filter(field == "Treatment") %>% pull(avail)
 wgs <- base_data %>% filter(field == "WGS") %>% pull(avail)

 x <- max(base_data$avail)
 ggplot( data = base_data, aes( y = field, x = avail, label = avail, alpha = as.character(field) )) + 
  geom_bar(stat = "identity", fill = color, color = "black", width = .8) + 
  scale_color_identity() + 
  scale_alpha_manual(values = unlist(alphas)) + 
  facet_grid(~extra) + 
  scale_x_continuous(limits = c(0,x*1.2)) + 
  geom_text(inherit.aes = FALSE, aes(label = avail, y = field, x = avail, hjust = -.22), size = 4) + 
  ggtitle( "Data availability") + 
  geom_segment(aes(x = .8*wgs, y = 1, xend = .95*wgs, yend = 1),arrow = arrow(length = unit(.3, "cm"))) +
  geom_segment(aes(x = .8*wgs, y = 2, xend = .95*wgs, yend = 2),arrow = arrow(length = unit(.3, "cm"))) 
}

treatment_plot <- function( i ){
 base_data <- base$trts_ready %>% filter( cohort == i ) 
 ct_max <- max( base_data %>% pull(ct), na.rm = TRUE)

 ggplot( data = base_data, aes( y = field, x = ct, label = ct) ) + 
  geom_bar(stat = "identity", fill = color, alpha = .5, color = "black", width = .8) + 
  facet_grid(~label2, scales = "free_x") + 
  scale_x_continuous(limits = c(0,ct_max*1.2)) +
  geom_text(data = base_data, aes(label = label, hjust =  -.22), size = 4) +
  ggtitle( "Patients with treatment and response data")
}

bar_plots <- list()
bar_plots[['avail']] <- availability_plot( plt_cohort )
bar_plots[['trt']] <- treatment_plot( plt_cohort )

saveRDS(bar_plots, paste0( READY_FIG, "0_basic-0_clinical.rds"))
