source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source(paste0(REF_DIR, "colors.r"))

ready <- readRDS(paste0( READY_DATA, "0_basic-4_compute.rds"))$vt

format_medians <- function( df, ref_cohort ){
 base <- df %>% se(cohort, type, medians) %>% unique()
    
 base %>% 
  lj(base %>% fi(cohort == ref_cohort) %>% tm(type, ref = medians), by = "type") %>% 
  mu(sc = ifelse(ref < medians, 7, 1/7), sc2 = ifelse(ref < medians, 5, 1/5))   
}

quantile_plot <- function(plt_cohort = "AYA", ref_cohort = "Non-AYA"){
 
 go <- ready %>% fi(cohort %in% c(plt_cohort, ref_cohort))
    
 medians <- format_medians(go, ref_cohort)   
 cohort_median <- medians %>% fi(cohort == plt_cohort)
 ref_median <- medians %>% fi(cohort == ref_cohort)

 color_map <- c()
 color_map[plt_cohort] <- fills[['plt']] 
 color_map[ref_cohort] <- fills[['ref']]   
    
 base <- 
 ggplot( go, aes( x = quantile, y = ct)) + 
  geom_point(aes(color = cohort), size = .2) +
  geom_segment( data = medians, aes(x=0,y=medians,xend=1,yend=medians, color = cohort)) + 
  scale_color_manual( values = color_map) +
  facet_wrap(~type, nrow = 1, scales = "free_x") +
  scale_y_log10() +
  scale_x_continuous(breaks=seq(0,1,.5), limits = c(-.05,1.05), labels = scales::percent) +
  ggtitle("Number of Variants") +
  xlab("Quantile") + 
  geom_text(data = ref_median, aes(x = .5, y = medians*sc, label = scales::label_comma()(round(medians))), colour = color_map[[ref_cohort]], size = 4) + 
  geom_segment(data = ref_median,aes(x = .5, y = medians, xend = .5, yend = medians*sc2, alpha = .3), linetype=2, colour = color_map[[ref_cohort]])

 if( plt_cohort != ref_cohort){
  base <- 
  base + 
   geom_text(
    data = cohort_median, 
    aes(x = .5, y = medians*sc, label = scales::label_comma()(round(medians))), 
        colour = color_map[[plt_cohort]], size = 4) + 
   geom_segment(
    data = cohort_median,
    aes(x = .5, y = medians, xend = .5, yend = medians*sc2, alpha = .3), 
    linetype=2, colour = color_map[[plt_cohort]])
 }
 base
}

quantile_plt <- quantile_plot(plt_cohort, ref_cohort)

saveRDS(quantile_plt, paste0( READY_FIG, "0_basic-3_variant_types.rds"))
