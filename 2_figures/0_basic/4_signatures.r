source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source(paste0(REF_DIR, "colors.r"))

ready <- readRDS(file = paste0( READY_DATA, "3_signatures-2_ready_sigs.rds" ) ) 

get_dressed <- function( ready, plt_cohort = "AYA", ref_cohort = "Non-AYA"){
 ready %>% 
  gb(cohort, gp, spot, .drop=FALSE) %>% 
  su(pct = round(100*sum(pct)), .drop=FALSE) %>% 
  mu(spot = ifelse(cohort == ref_cohort, spot + .3, spot)) %>% 
  rw() %>% 
  mu(col = ifelse(cohort == plt_cohort, sig_colors[[gp]], fills[['ref']])) %>% 
  ug() %>% 
  mu(order = ifelse(cohort == ref_cohort, 1, 2), 
     cohort = factor(cohort, levels = c(ref_cohort, plt_cohort)))
}

make_circle_plots <- function( base, plt_cohort = "AYA"){
 max_x <- max(base$pct) * 1.35
 ggplot(base, aes(x = 0, xend = pct, y=spot, yend = spot, color = col)) +
  geom_segment(color = "#bbbbbb", linewidth = 3.5, lineend = 'round', alpha = .9) +
  geom_segment(linewidth = 2.5, lineend = 'round') +
  scale_color_identity() +
  scale_y_continuous(limits = c(0, 7.5)) +
  scale_x_continuous(limits = c(0, max_x)) +
  geom_text(data=subset(base, cohort == plt_cohort), size=3.5, 
            aes( x=0, y=spot,label = paste0(gp, ": ",pct, "%    ")), 
            hjust="right", col="black") +
  coord_polar() 
}

go_circle <- function( cohort = "Prostate", ref = "Pan-Cancer" ){
 base <- get_dressed( ready, plt_cohort = cohort, ref_cohort = ref ) 
 make_circle_plots(base, cohort)
}

circles <- go_circle(cohort = plt_cohort, ref = ref_cohort)

saveRDS( circles ,paste0( READY_FIG, "0_basic-4_signatures.rds"))

READY_DATA

READY_FIG
