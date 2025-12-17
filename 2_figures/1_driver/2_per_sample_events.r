source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source(paste0(REF_DIR, "colors.r"))
source(paste0(REF_DIR, "theme.r"))

shh(library(ggpubr, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.1/"))

ready <- 
rbind(
 readRDS( paste0( READY_DATA, "1_driver-4_events.rds") ),
 readRDS( paste0( READY_DATA, "2_actionable-4_events.rds"))) %>% 
 mu(cohort_clean = cohort)

color_map <- c()
color_map[plt_cohort] <- fills[['plt']]
color_map[ref_cohort] <- fills[['ref']]

alpha_map <- c(
    "None" = 0,
    "Oth" = .2,
    "Exp" = .4,
    "Off" = .6,
    "FDA" = .8,
    "6+" = .8,
    "3+" = .8,
    "1-5" = .4,
    "1-2" = .4,
    "1+" = .6)

label_map <- 
list("GERMLINE" = "Germline drivers", 
     "TSG" = "TSGs",
     "ONCO" = "Oncogenes",
     "Overall" = "Somatic drivers", 
     "Actionable" = "Actionable",
     "Best Treatment Option" = "Best treatment option")
mapper <- function(i){if(i %in% names(label_map)){ label_map[[i]]} else { i }}

be_nice <- function(pct){
 pct = 100*round(pct,2)
 if(pct >= 10){paste0(pct, "%")} 
 else if( pct < 4 ){paste0(" ")} 
 else{paste0("  ", pct, "%")}
}

ready <-
ready %>% 
 drop_na(cohort_clean) %>% 
 mu(tot_summary = factor(tot_summary, levels = names(alpha_map))) %>% 
 ar(cohort_clean, label, desc(tot_summary)) %>% 
 rw() %>% 
 mu(label2 = 
    factor(label_map[[as.character(label)]], 
    levels = c("Germline drivers", "Somatic drivers", "Oncogenes", "TSGs", 
                "Actionable", "Best treatment option")),
    pct_label = be_nice(pct)) %>% 
 ug()

plot_events <- function ( plt_cohort = "AYA", ref_cohort = "Non-AYA", j = "Germline") {
    
 ready <- 
 ready %>% 
  fi(cohort %in% c(plt_cohort, ref_cohort)) %>% 
  mu(cohort = factor(cohort, levels =  c(plt_cohort, ref_cohort)))

 if( j == "Germline"){ ready <- ready %>% filter(label == "GERMLINE")}
 else if ( j == "Driver") { ready <- ready %>% filter(label %in% c("Overall", "ONCO", "TSG")) }
 else if ( j == "Actionable") { ready <- ready %>% filter(label %in% c("Actionable", "Best Treatment Option")) }
 go <- ready
    
 ggplot(
  data = go, 
  aes(x =cohort_clean, y=pct, fill=cohort_clean, alpha=tot_summary, label=tot_summary)) +
  geom_bar(stat = "identity",  width = .9, linewidth = 1, color = "black") +
  scale_fill_manual(values = color_map) +
  scale_alpha_manual(values = alpha_map) + 
  facet_grid( rows = vars(label2), switch = "y") +
  geom_text(aes(label = paste0(tot_summary)),
           alpha = 1, size = 3, 
           hjust = .5, vjust = .5, 
           position = position_stack(vjust = 0.5)) +
  geom_text(data = go %>% filter(cohort == ref_cohort), 
           aes(label = pct_label),
           alpha = 1, size = 2.5, 
           hjust = -1.2, vjust = .5, 
           position = position_stack(vjust = 0.5)) + 
  geom_text(data = go %>% filter(cohort == plt_cohort), 
           aes(label = pct_label),
       alpha = 1, size = 2.5, 
       hjust = 2.2, vjust = .3, 
       position = position_stack(vjust = 0.5)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,1), limits = c(0,1.01),
                    expand = c(0, 0))
}

event_plts <- list()
for ( j in c("Germline", "Driver", "Actionable")){
  event_plts[[j]] <- plot_events( plt_cohort = plt_cohort, ref_cohort = ref_cohort, j) 
}

saveRDS(event_plts, paste0( READY_FIG, "1_driver-2_per_sample_events.rds"))
