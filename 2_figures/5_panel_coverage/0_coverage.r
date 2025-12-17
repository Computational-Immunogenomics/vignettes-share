source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source("../../mission_control/actionable_rules.r")
source("../../mission_control/colors.r")
source("../../mission_control/coverage.r")

shh(library(gridExtra))
shh(library(ggforce, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3"))
shh(library(tidytext, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3"))
shh(library(cowplot, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3"))
shh(library(ggpubr, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.1"))

cts <- fread(paste0(READY_DATA, "5_panel_coverage-1_event_counts.txt"))

ready <- 
cts %>% 
  group_by(cohort, panel, evidence, tot) %>% 
  summarise( tot_events = sum(ct), avg_events = mean(ct) ) %>% 
  mutate(evidence = factor(evidence, levels = rev(names(level_map)))) %>% 
  mutate(label = ifelse(avg_events > .15, as.character(signif(avg_events,2)), "")) %>% 
  mutate( panel = factor(panel, levels = rev(unname(unlist((panel_maps))))))

evidence_map <- actionable_fill_map

stuff <-   
list(
  geom_bar(stat = "identity", color = "black", alpha = .7),
  theme_minimal(base_size = 13),
  theme(legend.position="bottom"),
  theme(axis.title.y=element_blank(), plot.title = element_text(hjust = .5, size = 14)),
  guides(fill = guide_legend(reverse=T)),
  theme(plot.margin = margin(l = .5,r = 0, b = .5,t = .5, "cm")),
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

ready <- 
cts %>% 
  group_by(cohort, panel, evidence, tot) %>% 
  summarise( tot_events = sum(ct), avg_events = mean(ct) ) %>% 
  mutate(evidence = factor(evidence, levels = rev(names(level_map)))) %>% 
  mutate(label = ifelse(avg_events > .2, as.character(signif(avg_events,2)), "")) %>%
  mutate(panel = factor(panel, levels = rev(unname(unlist((panel_maps))))))

plot_average <- function( i = "Pan-Cancer"){
  
  base <- ready %>% filter(cohort == i)
    
  go <- 
    base %>% 
    group_by(cohort, panel, tot) %>% 
    summarise( avg_events = sum(avg_events) )  
  
  plt <- base %>% 
    ggplot(aes(y = panel, x = avg_events, fill = evidence)) + 
    stuff + 
    scale_fill_manual(values = unlist(evidence_map)) + 
    geom_text( aes(label = label, x = avg_events), size = 4, position = position_stack(vjust = 0.5)) +
    geom_text( inherit.aes = FALSE, 
               data = go, aes(label = round(avg_events,1), 
                  x = avg_events, y = panel, hjust = -.3), stat = "identity", size = 5) +
    labs(fill = NULL, alpha = NULL) + 
    xlab("Average number of actionable events per patient") + 
    ggtitle("Average number of identified potentially actionable events\nidentified by test strategy") + 
    scale_x_continuous(expand=c(0,0), limits = c(0, max(go$avg_events, na.rm = TRUE)*1.1)) +
    theme(legend.position="none")
   
  plt  
}

avgs <- plot_average(plt_cohort)

best_treatment <- 
cts %>% 
  group_by( cohort, sampleId, panel ) %>% 
  mutate( event = sum(ct)> 0) %>% 
  mutate( evidence = factor(ifelse(!event, "None", evidence),
          levels = names(level_map)), 
          ct = ifelse(evidence == "None", 1, ct)) %>% 
  filter(ct > 0) %>% 
  group_by(cohort, sampleId, panel) %>% 
  mutate(rk = row_number(evidence)) %>% 
  filter(rk == 1) %>% 
  group_by(cohort, panel, evidence) %>% 
  summarise(ct = n()) %>% 
  ungroup() %>% 
  mutate(evidence = factor(evidence, level = rev(names(level_map)))) %>%
  group_by(cohort, panel) %>% 
  mutate(pct = ct/sum(ct), label = ifelse(pct > .03, as.character(ct), "")) %>% 
  mutate(panel = factor(panel, levels = rev(unname(unlist((panel_maps))))))

plot_best_treatment <- function( i = "Pan-Cancer"){
  best_treatment %>% 
    filter(cohort == i) %>% 
    ggplot(aes(y = panel, x = ct, fill = evidence)) +
    stuff + 
    scale_fill_manual(values = unlist(evidence_map)) + 
    geom_text(inherit.aes = FALSE, aes(label = label, y = panel, x = ct, fill = evidence), size = 3.5, position = position_stack(vjust = 0.5)) + 
    xlab("# of patients in Hartwig database") + 
    ggtitle("Highest evidence level treatment option\nidentified by test strategy") + 
    labs(fill = NULL, alpha = NULL) + 
    scale_x_continuous(expand=c(0,0)) 
}

btos <- plot_best_treatment(plt_cohort)

base <- fread(paste0(READY_DATA, "5_panel_coverage-0_prepare.txt"))
ref <- fread(file = paste0( READY_DATA, "00_cohorts.txt") ) 

go <- ref %>% left_join(base, by = "sampleId", relationship = "many-to-many")

get_ready <- 
go %>% 
  group_by(cohort, panel, evidence, event_group) %>% 
  summarise(total_events = n(), missed_events = sum(!coverage), 
            missed_pct = paste0(round(100*missed_events/total_events), "%")) %>% 
  group_by(cohort, panel, evidence) %>% 
  mutate(rk = row_number(desc(missed_events))) %>% 
  mutate(evidence = factor(evidence, levels = names(evidence_map))) %>% 
  drop_na(event_group) %>% 
  arrange(panel, evidence, rk) %>% 
  group_by(panel) %>% 
  mutate(overall_rk = row_number(desc(missed_events)))

filler <- function(i = "TSO500", j = "AYA", k = "FDA Approved"){
  data.frame( event_group = unique(get_ready$event_group), panel = i, cohort = j, evidence = k, tot_event = 0)
}

filler_df <- data.frame()
for( i in unique(get_ready$panel)){
    for( j in unique(get_ready$cohort)){
        for( k in unique(get_ready$evidence)){
            filler_df <- rbind(filler_df, filler(i, j, k))
        }
    }
}

ready <- 
filler_df %>% 
  left_join(get_ready, by = c("event_group", "panel", "cohort", "evidence")) %>% 
  mutate( total_events = ifelse(is.na(total_events), 0, total_events), 
          missed_events = ifelse(is.na(missed_events), 0, missed_events)) %>% 
  group_by(cohort, panel, evidence) %>% 
  mutate(rk = row_number(desc(missed_events))) %>% 
  mutate(evidence = factor(evidence, levels = names(evidence_map))) 

maker <- function( i = "TSO500", j= "Missed events by TSO versus WGS" ){  
  dat <- 
  ready %>% 
    filter(panel == i, cohort == plt_cohort, tolower(evidence) != "none" ) %>% 
    mutate(evidence = factor(evidence, levels = names(level_map))) %>% 
    filter(rk < 6) %>% 
    rowwise() %>% 
    mutate(event_group = ifelse(missed_events == 0, blank_names[[as.character(rk)]], as.character(event_group))) %>% 
    ungroup() %>% 
    mutate(label = ifelse(missed_events == 0, "", as.character(missed_events))) %>% 
    mutate(event_group = tidytext::reorder_within(event_group, desc(rk), evidence))
  
  dat %>% 
    ggplot(aes(y = event_group, x = missed_events, fill = evidence)) + 
    stuff + 
    theme(legend.position = "none") + 
    scale_fill_manual(values = unlist(evidence_map)) + 
    facet_wrap(~evidence, scales = "free_y", ncol = 2) + 
    geom_text(aes(label =label), hjust = -.2) + 
    scale_y_reordered() +
    scale_x_continuous(expand=c(0,0), limits = c(0, max(dat$missed_events, na.rm = TRUE)*1.3)) + 
    ggtitle(j) + 
    xlab("Number of Missed Events")
}

blank_names <- list("1" = "", "2" = " ", "3" = "  ", "4" = "   ", "5" = "    ")

coverage_tso <- maker(i = panel_maps[["tso_cov"]], j = "Top missed events by Comprehensive Panel vs WGS  ")
coverage_amp <- maker(i = panel_maps[["amp_cov"]], j = "Top missed events by Targeted Panel vs WGS  ")

coverage <- list()
coverage[['bto']] <- btos
coverage[['avg']] <- avgs
coverage[['tso']] <- coverage_tso 
coverage[['amp']] <- coverage_amp

saveRDS( coverage, paste0( READY_FIG, "5_panel_coverage-O_coverage.rds"))
