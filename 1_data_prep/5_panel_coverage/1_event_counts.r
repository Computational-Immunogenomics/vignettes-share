source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source("../../mission_control/actionable_rules.r")
source("../../mission_control/coverage.r")

clean <- fread(file = paste0(READY_DATA, "5_panel_coverage-0_prepare.txt"))
ref <- fread(file = paste0( READY_DATA, "00_cohorts.txt") ) 

ref_ready <- 
rbind(ref %>% mutate(evidence = "FDA Approved"),
      ref %>% mutate(evidence = "Drug Repurposing" ),
      ref %>% mutate(evidence = "Experimental/Other Guidelines"),
      ref %>% mutate(evidence = "Other"))

ref_go <- 
rbind(ref_ready %>% mutate(panel = panel_maps[["wgs"]]), 
      ref_ready %>% mutate(panel = panel_maps[["tso_cov"]]),
      ref_ready %>% mutate(panel = panel_maps[["amp_cov"]]))

ready <- 
ref_go %>% 
  select(sampleId, cohort, evidence, panel) %>% 
  left_join(clean, by = c("sampleId", "evidence", "panel"), relationship = "many-to-many") %>% 
  select(sampleId, cohort, event_group, evidence, panel, coverage) %>% 
  mutate(coverage = ifelse(is.na(coverage), FALSE, coverage),
         event_group = ifelse(is.na(event_group), "None", event_group))

event_cts <- 
ready %>% 
  group_by(sampleId, cohort, panel, evidence, .drop = FALSE) %>%
  summarise(ct = sum(coverage)) %>% 
  ungroup() %>% 
  mutate( gp = factor(cut(ct, c(-1, 0,1, 1000), c("0", "2+", "1")), levels = c("0", "2+", "1")), 
          gp2 = factor(cut(ct, c(-1, 0,1000), c("0", "1+")), levels = c("0", "1+")), 
          panel = factor(panel, levels = rev(unname(unlist((panel_maps))))),
          evidence = factor(evidence, levels = names(level_map))) %>% 
  group_by(cohort, panel) %>% 
  mutate( tot = n_distinct(sampleId)) %>% 
  ungroup()

fwrite(event_cts, file = paste0(READY_DATA, "5_panel_coverage-1_event_counts.txt"))
