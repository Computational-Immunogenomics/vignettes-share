source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source("../../mission_control/actionable_rules.r")

go <- fread( paste0( READY_DATA, "2_actionable-1_actionable_base.txt") )

grouper <- function(i){
 if(i == 0){ "None" } 
 else if (i < 3){ "1-2" }
 else if (i >= 3) { "3+" }
}

idx_events <- 
go %>% 
 gb(cohort, sampleId) %>% 
 su( tot = sum(top_level != "None")) %>% 
 rw() %>% 
 mu(tot_summary = grouper(tot)) %>% 
 ug()

events_per_sample <-
cohorts %>% 
 lj(idx_events, by = c("cohort", "sampleId")) %>% 
 drop_na(tot_summary) %>% 
 gb(cohort, tot_summary) %>% 
 su(ct = n()) %>% 
 ug() %>% 
 gb(cohort) %>% 
 mu(pct = ct/sum(ct), label = "Actionable")

events_per_sample$cohort <- factor(
    events_per_sample$cohort, 
    levels = events_per_sample %>% 
               filter(tot_summary == "1-2") %>% 
               select(cohort, ct) %>% 
               unique() %>% 
               arrange(desc(ct)) %>% 
               pull(cohort)
)

btos <- 
go %>% 
 lj(idx_events, by = c("cohort", "sampleId")) %>% 
 rw() %>% 
 mu( top_level = name_map[[top_level]]) %>% 
 ug() %>% 
 gb(sampleId, cohort, top_level) %>% 
 su(ct = n()) %>% 
 sp(top_level, ct) %>% 
 replace_na(list("FDA" = 0, "Off" = 0, "Exp" = 0, "Oth" = 0, "None" = 0)) %>% 
 rw() %>% 
 su(hl = highest_level_event(FDA, Off, Exp, Oth)) %>% 
 gb(cohort, hl) %>% 
 su(ct = n()) %>% 
 gb(cohort) %>% 
 mu(pct = ct/sum(ct), tot = sum(ct)) %>% 
 drop_na(cohort) %>% 
 mu( group = "All Samples", 
    hl = factor(hl, levels = rev(unname(unlist(name_map)))))

pan_first <- 
btos %>% 
 select(cohort,tot) %>% 
 unique() %>% 
 arrange(desc(tot)) %>% 
 pull(cohort)

btos_ready <-
btos %>% 
 mu( cohort = factor( cohort, levels = pan_first)) %>% 
 rw() %>% 
 mu(hl2 = strsplit(as.character(hl), "-")[[1]][1]) %>% 
 ug() %>% 
 tm(cohort, tot_summary = hl2, pct, label = "Best Treatment Option")

lets_go <- rbind(events_per_sample %>% select(-ct), btos_ready)

saveRDS(lets_go, paste0( READY_DATA, "2_actionable-4_events.rds"))
