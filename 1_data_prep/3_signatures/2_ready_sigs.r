source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/sig_map.r")
source("../../mission_control/set_cohort.r")

tmb <- query("execute_sql_on_prod 'select sampleId, tmbPerMb from purity'") %>% tm(sampleId, tmb = tmbPerMb )

wts <- fread(paste0( READY_DATA, "sigs/3_signatures-1_compute_sigs.txt"))

wts_go <-
wts %>% 
 ga(sig, pct, -sampleId) %>% 
 rw() %>% 
 mu(gp = sig_map[[sig]]) %>% 
 gb(sampleId, gp) %>% 
 su(pct = sum(pct), .groups = "drop") 

sigs_ready <-
cohorts %>% 
 lj(tmb, by = "sampleId") %>% 
 lj(wts_go, by = "sampleId", relationship = "many-to-many") %>%
 mu(tmb = ifelse(tmb > 30, 30, tmb)) %>% 
 mu(cont = pct * tmb) %>% 
 gb(cohort) %>% 
 mu( tot = sum(cont, na.rm = TRUE)) %>% 
 ug() %>% 
 gb( cohort, gp) %>% 
 su( pct = sum(cont)/mean(tot)) %>% 
 ug() %>% 
 drop_na(gp) %>%
 gb(cohort) %>% 
 mu(spot = row_number(pct)) %>% 
 ug()

saveRDS(sigs_ready, file = paste0( READY_DATA, "3_signatures-2_ready_sigs.rds") )
