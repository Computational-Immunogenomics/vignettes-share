source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source("../../mission_control/actionable_rules.r")

go <- fread( paste0( READY_DATA, "2_actionable-1_actionable_base.txt") ) %>% filter(event != "None")

prep_labels <- function(plt_cohort = "AYA", ref_cohort = "Non-AYA"){
 base <- go %>% fi(cohort == plt_cohort) 
 ref <- go %>% fi(cohort == ref_cohort) %>% mu(event = ifelse(event %in% base$event, event, NA))
 share <- rbind(base, ref) %>% drop_na(event)
 if(plt_cohort == "Pan-Cancer"){ share <- share %>% filter(rk < 30)}
 share
}

get_label_pcts <- function( base ){
 base %>% 
  mu(cohort_clean = cohort) %>%     
  gb(cohort_clean, event) %>% 
  su(pct = sum(top_level != "None")/n()) %>% 
  ar(desc(pct)) %>% 
  ug() %>% 
  rename(field = cohort_clean, val = pct) %>% 
  mu(val0 = val, val = ifelse((100*val) <= 10,
     as.character(round(100*val, 1)),as.character(round(100*val,0)))) %>%
  complete(field = c(plt_cohort, ref_cohort), event, fill = list(val = "0", val0 = 0)) 
}

get_fisher_highlights <- function(df) {
  p_values <- numeric(nrow(df))
  odds <- numeric(nrow(df))   
  for (i in seq_len(nrow(df))) {
    mat <- matrix(c(df$events_PLT[i], df$events_REF[i], df$non_events_PLT[i], df$non_events_REF[i]),nrow = 2,byrow = FALSE)
    fisher_out <- fisher.test(mat)
    p_values[i] <- fisher_out$p.value
    odds[i] <- fisher_out$estimate 
  }    
  data.frame(event = df$event,pval = p_values,odds = odds) %>% 
    tm(event, field = "Direction",val0 = NA, val = case_when(pval < .1 & odds > 1 ~ "▲",pval < .1 & odds < 1 ~ "▼",TRUE ~ " "))     
}

get_highlights <- function( base ){
 tmp <- 
 base %>% 
  mu(cohort = ifelse(cohort == plt_cohort, "PLT", "REF")) %>% 
  gb(cohort, event) %>% 
  su( tot = n(), events = sum(tolower(top_level) != "none"), na.rm = TRUE) %>% ug() %>%
  drop_na() %>% 
  tm( cohort, event, events, non_events = tot - events) %>%
  pivot_wider(names_from = cohort,values_from = c(events, non_events),names_sep = "_") %>% 
  mu(events_REF = ifelse(is.na(events_REF), events_PLT, events_REF),
     non_events_REF = ifelse(is.na(non_events_REF), non_events_PLT, non_events_REF))
    
 tot_ref = mean(tmp$events_REF + tmp$non_events_REF,na.rm = TRUE)
    
 ready <-
 tmp %>% 
  mu( events_REF = ifelse(is.na(events_REF), 0, events_REF),
      non_events_REF = ifelse(is.na(non_events_REF), tot_ref, non_events_REF))     
    
 get_fisher_highlights(ready)
}

get_labels <- function( cohort = "AYA", ref = "Non-AYA"  ){
 base <- prep_labels( plt_cohort = cohort, ref_cohort = ref)
 labels <- get_label_pcts( base )
 highlights <- get_highlights( base )
 together <- 
 rbind(labels, highlights) %>% 
  mutate(field2 = 
         factor(ifelse(field != "Direction", paste0(field, " (%)"), field), 
         levels = c(paste0(cohort, " (%)"), paste0( ref, " (%)"), "Direction")))
 together
}

store_labels <- get_labels( cohort = plt_cohort, ref = ref_cohort )

saveRDS(store_labels, paste0( READY_DATA, "2_actionable-3_labels.rds"))
