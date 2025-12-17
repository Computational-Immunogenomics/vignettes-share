source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")

go <- fread( paste0( READY_DATA, "1_driver-0_onco_base.txt"))

pathways <- fread( paste0( REFS_DIR, "pathways/pathways.csv"))

top_map <- list( "GERMLINE" = 10, "SOMATIC" = 30)

get_base <- function( plt_cohort = "AYA", j = "SOMATIC" ){
 go %>% 
  drop_na() %>% 
  fi(cohort == plt_cohort, type == j, rk <= top_map[[j]])
}

prep_labels <- function( plt_cohort = "AYA", ref_cohort = "Non-AYA", j = "SOMATIC"){
 base <- get_base( plt_cohort, j )
 ref <- go %>% fi(cohort == ref_cohort, type == j, gene %in% base$gene)
 together <- rbind(base, ref)
 together
}

get_label_pcts <- function( base ){
 base %>% 
  gb(cohort, gene) %>% 
  su(pct = sum(event != "none")/n()) %>% 
  ar(desc(pct)) %>%   
  ug() %>% 
  rename(field = cohort, val = pct) %>% 
  mu(val0 = val, 
     val = ifelse( (100*val) <= 10, 
                   as.character(round(100*val, 1)), 
                   as.character(round(100*val,0)))) %>% 
 complete(field = c(plt_cohort, ref_cohort), gene, fill = list(val = "0", val0 = 0)) 
}

get_label_category <- function( base ){
 base %>% 
  fi(category != "none") %>% 
  gb(gene, category) %>% 
  su(ct = n()) %>% 
  gb(gene, category) %>% 
  mu(rk = row_number(desc(ct))) %>% 
  fi( rk == 1) %>% select(-rk, -ct) %>% 
  mu(field = "Type", val0 = NA) %>% 
  mu( val = ifelse( category == "ONCO", "ONC", "TSG")) %>% 
  ug() %>% select(-category)
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
  data.frame(gene = df$gene,pval = p_values,odds = odds) %>% 
    tm(gene, 
       field = "Direction",
       val0 = NA, 
       val = case_when(pval <= .1 & odds > 1 ~ "▲", pval <= .1 & odds < 1 ~ "▼",TRUE ~ " "))     
}

get_highlights <- function( base ){
    
  tmp <- 
   base %>% 
    mu(cohort = ifelse(cohort == plt_cohort, "PLT", "REF")) %>% 
    gb(cohort, gene) %>% 
    su( tot = n(), events = sum(event != "none")) %>% ug()
    
 tmp2 <- 
  tmp %>%
   tm( cohort, gene, events, non_events = tot - events) %>%
   complete(cohort = c("PLT", "REF"), gene, fill = list(events = 0, non_events = tmp$tot[1])) %>% 
   pivot_wider(names_from = cohort,values_from = c(events, non_events),names_sep = "_")
 
 tot_ref = mean(tmp2$events_REF + tmp2$non_events_REF,na.rm = TRUE)
    
 ready <-
 tmp2 %>% 
  mu( events_REF = ifelse(is.na(events_REF), 0, events_REF),
      non_events_REF = ifelse(is.na(non_events_REF), tot_ref, non_events_REF))   
    
 get_fisher_highlights(ready)
}

add_pathway <- function( base ){
 data.frame(
  field = "Pathway", 
  gene = base %>% pu(gene) %>% unique(), 
  val0 = NA) %>% lj(pathways, by = "gene")
}

get_labels <- function( plt_cohort = "AYA", ref_cohort = "Non-AYA", j = "SOMATIC" ){
 base <- prep_labels( plt_cohort, ref_cohort, j )
 categories <- get_label_category( base )
 pcts <- get_label_pcts( base )
 highlights <- get_highlights(base) 
 pathways <- add_pathway(base)

 rbind(pcts, categories, highlights, pathways) %>% 
 mu(field2 = 
    factor(ifelse(!field %in% c("Pathway", "Type","Direction"),paste0(field," (%)"),field), 
    levels = c("Pathway", "Type", paste0(plt_cohort," (%)"), paste0(ref_cohort," (%)"),"Direction"))) 
}

store_labels <- list()
for( i in c("SOMATIC", "GERMLINE")){
 if( nrow(go %>% filter(type == i)) > 0 ){
  store_labels[[i]] <- get_labels( plt_cohort = plt_cohort, ref_cohort = ref_cohort, i )
 } else { 
  store_labels[[i]] <- NA}
}

saveRDS(store_labels, paste0( READY_DATA, "1_driver-3_driver_labels.rds"))
