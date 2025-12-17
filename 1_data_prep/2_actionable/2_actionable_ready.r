source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source("../../mission_control/actionable_rules.r")

go <- fread( paste0( READY_DATA, "2_actionable-1_actionable_base.txt") ) %>% fi( rk <= 27)

get_event_order <- function( df ) {
 rev(df %>% 
     select(event, rk) %>% 
     unique() %>% 
     arrange(rk) %>% 
     unique() %>%
     pull(event))
}

actionable_order <- function( a ) if( a == "None" ){ 2 } else { 1 } 

get_sample_order_step1 <- function (data, i, events) {
    sample_events <- 
      data %>% 
        filter(sampleId == i, event %in% events) %>% 
        arrange(rk) %>% 
        rowwise() %>% 
        mutate( action_idx = actionable_order(top_level) )
    
    tmp <- data.frame( i, t(sample_events %>% pull(action_idx)) )
    colnames(tmp) <- c("sampleId", rev(events))
    tmp
}

get_sample_order_step2 <- function( data ){
    events <- get_event_order(data)
    rows <- list()
    for( i in unique(data$sampleId)){ 
        rows[[i]] <- get_sample_order_step1( data, i, events)
    }
    share <- do.call( "rbind", rows )
    rownames(share) <- NULL
    share
}

get_sample_order_step3 <- function( data ){
    data %>% 
      drop_na(sampleId) %>% 
      column_to_rownames("sampleId") %>% 
      arrange(across(everything())) %>% 
      rownames_to_column("sampleId") %>% 
      pull(sampleId)
}

get_sample_order <- function( data ){
    a <- get_sample_order_step2( data )
    get_sample_order_step3( a )
}

order_oncoplot <- function( data ){
    event_order <- get_event_order( data )
    sample_order <- get_sample_order( data )
    data$event <- factor(data$event, levels = event_order)
    data$sampleId <- factor(data$sampleId, levels = sample_order)
    data$top_level <- factor(data$top_level, levels = names(level_map))
    data
}

system.time(
ready_oncoplot <- order_oncoplot(go %>% filter(cohort == plt_cohort))
)

saveRDS(ready_oncoplot, paste0( READY_DATA, "2_actionable-2_actionable_ready.rds"))
