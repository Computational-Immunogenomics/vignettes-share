source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/actionable_rules.r")

go <- query("execute_sql_on_prod 'select * from hmfpatients.protect where reported = 1'")

ready <-
go %>% 
 fi(source == "CKB", grepl("RESPONSIVE", direction), !(level == "C" & onLabel == 0)) %>% 
 mu( source = tolower(source) ) %>% 
 mu( level_label = paste0(level, "_", onLabel) ) %>% 
 mu( event = ifelse( transcript != "NULL", gene, event)) %>% 
 gb( sampleId, event ) %>% 
 su( lds =  paste0(level_label, collapse = ","), .groups = "drop") %>% 
 mu( A_on = grepl("A_1",lds),  A_off = grepl("A_0",lds),  
     B_on = grepl("B_1",lds),  B_off = grepl("B_0",lds),
     C_on = grepl("C_1",lds),  C_off = grepl("C_0",lds)) %>% 
 rw() %>% 
 mu( top_level = evidence_map[[(highest_level( A_on, A_off, B_on, B_off, C_on, C_off))]], ## see actionable_rule
     event = small_names(event)) %>% 
 se( sampleId, event, top_level) %>% 
 ug()

fwrite( ready, file = paste0( READY_DATA, "2_actionable-0_protect.txt") )
