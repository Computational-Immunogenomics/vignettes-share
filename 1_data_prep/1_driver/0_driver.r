source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")

go <- get_query_data("execute_sql_on_prod 'select * from hmfpatients.driverCatalog'")
fusions <- get_query_data("execute_sql_on_prod 'select * from hmfpatients.svFusion where reported = 1'")
virus <- get_query_data("execute_sql_on_prod 'select * from hmfpatients.virusAnnotation where reported = 1'")

grouper <- function(i){
 if(grepl("AMP",i) ){ "AMPLIFICATION"} 
 else if ( grepl("GERMLINE",i) ){ i } 
 else if ( i %in% c("DEL", "HOM_DEL_DISRUPTION", "HOM_DUP_DISRUPTION" )){ "DELETION" } 
 else if ( i %in% c("DISRUPTION" )){ "DISRUPTION" }     
 else { "MUTATION" }
}

ready <-
go %>% 
 se(sampleId, gene, category, driver, likelihoodMethod, driverLikelihood, biallelic) %>% 
 mu( driverLikelihood = ifelse(driver == "DISRUPTION", 1, driverLikelihood)) %>% 
 fi( driverLikelihood > .8  ) %>% 
 rw() %>% 
 mu(driver_simple = grouper(driver),
    type = ifelse(grepl("GERMLINE", driver), "GERMLINE", "SOMATIC"),
    event = ifelse(type == "GERMLINE", strsplit(driver_simple, "_")[[1]][2], driver_simple)) %>% 
 ug()

ready_fusion <-
fusions %>% 
 tm( sampleId, 
     gene = name, 
     category = "ONCO", 
     driver = "FUSION", 
     likelihoodMethod = reportedType,
     driverLikelihood = 1,
     biallelic = 0, 
     driver_simple = "FUSION",
     type = "SOMATIC",
     event = "FUSION")

ready_virus <-
virus %>% 
 tm( sampleId, 
     gene = interpretation, 
     category = "ONCO", 
     driver = interpretation, 
     likelihoodMethod = "Viral",
     driverLikelihood = 1,
     biallelic = 0, 
     driver_simple = "VIRUS",
     type = "SOMATIC",
     event = "VIRUS")

vamos <- rbind(ready, ready_fusion, ready_virus)

fwrite( vamos, file = paste0( READY_DATA, "1_driver-0_driver.txt") )
