source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")

ids <- get_query_data("execute_sql_on_prod 'select sampleId from hmfpatients.datarequest_all'") %>% pu(sampleId)

go_get_files <- function(i) {
 fread( 
  paste0("/data/datasets/", i, "/sigs/", i , ".sig.snv_counts.csv")
 )
}

clean_the_dish <- function(i){
 tmp <- strsplit(i, "_")[[1]]
 start <- substring(tmp[2],1,1)
 mid <- paste0("[",tmp[1], "]")
 end <- substring(tmp[2],3,3)
 paste0(start, mid, end)  
}
clean_the_kitchen <- function( l ) unlist(lapply(l, clean_the_dish))

get_ready <- function(i){
 base <- go_get_files(i)
 base$BucketName <- clean_the_kitchen(base$BucketName)
 data.frame(t(base %>% column_to_rownames("BucketName") ), check.names = FALSE)
}

tris <- data.frame()
system.time(
for( i in ids){
 print(i)
 flush.console()
 tris <- rbind(tris, get_ready(i))
})

fwrite( tris %>% rownames_to_column("sampleId"), file = paste0( READY_DATA, "sigs/3_signatures-0_collect_tris.txt") )
