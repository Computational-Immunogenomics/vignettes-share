source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")

go <- 
fread( paste0( READY_DATA, "1_driver-0_onco_base.txt") ) %>%
 fi( (type == "GERMLINE" & rk <= 10) | (type == "SOMATIC" & rk <= 30))

get_gene_order <- function( df ) {
 rev(
  df %>% 
   select(gene, rk) %>% 
   drop_na(gene) %>% 
   unique() %>% 
   arrange(rk) %>% 
   pull(gene))
}

driver_order <- function( i ) { if( i == "none" ){ 2 } else { 1 } }

make_sample_rows <- function (data, i, genes) {
 sample_genes <-
  data %>% 
    fi(sampleId == i, gene %in% genes) %>% 
    ar(rk) %>% 
    rw() %>% 
    mu( driver_idx = driver_order( event ))
 tmp <- data.frame( i, t(sample_genes %>% pull(driver_idx)) )
 colnames(tmp) <- c("sampleId", rev(genes))
 tmp
}

stack_samples <- function( data ){
 genes <- get_gene_order(data)
 rows <- list() 
 for( i in unique(data$sampleId)){ 
     rows[[i]] <- make_sample_rows( data, i, genes)
 }
 share <- do.call( "rbind", rows )
 rownames(share) <- NULL
 share
}

extract_sample_order <- function( data ){
 data %>% 
  drop_na(sampleId) %>% 
  column_to_rownames("sampleId") %>% 
  arrange(across(everything())) %>% 
  rownames_to_column("sampleId") %>% 
  pull(sampleId)
}

get_sample_order <- function( data ){
 extract_sample_order( stack_samples( data ) )
}

order_oncoplot <- function( data ){
 gene_order <- get_gene_order( data )
 sample_order <- get_sample_order( data )
 data$gene <- factor(data$gene, levels = gene_order)
 data$sampleId <- factor(data$sampleId, levels = sample_order)
 data
}

oncogene_ready <- list()

cohort <- go %>% filter( cohort == plt_cohort )
germline <- cohort %>% filter(type == "GERMLINE")
somatic <- cohort %>% filter(type == "SOMATIC")

if( nrow(germline) == 0){
 oncogene_ready[['germline']] <- NA  
} else {
 base_germline <- germline %>% gb(sampleId,gene) %>% slice_head(n=1) %>% ug()
 oncogene_ready[['germline']] <- order_oncoplot(base_germline)
} 

base_somatic <- somatic %>% gb( sampleId, gene) %>% slice_head(n=1) %>% ug()
system.time(oncogene_ready[['somatic']] <- order_oncoplot(base_somatic))

saveRDS(oncogene_ready, paste0( READY_DATA, "1_driver-2_onco_ready.rds"))
