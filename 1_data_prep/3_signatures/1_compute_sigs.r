source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
library(deconstructSigs, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.1")

tris <- fread(paste0( READY_DATA, "sigs/3_signatures-0_collect_tris.txt")) 

if( ! file.exists(paste0(READY_DATA,"cosmic_oct_2023.txt"))){
    url <- "https://cog.sanger.ac.uk/cosmic-signatures-production/documents/COSMIC_v3.4_SBS_GRCh38.txt"
    file_name <- "cosmic_oct_2023.txt"
    file_path <- paste0(REFS_DIR, "sigs/")
    download.file(url, paste(file_path, file_name, sep = ""), mode = "wb")
}

cosmic.oct2023 <- 
data.frame(t(fread(paste0(REFS_DIR,"sigs/cosmic_oct_2023.txt")) %>% 
 column_to_rownames("Type")), check.names = FALSE)

compute_weights <- function(i){
    whichSignatures(tumor.ref = ready,
                    sample.id = i, 
                    signatures.ref = cosmic.oct2023,
                    signature.cutoff = 0,
                    contexts.needed = TRUE)$weights   
}

ready <- tris %>% column_to_rownames( "sampleId" )

wts <- list()

ct <- 1
system.time(
for( i in rownames(ready)){
  print(ct); ct = ct + 1
  flush.console()
  if( !(i %in% names(wts))){   
   wts[[i]] <- compute_weights(i)
  }
})

wts_ready <- 
do.call( "rbind", wts ) %>% 
 rownames_to_column("sampleId") %>% 
 bind_rows(wts_already)

fwrite(wts_ready, paste0( READY_DATA, "sigs/3_signatures-1_compute_sigs.txt")) 
