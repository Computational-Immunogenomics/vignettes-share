source("../mission_control/map.r")
source("../mission_control/shortcuts.r")

data_dir <- substring(READY_DATA, 1, nchar(READY_DATA) - 1)
fig_dir <- substring(READY_FIG, 1, nchar(READY_FIG) - 1)

print("Clean directories to make sure old data/figures are not used: ")

remove_files <- list.files(data_dir, full.names = TRUE)
remove_files <- remove_files[file.info(remove_files)$isdir == FALSE]
remove_figs <- list.files(fig_dir, full.names = TRUE)

file.remove(remove_files)
file.remove(remove_figs)

args <- commandArgs(trailing = TRUE)
fn <- args[[1]]
cohort <- args[[2]]

cohorts_read <- readRDS( paste0("/data/tmp/vignettes-dev/util/data/cohorts/", fn) )

cohorts <- cohorts_read$cohorts[[cohort]]

cohort_doids <- 
cohorts %>% 
 lj( cohorts_read$id, by = c("patientId" = "donorId"), relationship = "many-to-many") %>% 
 se( cohort, doid ) %>% 
 unique() %>% 
 gb( cohort ) %>%
 su( all_doid = paste0( unique(doid), collapse = ","))

fwrite( cohorts, file = paste0( READY_DATA, "00_cohorts.txt") )
fwrite( cohort_doids, file = paste0( READY_DATA, "00_cohort_doids.txt") )

print("Run Vignettes for input file and cohort: ")
print(fn)
print(cohort)
