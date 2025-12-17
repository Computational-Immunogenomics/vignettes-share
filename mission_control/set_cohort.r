shh(library(tidyverse))
shh(library(data.table))
source("/data/repos/cancer-vignettes-dev/mission_control/map.r")

cohorts <- fread(paste0(READY_DATA, "00_cohorts.txt")) 
tmp <- cohorts %>% se(cohort, ref) %>% unique()
plt_cohort <- tmp %>% fi(!ref) %>% pu(cohort)
ref_cohort <- tmp %>% fi(ref) %>% pu(cohort)
vignette_save_name <- clean_filename(cohorts$name[1])
