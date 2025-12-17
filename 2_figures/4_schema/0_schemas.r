source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source(paste0(REF_DIR, "reader.r"))
source(paste0(REF_DIR, "colors.r"))

I_DIR <- paste0(REFS_DIR, "schemas/")

I_DIR

list.files(I_DIR)

sizes <- cohorts %>% gb(cohort) %>% su(ct = n()) %>% ar(desc(ct))

annotater <- function( plt_cohort = "AYA", ref_cohort = "Non-AYA" ) {
 cohort_size <- sizes %>% filter(cohort == plt_cohort) %>% pull(ct)
 ref_size <- sizes %>% filter(cohort == ref_cohort) %>% pull(ct)
 cohort_go <- paste0(plt_cohort, "\n", cohort_size, " Patients")
 ref_go <- paste0(ref_cohort, "\n", ref_size, " Patients")
 list("cohort" = cohort_go, "ref" = ref_go)
}

annotations <- annotater( plt_cohort = plt_cohort, ref_cohort = ref_cohort)

location <- cohorts %>% head(1) %>% pu(primaryTumorLocation)

plt <- 
ggit(paste0( I_DIR,location,".png" ))  + 
   annotate("text", x=.17, y=.93, label = annotations$cohort, size =6, color = fills[["plt"]]) +
   annotate("text", x=.81, y=.93, label = annotations$ref, size =6, color = fills[["plt"]])

system.time(saveRDS( plt, paste0( READY_FIG, "4_schema-0_schemas.rds"), compress = FALSE ))
