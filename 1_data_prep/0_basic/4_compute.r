source("../../mission_control/shortcuts.r")
source("../../mission_control/map.r")
source("../../mission_control/names.r")
source("../../mission_control/set_cohort.r")

cohorts <- fread(paste0(READY_DATA,"00_cohorts.txt"))
ready <- fread(paste0( READY_DATA, "0_basic-3_prep_ready.txt"))

base <- cohorts %>% lj(ready, by = "sampleId")

fields <- c("Response", "Treatment", "Pretreatment", "Biopsy site", "Age, Sex", "RNASeq", "WGS")

clin_avail <- 
base %>% 
 se(sampleId, cohort, all_of(fields)) %>% 
 ga(field, val, -sampleId, -cohort) %>% 
 gb(cohort, field) %>% 
 su(avail = sum(val), .groups = "drop") %>% 
 ug() %>% 
 mu(field = factor(field, levels = fields), 
    extra = "Number of patients with data")

trts <- c("None or NA", "Other", "Radio", "Immuno", "Hormonal", "Targeted", "Multiple", "Chemo")

trts_ready <- 
base %>% 
 se(sampleId, cohort, all_of(trts)) %>% 
 ga(field, val, -sampleId, -cohort) %>% 
 gb(cohort, field) %>% 
 su(counts = sum(!is.na(val)), response = sum(val, na.rm = TRUE), pct = response/counts, .groups = "drop") %>% 
 ga(type, ct, -cohort, -field, -pct) %>% 
 mu(label = ifelse(type == "counts", ct, paste0(ct, " (", round(100*pct), "%)")), 
    label2 = ifelse(type == "counts", "Post-biopsy treatment", "Responders")) %>% 
 mu(field = factor(field, levels = trts))

tc_names <- c()
for( i in names(bar_names)){tc_names <- c(tc_names, bar_names[[i]])}

tcs <- 
base %>%  
 mutate_all(~ replace_na(., 0)) %>% 
 se(sampleId, cohort, all_of(names(bar_names))) %>% 
 ga(feature, val, -sampleId, -cohort) %>% 
 gb(cohort, feature) %>% 
 su(val = mean(val, na.rm = TRUE), .groups = "drop") %>% 
 rw() %>% 
 mu(name = factor(namer(feature, bar_names), levels = tc_names)) %>% 
 ug() %>%
 mu(cohort = factor(cohort, levels = c( plt_cohort, ref_cohort )))

vts <- 
base %>% 
 tm(sampleId, cohort, SNV = SNP, INDEL, MNV = MNP, Structural = svTmb) %>% 
 ga(type, ct, -sampleId, -cohort ) %>% 
 gb(cohort, type) %>% 
 mu(rk = row_number(ct), tot = n(), quantile = rk/tot) %>% 
 ar(cohort, type, rk) %>% 
 mu(type = factor(type, levels = c("SNV", "INDEL", "MNV", "Structural")))  %>% 
 mu(medians = median(ct, na.rm = TRUE)) %>% 
 ug()

mirrors <- 
base %>% 
 tm(sampleId, cohort, tmbPerMb, msIndelsPerMb, ploidy, 
    svTmb, clonal, diploidProportion, purity, loh_sample, cna_sample,
    chr_arm_deviations = chr_arm_deviations_ploidy, telomere_ratio, cdr3_seqs)

saveRDS( list("clin_avail" = clin_avail, 
             "trts_ready" = trts_ready, 
             "tcs" = tcs, 
             "vts" = vts, 
             "mirrors" = mirrors),
        paste0( READY_DATA, "0_basic-4_compute.rds") )
