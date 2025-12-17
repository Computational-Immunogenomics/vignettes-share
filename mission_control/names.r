namer <- function(i, map){
 if( i %in% names(map)){map[[i]]}
 else( i )
}

bar_names <- list(
    "tmbStatus" = "TMB High\n",
    "diploidProportion" = "Diploid\n Proportion",
    "msStatus" = "MSI High\n",
    "purity" = "Purity",
    "wholeGenomeDuplication" = "WGD\n",
    "hrStatus" = "HRD\n",
    "gie_hla" = "GIE HLA",
    "gie" = "GIE\nOverall",
    "clonal" = "Clonal",
    "viral_insertion" = "Viral\nInsertions"
)

variant_names <- 
list(
  "INDEL" = "INDEL", 
  "MNP" = "MNP", 
  "SNP" = "SNV", 
  "svTmb" = "Structural"
)


trt_names <- 
list("chemotherapy" = "Chemo", 
     "multiple therapy" = "Multiple", 
     "targeted therapy" = "Targeted", 
     "hormonal therapy" = "Hormonal", 
     "immunotherapy" = "Immuno", 
     "radio" = "Radio")

trt_namer <- function(i) {
  if( is.na(i) | i == "NULL" | i == "Unknown"){ "None or NA"}
  else if( i %in% names(trt_maps)) {trt_maps[[i]]}
  else { "Other"}
}