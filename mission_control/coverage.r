### Clean and Prepare Events ###  
plan_the_event <- function( i ){
    if( grepl("fusion", i)){ "fusion" } 
    else if (grepl("p\\.",i)){ "small variant" } 
    else if (grepl("splice",i)){ "splice site"} 
    else if (grepl("positive",i)) { "viral"} 
    else if (grepl("\\:", i)){ "hla" } 
    else {i}
}
clean_other_event <- function(i){
    if( i != ""){
        c = str_trim(strsplit(i, "\\|")[[1]][2])
        p = strsplit(i, "\\|")[[1]][3]
        if( p %in% c("", "p.?")){ c } else { p }} 
    else { "" }
}
prep_events <- function( d, i = "somatic" ){
  d %>% 
  mutate( event = ifelse( canonicalHgvsProteinImpact %in% c("", "p.?"), 
                          canonicalHgvsCodingImpact, 
                          canonicalHgvsProteinImpact)) %>% 
  rowwise() %>% 
  mutate(other_event = clean_other_event(otherTranscriptEffects)) %>% 
  ungroup() %>%
  mutate(type = i)
  
}

### Apply coverage rules ### 
apply_rules <- function( event, rules = rules, panel = "tso", val ){
    if( tolower(event) %in% rules$events[[panel]]$miss){ FALSE} 
    else if (tolower(event) %in% rules$events[[panel]]$capture) { TRUE } 
    else { grepl("TRUE", val)}
}

### Select genes in panel ###
get_panel_genes <- function( panel = "tso"){
    pos = list( "tso" = 1, "amp" = 2)
    fp <- paste0("/data/tmp/vignettes/util/data/query/", panel, "_bed.txt")
    fread(fp) %>%
    rowwise() %>%
    mutate( gene = strsplit(target, "_")[[1]][pos[[panel]]]) %>% 
    ungroup() %>% 
    pull(gene) %>% 
    unique()
}

### Cleaning the output ### 
short <- function(i) paste0(unique(i), collapse = ",")

leveller <- function( level, onLabel ){
    if(level == "A" & onLabel == 1){ "A On-label"} 
    else if(level == "A" & onLabel == 0){ "A Off-label" } 
    else if (level == "B" & onLabel == 1){ "B On-label" } 
    else if (level == "B" & onLabel == 0){ "B Off-label" } 
    else { NA }
}

big_grouper <- function(i){
    i <- tolower(i)
    sigs <- c("high tumor mutational burden","high tumor mutational load","hr deficiency","microsatellite unstable")
    cn <- c("partial gain", "partial loss", "full gain", "full loss")
    som_variant <- c("splice site", "small variant")
    other <- c("viral", "hla", "fusion", "homozygous disruption")
    
    if( i %in% som_variant){
        "Somatic Variant"
    } else if (i %in% cn){
        "Copy Number"
    } else if (i %in% sigs){
        "Signature"
    } else if (i %in% other){
        "Other"
    } else {
        NA
    }
}

better_names <-
list("high tumor mutational burden" = "TMB High",
     "high tumor mutational load" = "TML High",
     "hr deficiency" = "HRD",
     "microsatellite unstable" = "MSI High",
     "hla" = "HLA",
     "homozygous disruption" = "HMZ Disruption", 
     "full gain" = "Full CN Gain",
     "full loss" = "Full CN Loss",
     "partial gain" = "Partial CN Gain",
     "partial loss" = "Partial CN Loss")

better_namer <- function(i){
    j <- tolower(i)
    if(j %in% names(better_names)){ better_names[[j]]}
    else {str_to_title(j)}
}

panel_maps <- list( "wgs" =      "WGS", 
		            "tso_cov" =  "Comprehensive\n521 gene panel",
                    "amp_cov" =  "Targeted    \n50 gene panel")
