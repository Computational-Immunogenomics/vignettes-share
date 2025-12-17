library(RColorBrewer)
library("wesanderson", lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3/")
pal <- wes_palette("Zissou1", 7, type = "continuous")

## MOLECULAR PANEL
hartwig_red <- "#e52f28"

### Simple bar plot and density plots
fills <- c("plt" = "#304b99", "ref" = "#878787" )

### Signatures
mbs_colors <- 
c("SBS" = "#bcbddc", 
  "MBS" = "#fcaf17", 
  "INDEL" = "#5e3c99")

sv_colors <- 
c("COMPLEX" = "#fef0d9", 
  "DEL SIMPLE" = "#fdcc8a", 
  "DUP SIMPLE" = "#fc8d59", 
  "LINE INSERT" = "#e34a33", 
  "OTHER" = "#e52f28")

sig_colors <- 
c("Unknown" = "#fcfbfc",
 "DNA damage repair" = "#304b99", 
 "APOBEC" = "#2299FF", 
 "Ageing" = "#92c7ea",
 "Chemical exposure" = "#e52f28", 
 "Tobacco" = "#fcaf17", 
 "UV light" = "#fdcc8a")

## ONCOPLOT
### DRIVER PANEL
driver_fill_map <- c(
 "none" = "white",  "GERMLINE" = "#92c7ec", "AMPLIFICATION" = "#e52f28",
 "DELETION" = "#304b99", "MUTATION" = "#fcaf17", "STRUCTURAL_VARIANT" = "#304b99", 
 "SMALL_VARIANT" = "#fcaf17", "FUSION" =  "#2299FF", "VIRUS" = "#ADC397","DISRUPTION" = "#7AABD3")
driver_labels_map <- c("other" = "black", "pan_cancer" = "#878787")

## Actionability Panels
actionable_fill_map <- c("FDA Approved" = "#396FB0", 
	                 "Drug Repurposing" = "#7AABD3",   
	                 "Experimental/Other Guidelines" = "#C3D6E6", 
	                 "Other" = "#deeaff", 
	                 "None"= "white")

events_colors <- c("3+" = "#5e3c99", "2" = "#756bb1", "1" = "#bcbddc", "0" = "white")
