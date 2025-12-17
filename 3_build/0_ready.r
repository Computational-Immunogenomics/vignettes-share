source("../mission_control/map.r")
source("../mission_control/shortcuts.r")
source("../mission_control/set_cohort.r")
source(paste0(REF_DIR, "theme.r"))
source(paste0(REF_DIR, "colors.r"))
source(paste0(REF_DIR, "reader.r"))

shh(library(png))
shh(library(grid, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3"))
shh(library(cowplot, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3"))

setwd(READY_FIG)

ready <- list()

ready$doids <- 
fread(paste0( READY_DATA, "00_cohort_doids.txt")) %>% 
 fi(cohort == plt_cohort) %>% 
 pu(all_doid)

ready$time_stamp <- read.table(paste0(REF_REPO, "queries/time_stamp.txt"))$V1

clinical <- readRDS('0_basic-0_clinical.rds')
ready$avail <- clinical$avail + cohort_theme + title_set
ready$trt  <- clinical$trt + cohort_theme + title_set

bars_simple <- readRDS("0_basic-1_tumor_characteristics.rds")

summary_theme <- 
list(base_theme, 
     vamos, 
     no_x_title, 
     no_y, 
     remove_y, 
     theme(legend.justification='left',legend.direction='horizontal',legend.spacing.x = unit(.6, 'cm'), 
           legend.text = element_text(size = 15), legend.key.size = unit(.4, "cm"), legend.position=c(.5,.9)))

ready$summary <- bars_simple + summary_theme

mirrors <- readRDS("0_basic-2_mirrors.rds")

left <- list(base_theme, vamos, no_y_label, y_title_vertical, remove_y, title_set)
mid <- list(base_theme, vamos, no_y_label, no_y, remove_y, title_set)
right <- list(base_theme, vamos, no_y_label, y_title_vertical, remove_y, title_set)

ready$ploidy <- mirrors$ploidy + left
ready$loh <- mirrors$loh + mid
ready$scna <- mirrors$scna + left
ready$aneuploidy <- mirrors$aneuploidy + mid
ready$sv <- mirrors$svTmb + right
ready$clonal <- mirrors$clonal + left + ggtitle("Clonal mutation fraction")
ready$tmb <- mirrors$tmb  + mid
ready$msi <- mirrors$msi + mid
ready$diploid <- mirrors$diploid + mid
ready$purity <- mirrors$purity + left
ready$telomere <- mirrors$telomere + right
ready$cdr3 <- mirrors$cdr3 + left

somatic_variant <- readRDS("0_basic-3_variant_types.rds")

somatic_variant_theme <- 
list(base_theme, 
     vamos, 
     theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))),
     set_title(size = 14, face = "plain", color = "black"),
     ylab("Number of variants"), 
     ggtitle("Variant types"))

ready$somatic <- somatic_variant + somatic_variant_theme

sigs <- readRDS("0_basic-4_signatures.rds")

sigs_theme <-
list(
 ggtitle("Processes Underlying Mutations"),
 theme_void(),
 theme(plot.margin = margin(t = 0,r = -2,l = -2,b = -2, "cm")),
 title_set
)

ready$sigs <- sigs + sigs_theme

onco_driver <- readRDS("1_driver-0_oncodriver.rds")

if(!is.list(onco_driver$germline)){
 onco_driver$germline <- ggplot() + theme_void() + 
 geom_text(aes(x=.4, y=.5, label="No Events"), size = 7)
}

ready$onco_germline <- onco_driver$germline + onco_theme + ggtitle("Top potential germline drivers") + title_set
ready$onco_somatic <- onco_driver$somatic + onco_theme  + ggtitle("Top cancer drivers") + title_set

driver_labels <- readRDS("1_driver-1_driver_labels.rds")

labels_theme <- list(label_theme, dodge_x, ggtitle("Summary"), title_set)
labels_void_theme <- list(labels_theme, theme_void(),geom_text(aes(x=.4, y=.5, label="No Events"), size = 7))

if(!is.list(driver_labels$GERMLINE)){
 ready$driver_labels_germline <- ggplot() + label_void_theme
} else {
 ready$driver_labels_germline <- driver_labels$GERMLINE + labels_theme
}

ready$driver_labels_somatic <- driver_labels$SOMATIC + labels_theme

events_samps <- readRDS("1_driver-2_per_sample_events.rds")

big_strips <- theme(strip.text.y = element_text(size =12, colour = "black", angle = 90))
expand <- scale_x_discrete(expand = expansion(mult = c(1.2,1.2)), guide = guide_axis(n.dodge=3))

events_theme <- list(samps_theme, dodge_x, no_grid, ggtitle("# Events"), title_set, big_strips, expand)

ready$events_driver <- events_samps$Driver + events_theme
ready$events_germ <- events_samps$Germline + events_theme + x_text
ready$events_act <- events_samps$Actionable + events_theme

ready$onco_actionable <- 
readRDS("2_actionable-0_onco.rds") + onco_theme + ggtitle("Top potential actionable events") + title_set

ready$actionable_labels <- readRDS("2_actionable-1_labels.rds") + labels_theme

ready$circos <- ggit("3_circos-1_run.png") + ggtitle( "Copy Number Alteration Profile" )

system.time(ready$schematic <- readRDS( "4_schema-0_schemas.rds") )

ready$coverage <- readRDS("5_panel_coverage-O_coverage.rds")

system.time(saveRDS(ready, paste0( READY_FIG, "READY_FIGURES.rds"), compress = FALSE))
