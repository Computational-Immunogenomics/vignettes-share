source("../mission_control/map.r")
source("../mission_control/shortcuts.r")
source(paste0(REF_DIR, "theme.r"))
source(paste0(REF_DIR, "colors.r"))

shh(library(gridExtra))
shh(library(ggpubr, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.1"))
shh(library(cowplot, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3"))
shh(library(png, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3"))
shh(library(grid, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3"))

cohorts <- fread(paste0(READY_DATA,"00_cohorts.txt")) 
i <- unique(cohorts %>% fi(!ref) %>% pu(cohort))
b <- unique(cohorts %>% fi(ref) %>% pu(cohort))

ready <- readRDS(paste0( READY_FIG, "READY_FIGURES.rds"))
for( j in names(ready)){ assign(j, ready[[j]])}

set_title <- function (hjust = 0.5, vjust = 0, size = 14, color = hartwig_red, face = "bold") {
    theme(plot.title = element_text(hjust = hjust, vjust = vjust, size = size, colour =color, face = face))
}

set_panel_title <- function(size = 22, hjust = 0, vjust = 1.5, face = "bold"){
    set_title(size = size, hjust = hjust, vjust = vjust, face = face)
}
marg <- function(t=0, r=0, b=0, l=0) {theme(plot.margin = margin(t = t, r = r, b = b, l = l, "cm"))}
panel <- theme(panel.background = element_rect(colour = "black", linewidth=4, fill=NA))

clinical_panel_layout <- rbind(c(2,2,2,2,2,2,3,3,3,3,3,3,3,3,3))

clinical_panel <- 
  as_ggplot(arrangeGrob(
            avail + marg(l=.4), trt + no_x_text + no_x_line + marg(l=.4), 
            layout_matrix = clinical_panel_layout)
  ) + 
  ggtitle( "Cohort Metadata") + 
  set_panel_title()

summary_panel_theme <- 
list(ggtitle( "Tumor Characteristics"), legend_sizer(i = 16, j = .4), 
     theme(legend.spacing.x = unit(.4, 'cm')),set_panel_title())

summary_panel <- 
summary + ggtitle( "Tumor Characteristics") + 
                   legend_sizer(i = 16, j = .4) + 
                   theme(legend.spacing.x = unit(.4, 'cm')) +
                   set_panel_title()

mutation_panel_layout <- rbind(c(1,1,1,1,2,2,2,2,3,3,3,3),
                               c(4,4,4,4,4,4,4,4,4,4,4,4),
                               c(5,5,5,5,6,6,6,6,7,7,7,7), 
                               c(8,8,8,9,9,9,10,10,10,11,11,11))

s <- set_title(size = 11, face = "plain", color = "black")

somatic_panel <- go(
    arrangeGrob(
        purity, clonal, tmb,  
        somatic + s,
        telomere, 
        cdr3 + no_y, 
        msi,
        ploidy + ggtitle("Mean genome ploidy") + s, 
        aneuploidy + s,
        scna + ggtitle("% Copy number alteration") + s,
        loh + ggtitle("% Genome LOH") + s, 
        layout_matrix = mutation_panel_layout )) + 
    ggtitle( "Mutational Landscape" ) + 
    set_panel_title()

left_layout <- cbind( c(1,1,1,2,2,3,3,3,3,3,3,3,3,3))

pad <- .2

left <- go( arrangeGrob(clinical_panel + marg( r= 0, b=pad + .4), 
                        summary_panel + marg(r=pad, b=pad + .4), 
                        somatic_panel + marg(r=pad, b=pad), 
                        layout_matrix = left_layout))

middle_layout <- cbind( c(1,1,1,2,2,2,2,3,3,3,3,3))

middle <- go( arrangeGrob(
              sigs + set_panel_title(hjust = .5, vjust = -2)  + marg(r=-2,b=-1,l=-2),  
              schematic + marg(),
              circos + set_panel_title(hjust = .5, vjust = -4) + marg(b=-.7,l=-.7),
              layout_matrix = middle_layout ))

pad <- .1
onco_pads <- marg(t=pad,r=pad,b=pad,l=pad)

widths <- c(1.3,2.5, 5)

germline_panel <-
    plot_grid(events_germ, driver_labels_germline, onco_germline,
              align = "h", 
              axis = "b", 
              rel_widths = widths, 
              ncol = 3, 
              nrow = 1) + 
    ggtitle("Germline Predisposition") + 
    set_panel_title() + 
    onco_pads

driver_panel <-
    plot_grid( events_driver, driver_labels_somatic, onco_somatic,
               align = "h", 
               axis = "b", 
               rel_widths = widths, 
               ncol = 3, 
               nrow = 1) + 
    ggtitle( "Cancer Driver Landscape" ) + 
    set_panel_title() +
    onco_pads

widths <- c(1.3,1.9, 5)

small_legend <- theme(legend.spacing.x = unit(.5, 'cm'), legend.text = element_text(size = 9.5)) 

actionable_panel <-
    plot_grid(events_act, 
              actionable_labels, 
              onco_actionable + small_legend,
              align = "h", 
              axis = "b", 
              rel_widths = widths, 
              ncol = 3, 
              nrow = 1) +
    ggtitle( "Potentially Actionable Events") + 
    set_panel_title() + 
    onco_pads

oncos_layout <- cbind(c(1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3))

options(repr.plot.width = 12, repr.plot.height = 18, repr.plot.res = 200)
right <- as_ggplot(arrangeGrob(driver_panel, 
                               actionable_panel, 
                               germline_panel, layout_matrix = oncos_layout))

leg <- 
get_legend(coverage$bto + 
           guides(fill=guide_legend(nrow=2,byrow=TRUE, reverse=TRUE)) + 
                  theme(legend.spacing.x = unit(.5, 'cm'), 
                        legend.text = element_text(size = 11.5)))

cov_panel <- 
plot_grid(leg,
          coverage$bto + theme(legend.position = "none", axis.title.y=element_blank(), plot.title = element_text(hjust = .5, size = 14)), 
          coverage$avg + theme(axis.title.y=element_blank(), plot.title = element_text(hjust = .5, size = 14)), 
          coverage$tso + theme(axis.title.y=element_blank(), plot.title = element_text(hjust = .45, size = 16)), 
          coverage$amp + theme(axis.title.y=element_blank(), plot.title = element_text(hjust = .45, size = 16)),
          align = "v", 
          axis = "b", 
          rel_heights = c(1.1,5,5,7,7), 
          ncol = 1, 
          nrow = 5) +
    ggtitle( "WGS vs Panel Coverage") + 
    set_panel_title(hjust = .7) + 
    onco_pads

big_layout <- t(c(rep(1,10),rep(2,7),rep(3,14), rep(4,8)))

options(repr.plot.width = 12, repr.plot.height = 24, repr.plot.res = 100)

vignette <- as_ggplot((arrangeGrob( left , middle, right, cov_panel, layout_matrix = big_layout)))

name <- cohorts %>% fi(!ref) %>% head(1) %>% pu(name)

title <- paste0( "The Genomic and Actionability Landscape of ", name)

image <- vignette + 
           set_title(hjust = .43, vjust = 7, size = 30, color = "black") + 
           ggtitle(title)  + 
           marg(t=3) + 
           theme(plot.title.position = "plot")

logo <- readPNG(source = paste0(LOGO_DIR, "/hartwig_log.png"))
cc <- readPNG(source = paste0(LOGO_DIR, "/license.png"))

license <- "CC BY-NC: This license enables reusers to distribute, remix, adapt,\nand build upon the material in any medium nor format for noncommercial\npurposes only, and only so long as attribution is given to the creator."
website <- "Website: https://www.hartwigmedicalfoundation.nl\nTools: https://github.com/hartwigmedical/hmftools\nDatabase: https://www.hartwigmedicalfoundation.nl/en/data/database/"
header <- "Panel annotations and abbreviations"

image_go <- 
image + 
 annotation_custom(rasterGrob(logo), xmin =0, xmax = .08, ymin = 1.01, ymax = 1.09) + 
 annotation_custom(rasterGrob(cc), xmin = .95, xmax = 1, ymin = 1.02, ymax = 1.08) +
 annotate("text",x=.972,y=1.02,label=license, size = 1.3) +
 annotate("text",x=.874,y=1.047,label=website, size = 3.2) 

go <- ggdraw(add_sub(image_go, header, x = 0, hjust = 0, size = 15, fontface = "bold"))
go_blank_bottom <- ggdraw(add_sub(go, "\n\n\n\n\n" , x = 0, hjust = 0, size = 13, fontface = "plain"))

header <- "Panel annotations and abbreviations"
bottom_left <- "Cohort Metadata: WGS - whole genome sequencing, RNASeq - RNA Sequencing, Chemo - Chemotherapy, Targeted - Targeted Therapy, Hormonal - Hormonal Therapy, Immuno - Immunotherapy, Radio - Radiotherapy.\nTreatments - some samples received multiple types of treatment. Patients have both pre and post biopsy treatment available. Responders - Complete or partial response based on RECIST criteria.\nTumor Characteristics: TMB - tumor mutational burden, MSI - microsatellite instable, WGD - whole genome doubling, GIE - genetic immune escape, HLA - human leukycte antigen, LOH - loss of heterozygosity, HRD - homologous recombination deficiency.\nMutational Landscape: SNV - single nucleotide variant, INDEL - insertion or deletion, Structural - structural variant, MNV - multiple nucleotide variant, VDJ - variable–diversity–joining, CDR - Complementarity-determining regions.\nProcesses Underlying Mutations: Processes estimated based on single base substitutions and trinucelotide contexts. Signatures were fit using deconstructSigs in R with cosmic version 3.4 and grouped based on proposed aetiology.\nCopy Number Alteration Profile: Inner ring shows the percentage of tumours with homozygous deletion (dark blue), LOH and significant loss (copy number < 0.6× sample ploidy; blue) and near copy neutral LOH (light blue).\nOuter ring shows percentage of tumours with high level amplification (>3× sample ploidy; dark red), moderate amplification (>2× sample ploidy; red) and low level amplification (>1.4 × sample ploidy; light red).\nFrequently observed high-level driver gene amplifications (red) and homozygous deletions (blue)."
bottom_middle <- "Cancer Driver Landscape, Germline Disposition: Events - events per tumor, TSG - tumor suppressor gene, ONC - oncogene. Summary %'s based on all samples, oncoplots only show samples with atleast 1 event. \nPotentially Actionable Events: References Jackson Clinical Knowledgebase database. # Events: FDA - FDA approved, Off - Drug Repurposing, Exp - Experimental/Other Guidelines, Oth - Any other event.\nTMB high - tumor mutational burden > 10 mutations per megabase, MSI high - > 4 microsatellite inserts per megabase, Rest * - All remaining potentially actionable events."
bottom_right <- "WGS vs Panel Coverage Summary: In-silico coverage study comparing WGS to comprehensive and targeted panels across\nvarious genomic biomarkers. WGS actionable biomarkers identified by the Jackson Clinical Knowledgebase (CKB) used as\na baseline, and panel coverage determined by rules below:\n   -Signatures: TMB High and MSI High captured by comprehensive panel (no HRD); only MSI High covered by targeted panel.\n   -Small variants/splice sites: Coverage measured by comparing genomic coordinates of events to panel BED input files.\n   -Copy number: Events for panel genes with min copy number < 0.5 or max copy number > 6 assumed covered.\n.  -Other events: Fusions, HMZ (Homozygous) Distruptions, Viral inserts, HLA type assumed not captured by panels.\n** See documentation for further details on the WGS vs Panel coverage study."

extra <- paste0("Acronym: ", cohorts$acronymn[1], "\nDOIDs included: ", ready$doids, "\nDate created from database: ", time_stamp)

options(repr.plot.width = 36, repr.plot.height = 24, repr.plot.res = 100)
lets_go <- 
  go_blank_bottom + 
    annotate("text",x=0,y = .055,label=bottom_left, size = 2.8, hjust = 0, vjust = 1) +
    annotate("text",x=15.5/35,y = .055,label=bottom_middle, size = 2.8, hjust = 0, vjust = 1) +
    annotate("text",x=15.5/35,y = .03,label=extra, size = 4, hjust = 0, vjust = 1) + 
    annotate("text",x=28/35,y = .055,label=bottom_right, size = 2.8, hjust = 0, vjust = 1) + 
    marg(r = 2, l = 2, t = 1, b = 1)

o_file <- clean_filename(paste0(cohorts$name[1], "_", i,"_vs_",b,".png"))
o_file_pieces <- clean_filename(paste0(cohorts$name[1], "_", i,"_vs_",b,"pieces.rds"))

lets_go

ggsave(paste0(SHARE_FIG, o_file), 
       plot = lets_go, 
       width = 36, 
       height = 24, 
       dpi = 300)

file.copy(from = paste0( READY_FIG, "READY_FIGURES.rds"), 
          to = paste0(SHARE_FIG, o_file_pieces), 
          overwrite = TRUE)
