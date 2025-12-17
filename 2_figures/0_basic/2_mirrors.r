source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source(paste0(REF_DIR, "colors.r"))

go <- readRDS(paste0( READY_DATA, "0_basic-4_compute.rds"))
ready <- go$mirrors
pcts <- go$tcs

mirror <- function( df, 
                    x = "tmbPerMb", 
                    plt_cohort = "Breast",
                    ref_cohort = "Pan-Cancer",
                    scale_x = "log2", 
                    breaks_x = c(1,2, 5,10,20,40, 80, 160), 
                    xlab = "TMB per Megabase", 
                    threshold = 10, 
                    pct = FALSE, 
                    xmin = .3, 
                    title = "TMB"){

    df <- df %>% mutate( x = .data[[x]])
    df_cohort <- df %>% filter(cohort == plt_cohort)  
    df_ref <- df %>% filter(cohort == ref_cohort) 
    
    color_map <- c()
    color_map[plt_cohort] <- fills[['plt']] 
    color_map[ref_cohort] <- fills[['ref']]
    
    ls <- 6
    
    plt <- ggplot( df_cohort )
    
    if( x %in% c("tmbPerMb", "msIndelsPerMb")){
     add <- 
      annotate(geom = "rect", xmin = threshold, xmax=Inf, ymin=-Inf, ymax=Inf, fill="red", alpha=0.1)
      plt <- plt + add  
    }
    
    plt <- 
    plt + 
     geom_density( aes(x = x, y = after_stat(density), fill = cohort), alpha = 1) + 
     geom_density( data = df_ref, aes(x = x, y = -after_stat(density), fill= ref_cohort), alpha = 1 ) + 
     scale_fill_manual( values = color_map ) + 
     labs(x = xlab, title = title, y = "Density")
    
    if( x == "tmbPerMb"){
      plt <- 
      plt + 
        geom_segment(aes(x = threshold, y = -.5, xend = 25, yend = -.5), 
                     arrow = arrow(length = unit(0.3, "cm"))) +  
        geom_text( aes( x = 90, y = -.5, label = "High", size = ls), 
                  check_overlap = TRUE)
    } else if ( x == "msIndelsPerMb"){
      plt <- 
      plt + 
        geom_segment(aes(x = threshold, y = -.5, xend = 10, yend = -.5), 
                     arrow = arrow(length = unit(0.3, "cm"))) +
        geom_text( aes( x = 60, y = -.5, label = "Instable", size = ls), 
                  check_overlap = TRUE)
    } 
     
    ### Scale x-axis 
    if( pct ){ 
        plt <- plt + scale_x_continuous( labels = scales::percent, breaks=breaks_x, limits = c(xmin,max(ready %>% pull(x))))
    } else {
        plt <- plt + scale_x_continuous( trans = scale_x, breaks = breaks_x, limits = c( max(min(ready %>% pull(x)),xmin), max(ready %>% pull(x))))
    }
    
    ### Add median lines
    y_range <- layer_scales(plt)$y$range$range

    medians <- c(median(df_cohort$x, na.rm = TRUE), median(df_ref$x, na.rm = TRUE))
    
    if( medians[1] < 10) { labels <- signif(medians, 2)} 
    else { labels <- round(medians)}
    if( pct ){ labels <- paste0(as.character(100*labels), "%")}
    
    plt <- 
    plt + 
      geom_segment( data = df_cohort, aes(x = median(x, na.rm = TRUE), y = 0, xend = median(x, na.rm = TRUE), 
                   yend = y_range[2]), color = "black", linewidth = .5, linetype = "dashed", lineend = "butt") +
      geom_text( data = df_cohort, aes( x = median(x, na.rm = TRUE), y = 1.14*y_range[2], label = labels[1], size = ls), check_overlap = TRUE) +
      geom_segment( data = df_ref, aes(x = median(x, na.rm = TRUE), y = 0, xend = median(x, na.rm = TRUE), yend = y_range[1]), color = "black", linewidth = .5, linetype = "dashed", lineend = "butt")

    if( plt_cohort != "Pan-Cancer"){
      plt <- 
        plt + 
         geom_text( data = df_ref, aes( x = median(x, na.rm = TRUE), y = 1.14*y_range[1], label = labels[2], size = ls), check_overlap = TRUE) 
    }
    
    if( x == "tmbPerMb"){
        tmb_high_coh <- paste0( round(100 * pcts %>% filter(cohort == plt_cohort, grepl("TMB", name)) %>% pull(val),0), "%")
        tmb_high_ref <- paste0( round(100 * pcts %>% filter(cohort == ref_cohort, grepl("TMB", name)) %>% pull(val),0), "%")
        plt <- plt + geom_text( aes( x = 650, y = 1*y_range[2], label = tmb_high_coh, size = ls), check_overlap = TRUE)
        if( plt_cohort != "Pan-Cancer"){
            plt <- plt + geom_text( aes( x = 650, y = 1*y_range[1], label = tmb_high_ref, size = ls), check_overlap = TRUE)
        }
    } else if (x == "msIndelsPerMb"){
        msi_high_coh <- paste0( round(100 * pcts %>% filter(cohort == plt_cohort, grepl("MSI", name)) %>% pull(val),0), "%")
        msi_high_ref <- paste0( round(100 * pcts %>% filter(cohort == ref_cohort, grepl("MSI", name)) %>% pull(val),0), "%")
        plt <- plt + geom_text( aes( x = 100, y = 1*y_range[2], label = msi_high_coh, size = ls), check_overlap = TRUE)
        if( plt_cohort != "Pan-Cancer"){
            plt <- plt + geom_text( aes( x = 100, y = 1*y_range[1], label = msi_high_ref, size = ls), check_overlap = TRUE)
        }
    }
    plt
}

tmb_mirror <- function( go, cohort, ref_cohort ){
 mirror( 
  go, 
  x = "tmbPerMb", 
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "log10",
  breaks_x = c(1,10,100), 
  xlab = "TMB per Megabase", 
  threshold = 10, 
  pct = FALSE, 
  xmin = .1, 
  title = "Tumor mutational burden")
}
sv_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go, 
  x = "svTmb", 
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "log10",
  breaks_x = c( 1 ,10, 100, 1000), 
  xlab = "Structual variants", 
  threshold = 0,  
  pct = FALSE, 
  xmin = 1, 
  title = "Structural variant burden")
}
msi_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go, 
  x = "msIndelsPerMb", 
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "log10",
  breaks_x = c(1,4, 10, 100), 
  xlab = "MS indels per megabase", 
  threshold = 4,
  pct = FALSE, 
  xmin = .01, 
  title = "Microsatellite instability")
}
ploidy_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go, 
  x = "ploidy", 
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "log2",
  breaks_x = c(1, 2, 4, 8), 
  xlab = "", 
  pct = FALSE,
  threshold = 0,
  title = "Mean ploidy") 
}
clonal_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go,     
  x = "clonal", ## or clonal
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "identity",
  breaks_x = c( .5,1 ), 
  xlab = "", 
  threshold = 0,
  pct = TRUE, 
  xmin = 0, 
  title = "Clonal somatic mutation fraction")
}
diploid_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go,     
  x = "diploidProportion",
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "identity",
  breaks_x = c( 0, .5,1 ), 
  xlab = "", 
  threshold = 0,
  xmin = 0, 
  pct = TRUE, 
  title = "% Diploid")
}
purity_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go,     
  x = "purity",
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "identity",
  breaks_x = c( 0, .2,.5,1 ), 
  xlab = "", 
  threshold = .2,
  pct = TRUE, 
  xmin = .2, 
  title = "Tumor purity")
}
loh_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go,     
  x = "loh_sample",
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "identity",
  breaks_x = c( 0, .5, 1 ), 
  xlab = "", 
  threshold = 0,
  pct = TRUE, 
  xmin = 0, 
  title = "% of genome with loss of heterozygosity")
}
scna_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go,     
  x = "cna_sample",
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "identity",
  breaks_x = c( 0, .5, 1 ), 
  xlab = "", 
  threshold = 0,
  pct = TRUE, 
  xmin = 0, 
  title = "% of genome with copy number alterations")
}
aneuploidy_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go,     
  x = "chr_arm_deviations",
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "identity",
  breaks_x = c( 0, 13, 26, 39 ), 
  xlab = "", 
  threshold = 0,
  pct = FALSE, 
  xmin = 0, 
  title = "Aneuploidy Score")
}
telomere_mirror <- function( go, cohort, ref_cohort ){
 mirror(
  go,     
  x = "telomere_ratio",
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "log2",
  breaks_x = c(.2, 0, 1, 5),  
  xlab = "Log2 (Tumor/Germline length)", 
  threshold = 0,
  pct = FALSE, 
  xmin = -5, 
  title = "Telomere Length")
}
cdr3_mirror <- function( go, cohort, ref_cohort){
 mirror(
  go,     
  x = "cdr3_seqs",
  plt_cohort = cohort,
  ref_cohort = ref_cohort, 
  scale_x = "log2",
  breaks_x = c(0, 2, 4, 8,20),  
  xlab = "Log2 (CDR3 + 1)", 
  threshold = 0,
  pct = FALSE, 
  xmin = 0, 
  title = "VDJ CDR3 Sequences")
}

cohort_mirrors <- function( go, plt_cohort = "Lung", ref_cohort = "Pan-Cancer"){    
    list(
      "tmb" = tmb_mirror( go, plt_cohort, ref_cohort ), 
      "msi" = msi_mirror( go, plt_cohort, ref_cohort ),
      "ploidy" = ploidy_mirror( go, plt_cohort, ref_cohort),
      "svTmb" = sv_mirror( go, plt_cohort, ref_cohort ),
      "clonal" = clonal_mirror( go, plt_cohort, ref_cohort),
      "diploid" = diploid_mirror( go, plt_cohort, ref_cohort),
      "purity" = purity_mirror( go, plt_cohort, ref_cohort), 
      "loh" = loh_mirror( go, plt_cohort, ref_cohort),
      "scna" = scna_mirror( go, plt_cohort, ref_cohort),
      "aneuploidy" = aneuploidy_mirror( go, plt_cohort, ref_cohort), 
      "telomere" = telomere_mirror(go, plt_cohort, ref_cohort),
      "cdr3" = cdr3_mirror(go, plt_cohort, ref_cohort)
     )
}

mirrors <- cohort_mirrors( go = ready, plt_cohort = plt_cohort, ref_cohort = ref_cohort)

saveRDS( mirrors, paste0( READY_FIG, "0_basic-2_mirrors.rds"), compress = FALSE)
