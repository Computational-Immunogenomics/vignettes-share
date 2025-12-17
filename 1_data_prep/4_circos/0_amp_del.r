source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")

ploidy <- query("execute_sql_on_prod 'select sampleId, ploidy from hmfpatients.purity'")
cn <- query("execute_sql_on_prod 'select sampleId, chromosome, start, end, segmentStartSupport, segmentEndSupport, copyNumber, minorAlleleCopyNumber from hmfpatients.copyNumber'")

cn_go <- ploidy %>% lj(cn, by = "sampleId")

ready <- 
cn_go %>% 
 gb(sampleId) %>%
 mu(
  male = sum(chromosome == "Y") > 0 ,
  copyNumber = ifelse(male & (chromosome %in% c("X","Y")), 2*copyNumber, copyNumber ), 
  majorAlleleCopyNumber = copyNumber - minorAlleleCopyNumber,
  homozygous_del = (copyNumber < .5 & chromosome != "Y"),
  loh =  (minorAlleleCopyNumber < 0.3 & majorAlleleCopyNumber > 0.7),
  loh = ifelse(male & (chromosome %in% c("X","Y")), FALSE, loh), 
  neutral_loss_and_loh = (copyNumber >= .6 * ploidy) & (loh) & (!homozygous_del),
  sig_loss_and_loh = ifelse(
    male & (chromosome %in% c("X","Y")),
    (copyNumber < .6 * ploidy) & (!homozygous_del),
    (copyNumber < .6 * ploidy) & (loh) & (!homozygous_del)),
  low_amp = (copyNumber > 1.4 * ploidy) & (copyNumber <= 2.0 * ploidy),
  mod_amp = (copyNumber > 2 * ploidy)  &  (copyNumber <= 3.0 * ploidy),
  high_amp = copyNumber > 3 * ploidy) %>% 
 ug()

go <- cohorts %>% left_join(ready, by = "sampleId", relationship = "many-to-many")

k <- 10000000

computer <- function(chr = 1, pos = 0, k = 1000000) { ### us data.table for speed
  end_pos <- pos + k

  # Ensure go is a data.table
  if (!is.data.table(go)) setDT(go)

  # Filter overlaps
  overlap_dt <- go[
    chromosome == chr &
    ((start < pos & end > end_pos) | (start > pos & end < pos) |
     (start < pos + k & end > end_pos) | (start < pos & end > pos))]

  # First aggregation by cohort + sampleId
  summed <- overlap_dt[
    , .(
      homozygous_del = sum(homozygous_del),
      neutral_loss_and_loh = sum(neutral_loss_and_loh),
      sig_loss_and_loh = sum(sig_loss_and_loh),
      low_amp = sum(low_amp),
      mod_amp = sum(mod_amp),
      high_amp = sum(high_amp)
    ),
    by = .(cohort, sampleId)
  ]

  # Add static columns
  summed[, `:=`(chr = paste0("hs", chr), start = pos)]

  # Second aggregation
  result <- summed[
    , .(
      homozygous_del = round(mean(homozygous_del > 0), 3),
      neutral_loss_and_loh = round(mean(neutral_loss_and_loh > 0), 3),
      sig_loss_and_loh = round(mean(sig_loss_and_loh > 0), 3),
      low_amp = round(mean(low_amp > 0), 3),
      mod_amp = round(mean(mod_amp > 0), 3),
      high_amp = round(mean(high_amp > 0), 3)
    ),
    by = .(cohort, chr, start)
  ]

  return(as_tibble(result))
}

sizes <- go %>% group_by(chromosome) %>% summarise(end = max(end)) %>% ungroup() %>% drop_na()

chromosomes <- sizes$chromosome

options(dplyr.summarise.inform = FALSE)
compute <- data.frame()
system.time(
for( chr in chromosomes){
 print(chr)
 flush.console()
 start_positions = seq(0, sizes %>% filter(chromosome == chr) %>% pull(end), k)
 for( start in start_positions){
  compute <- rbind( compute, computer( chr, start, k))
 }    
})

ready <- 
compute %>% 
  ug() %>% 
  mu( end = start + k) %>% 
  mu( start = str_trim(format(start, scientific = F)), 
      end = str_trim(format(end, scientific = F))) %>% 
  ug()

saveRDS(ready, file = paste0(CIRCOS_DATA, "circos_data.rds"))

writer <- function( df, file) { fwrite( df, file, , sep = " ", col.names = FALSE)}
circos_writer <- function( i ){
    setwd(READY_DATA)
    base <- ready %>% filter(cohort == i)
    writer( base %>% transmute(chr, start, end, val = -homozygous_del), "4_circos_homozygous_del.circos")
    writer( base %>% transmute(chr, start, end, val = -neutral_loss_and_loh), "4_circos_neutral_loss_and_loh.circos")
    writer( base %>% transmute(chr, start, end, val = -sig_loss_and_loh), "4_circos_sig_loss_and_loh.circos")
    writer( base %>% transmute(chr, start, end, val = low_amp), "4_circos_low_amp.circos")
    writer( base %>% transmute(chr, start, end, val = mod_amp), "4_circos_mod_amp.circos")
    writer( base %>% transmute(chr, start, end, val = high_amp), "4_circos_high_amp.circos")
}

for( i in plt_cohort) { 
    circos_writer(i) 
}

stacked_output <- function(i){
    setwd(READY_DATA)
    columns <- c("chr","start","stop","value")
    
    ### Amplifications ### 
    low <- read_delim( "4_circos_low_amp.circos",  delim=" ", col_names=columns) %>% mutate(level="low")
    mod <- read_delim( "4_circos_mod_amp.circos",  delim=" ", col_names=columns) %>% mutate(level="mod")
    hig <- read_delim( "4_circos_high_amp.circos", delim=" ", col_names=columns) %>% mutate(level="hig")
    amps <- bind_rows(low, mod, hig) %>% pivot_wider(names_from=level, values_from=value, values_fill=0.0) %>% mutate(value=paste(hig,mod,low,sep=",")) %>% select(chr,start,stop,value)
    write_delim(amps, "4_circos_amps.circos", delim=" ", col_names=F)

    # DELS
    low <- read_delim("4_circos_neutral_loss_and_loh.circos",  delim=" ", col_names=columns) %>% mutate(level="loh")
    mod <- read_delim("4_circos_sig_loss_and_loh.circos",  delim=" ", col_names=columns) %>% mutate(level="del")
    hig <- read_delim("4_circos_homozygous_del.circos", delim=" ", col_names=columns) %>% mutate(level="hom")
    dels <- bind_rows(low, mod, hig) %>% pivot_wider(names_from=level, values_from=value, values_fill=0.0) %>% mutate(value=paste(hom,del,loh,sep=",")) %>% select(chr,start,stop,value)

    write_delim(amps, "4_circos_amps.circos", delim=" ", col_names=F)
    write_delim(dels, "4_circos_dels.circos", delim=" ", col_names=F)
}

for( i in plt_cohort) { 
    stacked_output(i) 
}
