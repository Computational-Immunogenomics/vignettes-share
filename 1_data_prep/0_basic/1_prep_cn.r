source("../../mission_control/shortcuts.r")
source("../../mission_control/map.r")

ploidy <- query("execute_sql_on_prod 'select sampleId, ploidy from hmfpatients.purity'")
cn <- query("execute_sql_on_prod 'select sampleId, chromosome, start, end, segmentStartSupport, segmentEndSupport, copyNumber, minorAlleleCopyNumber from hmfpatients.copyNumber'")

ref <- 
ij(
 cn %>% fi( sampleId == ploidy$sampleId[1], segmentEndSupport == "CENTROMERE")
    %>% tm( chromosome, end_p = end ),
 cn %>% gb(chromosome) %>% su(end_q = max(end)) %>% ug(),
 by = "chromosome"
)

genome_size <- sum(ref$end_q)

arms <-
cn %>% 
 lj(ref, by = "chromosome") %>% 
 mu( chr_arm = ifelse(start > end_p, paste0(chromosome,"q"), paste0(chromosome,"p")),
     length = end - start) %>% 
 mu( arm_length = ifelse(grepl("p",chr_arm), end_p,end_q - end_p)) %>% 
 fi(!chr_arm %in% c("13p", "14p", "15p", "21p", "22p", "Xq", "Xp")) %>% 
 se(sampleId, copyNumber, chr_arm, length, arm_length)

arm_deviations <- 
ploidy %>% 
 lj(arms, by = "sampleId") %>% 
 mu(
  amp = ifelse(copyNumber > 2.5, length/arm_length, 0), 
  del = ifelse(copyNumber < 1.5, length/arm_length, 0), 
  amp_ploidy = ifelse(copyNumber - ploidy > .5, length/arm_length, 0),
  del_ploidy = ifelse(copyNumber - ploidy < -.5, length/arm_length, 0)
 ) %>% 
 gb(sampleId, chr_arm) %>% 
 su(amp = sum(amp), 
    del = sum(del), 
    amp_ploidy = sum(amp_ploidy), 
    del_ploidy = sum(del_ploidy), 
    .groups = 'drop') %>% 
 mu(
  event = (amp > .8 | del > .8), 
  event_ploidy = (amp_ploidy > .8 | del_ploidy > .8)
 ) %>% 
 gb( sampleId) %>% 
 su( chr_arm_deviations = sum(event, na.rm = TRUE), 
     chr_arm_deviations_ploidy = sum(event_ploidy, na.rm = TRUE), .groups = 'drop')

lohs_cna <- 
cn %>% 
 mu( 
  length = end - start, 
  loh = ifelse( minorAlleleCopyNumber < .3, 1, 0),
  cna = ifelse( copyNumber < 1.5 | copyNumber > 2.5, 1, 0), 
  loh_contribution = length*loh, cna_contribution = length*cna 
 ) %>% 
 gb( sampleId) %>% 
 su( loh_sample = sum(loh_contribution)/genome_size, 
     cna_sample = sum(cna_contribution)/genome_size, .groups = 'drop') %>% 
 ug()

vamos <- arm_deviations %>% ij( lohs_cna, by = "sampleId")

fwrite( vamos, file = paste0( READY_DATA, "0_basic-1_prep_cn.txt") )
