source("../../mission_control/shortcuts.r")
source("../../mission_control/map.r")

clinical <- query("execute_sql_on_prod 'select * from hmfpatients.clinical'")
purity <- query("execute_sql_on_prod 'select * from hmfpatients.purity'") %>% tm(sampleId, WGS = purity)
trt_response <- query("execute_sql_on_prod 'select a.id as id, a.patientId as patientId, treatmentGiven, radiotherapyGiven, treatmentId, type, response from treatment a left join treatmentResponse b on a.id = b.treatmentId and a.patientId = b.patientId'")
rano_response <- query("execute_sql_on_prod 'select a.patientId as patientId, a.id as id, overallResponse, type, radiotherapyGiven, treatmentGiven from treatment a left join ranoMeasurement b on a.patientId = b.patientId'")
patient <- query("execute_sql_on_prod 'select * from hmfpatients.patient'")
sample <- query("execute_sql_on_prod 'select * from hmfpatients.sample'")

available <- function(i){
 if( i == "NULL" | is.na(i) | i == 0){ 0 } 
 else { 1 }
}

data_avail <- 
purity %>% 
 lj( clinical %>% 
     tm(sampleId, 
        Treatment = treatment, 
        Pretreatment = hasSystemicPreTreatment, 
        Response = firstResponse, 
        `Age, Sex` = clinicalGender, 
        RNASeq = hasRNA, 
        `Biopsy site` = biopsySite) , by = "sampleId") %>% 
 rw() %>% 
 mu(across(-sampleId, available)) %>% 
 ug()

bor_binary_go <- function( responses ){
    if( grepl("CR", responses) || grepl("PR", responses)){ TRUE } 
    else if (grepl("SD", responses) || grepl("PD", responses)){ FALSE } 
    else {NA}
}
short <- function(i){paste0(unique(i), collapse = ",")}
null_to_na <- function(i) ifelse(i == "NULL", NA, i)

trt_ready <- 
trt_response %>% 
  se(id = patientId, treatmentId, response, type, radiotherapyGiven, treatmentGiven) %>% 
  bind_rows(
   rano_response %>% 
    tm(id = patientId,treatmentId = as.character(id),response = overallResponse, type,radiotherapyGiven,treatmentGiven)
  ) %>%   
  gb(id, treatmentId) %>% 
  su( responses =  short(response),
      postTreatments = short(type),
      radiotherapyGiven = short(radiotherapyGiven),
      treatmentGiven = short(treatmentGiven), .groups = 'drop') %>% 
  rw() %>% 
  mu(bor_binary = bor_binary_go(responses)) %>% 
  ug()

trt_response_base <- 
sample %>% 
 lj(patient, by = "donorId") %>% 
 se(sampleId, id) %>% 
 lj(trt_ready, by = "id", relationship = "many-to-many")

trt_maps <- 
list("chemotherapy" = "Chemo", 
     "multiple therapy" = "Multiple", 
     "targeted therapy" = "Targeted", 
     "hormonal therapy" = "Hormonal", 
     "immunotherapy" = "Immuno", 
     "radio" = "Radio")

map_trt <- function(i) {
  if( is.na(i) | i == "NULL" | i == "Unknown"){ "None or NA"}
  else if( i %in% names(trt_maps)) {trt_maps[[i]]}
  else { "Other"}
}

trt_response_ready <- 
rbind(
 trt_response_base %>% se(sampleId, postTreatments, bor_binary), 
 trt_response_base %>% 
  fi(tolower(radiotherapyGiven) == "yes") %>% 
  tm(sampleId, postTreatments = "Radio", bor_binary)
 ) %>% 
 mu(postTreatments = tolower(ifelse(grepl(",", postTreatments), "Multiple therapy", postTreatments))) %>% 
 rw() %>% 
 mu(treatments = map_trt(postTreatments)) %>% 
 ug() %>% 
 gb(sampleId, treatments) %>% 
 su(response = ifelse(sum(bor_binary, na.rm = TRUE) > 0, 1, 0), .groups = "drop") %>% 
 ug() %>% 
 sp(treatments, response)

clin_ready <- 
data_avail %>% 
 lj(trt_response_ready, by = "sampleId") %>% 
 mu(across(.cols = colnames(trt_response_ready)[-1], 
           .fns = ~ ifelse(Treatment == 0, NA, .))) 

fwrite(clin_ready, paste0(READY_DATA, "0_basic-0_prep_clin.txt"))
