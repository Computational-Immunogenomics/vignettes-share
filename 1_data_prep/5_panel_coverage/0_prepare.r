source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/coverage_rules.r")
source("../../mission_control/coverage.r")
source("../../mission_control/actionable_rules.r")
library(sqldf, lib.loc = "/home/jusset/R/x86_64-pc-linux-gnu-library/4.3")

amp <- query("execute_sql_on_prod 'select * from silico_actionability.AmpliSeq'")
tso <- query("execute_sql_on_prod 'select * from silico_actionability.TSO500'")

amp_genes <- 
amp %>% 
 rw() %>% mu(gene = strsplit(target, "_")[[1]][2]) %>% ug() %>% 
 pull(gene) %>% unique()

tso_genes <- 
tso %>% 
 rw() %>% mu(gene = strsplit(target, "_")[[1]][1]) %>% ug() %>% 
 pull(gene) %>% unique()

protect <- 
query("execute_sql_on_prod 'select * from hmfpatients.protect where reported = 1'") %>% 
 fi(source == "CKB", grepl("RESPONSIVE", direction), 
    !(level == "C" & onLabel == 0),!grepl("EXPRESSION", evidenceType))

somatic <- fread(paste0(QUERY_DATA,"coverage_somatic.txt"))
germline <- fread(paste0(QUERY_DATA,"coverage_germline.txt"))
cn <- fread(paste0(QUERY_DATA,"coverage_cn.txt")) 

protect <- 
protect %>% 
 rw() %>% mu( event_group = plan_the_event(event)) %>% ug() %>% 
 mu( event = ifelse(grepl("splice",event), gsub(" splice", "", event), event))

som_germ <- 
rbind(prep_events(somatic, i = "somatic"),prep_events(germline, i = "germline")) %>% 
 se(sampleId, chromosome, gene, position, event, other_event, type)

som_germ_clean <- 
rbind(
  som_germ %>% select(-other_event) %>% mutate(type2 = "main"),
  som_germ %>% filter( other_event!="") %>% select(-event) %>% rename(event=other_event) %>% mutate(type2="other")
)

together <- 
protect %>% 
 lj(som_germ_clean, by = c("sampleId","event","gene"), relationship = "many-to-many")

somatic_germ_go <- 
as.tibble(sqldf(
  "select a.*, b.chrom as tso, c.chrom as amp
   from ((together a
   left join tso b on a.chromosome = b.chrom and a.position >= b.chromStart and a.position <= b.chromEnd)
   left join amp c on a.chromosome = c.chrom and a.position >= c.chromStart and a.position <= c.chromEnd)
")) %>% 
mutate(amp = !is.na(amp), tso = !is.na(tso)) %>% 
filter(!event %in% c("full gain", "partial gain", "full loss", "partial loss")) ### remove copy number

cn_go <- 
cn %>% 
 unique() %>% 
 mu( 
  panel_del = as.numeric(maxCopyNumber) < rules$cn$min, ### need to discuss
  panel_amp = as.numeric(minCopyNumber) > rules$cn$max, ### need to discuss
  panel_event = panel_del | panel_amp,
  tso = (gene %in% tso_genes) & panel_event, 
  amp = (gene %in% amp_genes) & panel_event, 
  type = "cn", type2 = "main", event_group = event, chromosome = NA, position = NA 
 )

juntos <- bind_rows(somatic_germ_go, cn_go %>% mutate(isCanonical = as.character(isCanonical)))

leveller <- function(level, onLabel){
 if (level == "A" & onLabel == 1) { "FDA Approved" }
 else if( level == "A" & onLabel == 0){ "Drug Repurposing" }
 else if( level == "B" & onLabel == 1){ "Experimental/Other Guidelines" }
 else {"Other"}
}

ready <- 
juntos %>% 
 rw() %>% mu(evidence = factor(leveller(level, onLabel),levels = names(level_map))) %>% ug() %>% 
 gb( sampleId, event ) %>% 
 mutate( rk = row_number(evidence)) %>% filter(rk == 1) %>% ungroup() %>% select(-rk) %>% 
 gb( sampleId, gene, event) %>% summarise_all(~short(.))  %>% ug() %>% 
 fi( !(chromosome == "NA" & event_group == "small variant")) %>% 
 rw() %>% 
 mu(event_group = better_namer(event_group),
    tso_cov = apply_rules(event_group, rules, "tso", tso) ,
    amp_cov = apply_rules(event_group, rules, "amp", amp)) %>% 
 mu( tso_gene = gene %in% tso_genes, amp_gene = gene %in% amp_genes) %>% 
 ug() 

base <- 
ready %>% 
  select(sampleId, event, event_group, 
         germline, evidence, tso_gene, amp_gene, 
         tso_cov, amp_cov) %>% 
  gather("panel", "coverage", tso_cov, amp_cov) %>% 
  rowwise() %>% 
  mutate(panel = panel_maps[[panel]]) %>%
  ungroup()

clean <- 
rbind( base, base %>% filter(panel == panel_maps[["tso_cov"]]) %>% 
       mutate(coverage = TRUE, panel = panel_maps[["wgs"]]))

fwrite(clean, file = paste0(READY_DATA, "5_panel_coverage-0_prepare.txt"))
