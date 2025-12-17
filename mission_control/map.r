### Top file paths must be change to switch dev/prod
REF_REPO="/data/repos/cancer-vignettes-dev/"
REF_DATA="/data/tmp/vignettes-dev/"

### Derived filepaths from above
REF_DIR=paste0(REF_REPO, "mission_control/")
REFS_DIR=paste0(REF_REPO, "ref/")
LOGO_DIR=paste0(REFS_DIR, "logo/")

QUERY_DATA=paste0(REF_DATA, "util/data/query/")
READY_DATA=paste0(REF_DATA, "util/data/ready/")
COHORTS_DIR=paste0(REF_DATA, "util/data/cohorts/")
CIRCOS_DATA=paste0(REF_DATA, "util/data/circos/")
CIRCOS_FIG=paste0(REF_DATA, "util/figures/circos/")
READY_FIG=paste0(REF_DATA, "util/figures/ready/")
SHARE_FIG=paste0(REF_DATA, "util/figures/share/")
FINAL_FIG=paste0(REF_DATA, "util/figures/share/update/")