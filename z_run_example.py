import os
import subprocess

os.chdir("/data/repos/cancer-vignettes-dev/")
r_script = "/data/repos/cancer-vignettes-dev/0_define_cohort/run/1_set_cohort.r"
python_script = "/data/repos/cancer-vignettes-dev/run.py"

data_file = "request_prostate_niven.rds"
cohorts = ["Prostate"]

for cohort in cohorts:
  subprocess.run(["Rscript", r_script, data_file, cohort], check=True)
  subprocess.run(["python", python_script], check=True)
