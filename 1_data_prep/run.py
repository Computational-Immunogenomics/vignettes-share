import os

aqui = os.getcwd()
data_dirs = [aqui + "/" + i for i in ['0_basic', '1_driver', '2_actionable', '3_signatures', '4_circos','5_panel_coverage']]

for i in data_dirs:
    os.chdir(i)
    os.system('./run.sh')
