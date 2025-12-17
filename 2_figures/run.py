import os

aqui = os.getcwd()
fig_dirs = [aqui + "/" + i for i in ['0_basic', '1_driver', '2_actionable', '3_circos', '4_schema', '5_panel_coverage']]

for i in fig_dirs:
    os.chdir(i)
    os.system('./run.sh')
