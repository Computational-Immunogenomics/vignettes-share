import sys
import os
aqui = os.getcwd()

pieces = ['/1_data_prep', '/2_figures', '/3_build']

for i in pieces:
    print("Go! " + i)
    os.chdir(aqui + i)
    os.system('python run.py')