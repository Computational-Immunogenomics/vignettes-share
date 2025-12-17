### Run queries
import os
os.chdir(os.getcwd())
os.system("./run.sh")

### Add time stamp to query
from datetime import date
f = open("time_stamp.txt","w")
f.write(str(str(date.today())))
f.close()
