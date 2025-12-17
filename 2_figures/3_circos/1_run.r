source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")


cmd = paste0("sed 's|READY_DATA|", READY_DATA,"|g' 0a_go.conf > 0_go.conf")
print(cmd); system(cmd)
    
cmd2 = paste0("/data/tools/circos/0.69.6/bin/circos -nosvg -conf 0_go.conf -outputdir ", CIRCOS_FIG, " -outputfile ", vignette_save_name,".png")
print(cmd2); system(cmd2)
    
o_file <- paste0(CIRCOS_FIG, vignette_save_name, ".png")
ready_file <- paste0(READY_FIG, "3_circos-1_run.png")
    
cmd3 = paste0("cp ", o_file, " ", ready_file)
print(cmd3); system(cmd3)
