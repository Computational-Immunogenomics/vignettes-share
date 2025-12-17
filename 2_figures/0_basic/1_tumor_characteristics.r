source("../../mission_control/map.r")
source("../../mission_control/shortcuts.r")
source("../../mission_control/set_cohort.r")
source(paste0(REF_DIR, "colors.r"))

ready <- readRDS(paste0( READY_DATA, "0_basic-4_compute.rds"))$tc

colors <- c()
colors[plt_cohort] <- fills[['plt']]; colors[[ref_cohort]] <- fills[['ref']]

bar_maker <- function( df, colors ){
 ymax <- max(ready$val, na.rm = TRUE) + .1
 ggplot(data = df, aes( x = name, y = val,  label = val, fill = cohort)) +
  geom_bar(stat="identity", width=.8, position = 'dodge', size = 1, color = "black", alpha = 1) + 
  scale_fill_manual( values = colors) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limit = c(0,ymax+.23), expand = c(0,0)) + 
  geom_text(aes(label=paste0(100*round(val,2),"%")),stat = "identity", position = position_dodge(width = .8),color="black",size=4,hjust=.5, vjust = -.5) + 
  ggtitle("Summary")
}

bars <- bar_maker( ready, colors)

saveRDS(  bars, paste0( READY_FIG, "0_basic-1_tumor_characteristics.rds"))
