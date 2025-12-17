go <- function( grob ) as_ggplot(grob) + add_padding()

set_title <- function (hjust = 0.5, vjust = 0, size = 14, color = "#7F3D17", face = "bold") {
	    theme(plot.title = element_text(hjust = hjust, vjust = vjust, size = size, colour = color, face= face))
}

add_padding <- function(k = .2) theme(plot.margin = margin(k,k,k,k, "cm"))
add_legend <- function(i = "none") theme(legend.position=i)

draw_border <- theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

no_y_label <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
no_x_label <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

y_title_vertical <-  theme(axis.title.y = element_text(angle = 90))
y_title_horizontal <-  theme(axis.title.y = element_text(angle = 0))
no_title <- theme(plot.title = element_blank())
no_y_title <- theme(axis.title.y = element_blank())
no_x_title <- theme(axis.title.x = element_blank())

no_y <- no_y_label + no_y_title
no_x <- no_x_label + no_x_title

x_text <- theme(axis.text.x = element_text(size = 9))
no_x_text <- theme(axis.text.x = element_blank())
no_y_text <- theme(axis.text.y = element_blank())

no_ticks <- theme(axis.ticks = element_blank())

remove_y <- theme(axis.line.y = element_blank())
no_ticks <- theme(axis.ticks = element_blank())
no_y_line <- theme(axis.line.y = element_blank())
no_x_line <- theme(axis.line.x = element_blank())

dodge_x <- scale_x_discrete(guide = guide_axis(n.dodge=3))
dodge_y <- scale_y_discrete(guide = guide_axis(n.dodge=3))

pct_x_scale <- scale_x_continuous(labels = scales::percent)
pct_y_scale <- scale_y_continuous(labels = scales::percent)

bottom_legend <- theme(legend.position = "bottom")
right_legend <- theme(legend.position = "right")
no_legend <- theme(legend.position = "none")
no_legend_title <- theme(legend.title=element_blank())
rev_legend <- guides(fill = guide_legend(reverse=T, nrow = 1) )
rev_legend2 <- guides(fill = guide_legend(reverse=T, nrow = 2) )
legend_sizer <- function(i = 10, j = .5)  {
    theme(legend.text  = element_text(size = i),  legend.key.size = unit(j, 'cm'))
}
top_legend <- theme(legend.position=c(.5,.9), 
                    legend.justification='left',
                    legend.direction='horizontal',
                    legend.text = element_text(size = 14))

no_alpha <- guides(alpha = "none", color = "none")

vertical_x <- theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank())
horizontal_x <- theme(axis.text.x = element_text(angle = 0), axis.title.x = element_blank())

clean_background <- theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

text_color <- function(i){
    theme(axis.text.y=element_text(color=dr_labs$colors[[i]]))
}

no_grid <- (theme(plot.background = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.border = element_blank()))


### Themes 
base_theme <- theme_classic(base_size = 13)
vamos <- (theme(strip.placement = "outside",
                 strip.background = element_blank(),
                 legend.box.spacing = unit(0, "pt"),
                 legend.title=element_blank())
          + add_padding()
          + add_legend()
          + set_title(color = "black")
)
panel_theme <- vamos + set_title(hjust = 0, vjust = 1, size = 17) + add_padding() # + draw_border

common <- base_theme + no_y_title + no_ticks + no_x_text + add_padding()
onco_theme <- common + bottom_legend + no_y_text + no_y_line + no_legend_title
label_theme <- common  + no_legend + horizontal_x
samps_theme <- theme_minimal() + vamos + no_x_title + no_y + add_padding(.3)
cohort_theme <- base_theme + vamos + no_y_title + no_x_title + no_x_text + no_x_line + no_ticks
title_set <- set_title(size = 14, face = "plain", color = "black")
