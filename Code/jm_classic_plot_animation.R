#--------------------------------
#        COLORS
# #2d4059 dark blue
# #ea5455 red
# #ffb400 yellow
# #1098f7 blue
# #fcfffd white
#--------------------------------

#--------------------------------
# LIBRARIES
#--------------------------------
library(gifski)
library(dplyr)
library(magrittr)
#library(emo)
library(ggplot2)
library(extrafont)
library(cowplot)
#library(emojifont)
library(purrr)
#library(Unicode)
#?font_import
font_import(paths = "./Custom_Fonts/")
fonts()
#loadfonts(device = "win", quiet = TRUE)
#library(showtext)
#font_add(family = "ADAM.CG", regular = "./Custom_Fonts/ADAM.CG PRO.otf")
#font_add(family = 'FontAwesome', regular = './Custom_Fonts/FontAwesome.otf')
#showtext_auto()

#----------------------------
# PREPARE DATA
#----------------------------
# 15 discrete time points
t <- seq(0, 15, length.out = 15)
# 15 discrete values
val <- c(1.3, 2, 2.2, 0.4, 1.4, 1.25, 0.36, 0.28, 0.26, 0.20, 0.25, 0.28, 0.4, 0.8, 1.3)
haz2 <- -2*val
group <- rep(c("Hazard", "Longitudinal outcome"), each = 15)
haz3 <- (haz2 - min(haz2)) / (max(haz2) - min(haz2))
plotdata <- data.frame("group" = group, "time" = rep(t, 2), "y" = c(haz3, val))
plotdata$panel <- rep(c("a", "b"), each = 15)
p <- ggplot(plotdata, aes(time, y)) + facet_wrap(~ group, scales = "free_y", nrow = 2, strip.position = "right") + theme_bw()
p1 <- p + geom_point(data = plotdata[plotdata$panel == "b", ], size = 1.2, alpha = 0.6, color = "black") + 
  geom_smooth(data = plotdata[plotdata$panel == "a", ], se = FALSE, method = "loess", color = "red3") + 
  geom_step(data = plotdata[plotdata$panel == "b", ], linetype = 2, alpha = 1) + 
  geom_smooth(data = plotdata[plotdata$panel == "b", ], se = FALSE, method = "loess") + xlab("Time") + 
  theme(panel.spacing = unit(0, "lines"), 
        axis.title.y = element_blank()) + theme(legend.position = 'bottom', axis.title.x = element_text(size = 8), 
                                                axis.title.y = element_text(size = 8), legend.text = element_text(size = 5),
                                                legend.key.size = unit(0.40, units = 'cm'), legend.title = element_text(size = 4),
                                                strip.text = element_text(size = 6))

ggplot_build(p1)$data

# extract smooth data coordinates
hazard_smooth_data <- ggplot_build(p1)$data[[2]]
long_smooth_data <- ggplot_build(p1)$data[[4]]

new_plotdata <- data.frame('h_smooth_x' = hazard_smooth_data$x, 
                           'h_smooth_y' = hazard_smooth_data$y, 
                           'long_smooth_x' = long_smooth_data$x, 
                           'long_smooth_y' = long_smooth_data$x, 
                           'group' = rep(c("Hazard", "Longitudinal outcome"), each = 80), 
                           'panel' = rep(c("a", "b"), each = 80))

# create animation plot
ggplot(plotdata, aes(time, y)) + facet_wrap(~ group, scales = "free_y", nrow = 2, strip.position = "right") + 
  theme_bw() + theme(axis.title.y = element_blank(), axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#2d4059'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#2d4059'), 
                     axis.title.x = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_blank())


ggsave(filename = 'D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\JM_Classic\\JMclassic_1.png', 
       height = 648, width = 1280, units = 'px')

for (i in 1:15) {
  ggplot(plotdata, aes(time, y)) + facet_wrap(~ group, nrow = 2, strip.position = "right") + 
    theme_bw() + theme(axis.title.y = element_blank(), axis.text = element_blank(), 
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                       panel.background = element_rect(fill = '#2d4059'), 
                       strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                       strip.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                       axis.line = element_line(colour = '#2d4059'), 
                       axis.title.x = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                       axis.ticks = element_blank()) + 
    # geom_point(data = plotdata[plotdata$group == "Longitudinal outcome", ][1:i, ], 
    #            aes(x = time, y = y), fill = '#ea5455', color = '#ea5455', shape = '\U0001f494', 
    #            size = 4)
  geom_point(data = plotdata[plotdata$group == "Longitudinal outcome", ][1:i, ], 
             aes(x = time, y = y), color = '#ea5455', shape = '\U2665', 
             size = 3, alpha = 0.8) + 
  geom_blank(data = plotdata[plotdata$group == "Longitudinal outcome", ]) + 
  xlim(0, 15)
  # geom_point(data = plotdata[plotdata$group == "Longitudinal outcome", ][1:i, ], 
  #              aes(x = time, y = y), color = '#ea5455', shape = '\U0001f494', 
  #              size = 1)
  ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\JM_Classic\\JMclassic_', i+1, '.png'), 
         height = 648, width = 1280, units = 'px')
}

paths <- rep(NA, 15)
for(i in 1:15) {
  paths[i] <- paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\JM_Classic\\JMclassic_', 
                    i+1, '.png')
}

gif_file <- gifski(paths, delay = 2, loop = FALSE, width = 1280, height = 648)
gif_file
?gifski
