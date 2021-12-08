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
font_import('./Custom_Fonts/')
fonts()
loadfonts()

d <- data.frame("xmin" = c(0, 45), "xmax" = c(15, 60), 
                "ymin" = c(50, 50), "ymax" = c(60, 60))

dline <- data.frame("x1" = c(15), "y1" = c(55), "xend" = c(45), "yend" = c(55))

dcurve <- data.frame("x1" = c(7.5, 52.5), "y1" = c(50, 50), 
                     "xend" = c(22.4, 37.4), "yend" = c(7.5, 7.5))

ggplot() + geom_rect(data = d, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                     fill = "#fcfffd", color = '#2d4059', 
                     size = 0.4) + 
  geom_text(aes(x = 7.5, y = 55, label = "Heart Disease"), size = 2, color = '#2d4059', family = 'Proxima Nova') +
  geom_text(aes(x = 52.5, y = 55, label = "Outcome"), size = 2, color = '#2d4059', family = 'Proxima Nova') +
  #geom_text(aes(x = 30, y = 7, label = "Death"), size = 5) +
  #geom_text(aes(x = 30, y = 52.5, label = '?'), size = 3, parse = FALSE) +
  #geom_text(aes(x = 30, y = 52.5, label = 'h^t~(t)'), size = 3, parse = TRUE) +
  #geom_text(aes(x = 12.5, y = 30, label = "h^d~(t)"), size = 3, parse = TRUE) +
  #geom_text(aes(x = 47.5, y = 30, label = "h[2][3](t)"), size = 3, parse = TRUE) +
  geom_segment(data = dline, aes(x = x1, y = y1, xend = xend, yend = yend), size = 0.4, 
               arrow = arrow(length = unit(0.04, "npc")), color = '#ea5455') + 
  # geom_segment(data = dcurve[1, ], aes(x = x1, y = y1, xend = xend, yend = yend), 
  #              size = 1,  
  #              arrow = arrow(length = unit(0.04, "npc"))) +
  # # geom_segment(data = dcurve[2, ], aes(x = x1, y = y1, xend = xend, yend = yend), 
  #              size = 1,  
  #              arrow = arrow(length = unit(0.02, "npc"))) +
  ylim(0, 60) + xlim(0, 60) + 
  theme_void() + theme(panel.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'))

ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\Two_State.png'), 
       height = 648, width = 1280, units = 'px')


