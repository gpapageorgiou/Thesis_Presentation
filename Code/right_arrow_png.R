library(ggplot2)

ggplot() + theme_void() + 
  geom_segment(aes(x = 0, y = 0.5, xend = 1, yend = 0.5), size = 0.5, color = "#2d4059", 
               arrow = arrow(length = unit(0.04, "npc")))

ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Media\\Images\\right_arrow.png'), 
       height = 648, width = 1280, units = 'px')




