##################
# UPDATE SIMULATION PLOTS
#---------------------------------
library(JMbayes)
library(ggplot2)
library(grid)
library(dplyr)
library(nlme)
library(gifski)
library(dplyr)
library(magrittr)
#library(emo)
library(ggplot2)
library(extrafont)
library(cowplot)
#library(emojifont)
library(purrr)


load(file = 'RData/overall_AUCs_PEs.RData')

# Create Plots
ggplot() + 
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                     axis.text.y = element_blank(),
                     axis.text.x = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#2d4059'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#fcfffd'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_blank(), 
                     legend.position = 'none') + 
  geom_boxplot(data = df.AUCs, aes(Method, AUC, color = Method), fill = '#2d4059', size = 0.25, 
               outlier.size = 0.1) + 
  facet_grid(Scenario ~ TH) + 
  scale_color_manual(values = c('Extrapolation' = '#ffb400', 'TD-Cox' = '#ea5455', 'WT' = '#1098f7'))


ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\PART_I\\AUCs.png'), 
       height = 648, width = 1280, units = 'px')

