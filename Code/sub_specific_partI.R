### LOAD dataset
load(file = "D:\\gpapageorgiou\\Github_Repos\\Thesis\\Memoir_Latex\\R_CODE\\PART_I\\Datasets\\PulmonaryGradDataset.RData")

### LIBRARIES
library(nlme)
library(lattice)
library(latticeExtra)
library(survival)
library(JMbayes)
library(gifski)
library(dplyr)
library(magrittr)
#library(emo)
library(ggplot2)
library(extrafont)
library(cowplot)
#library(emojifont)
library(purrr)


paper.ids.2 <- c("C043", "C002", "C522", "C146")

join.dat.3 <- join_full_final[join_full_final$idnr %in% paper.ids.2, ]

join.dat.3$Status <- NA
join.dat.3$Reoperation_Status <- NA
join.dat.3$Event_Status <- NA

join.dat.3[join.dat.3$idnr == "C043", "Reoperation_Status"] <- "Reoperated"
join.dat.3[join.dat.3$idnr == "C002", "Reoperation_Status"] <- "Reoperated"
join.dat.3[join.dat.3$idnr == "C522", "Reoperation_Status"] <- "Not reoperated"
join.dat.3[join.dat.3$idnr == "C146", "Reoperation_Status"] <- "Not reoperated"

join.dat.3[join.dat.3$idnr == "C522", "reop.time"] <- NA
join.dat.3[join.dat.3$idnr == "C146", "reop.time"] <- NA


join.dat.3[join.dat.3$idnr == "C043", "Event_Status"] <- "Deceased"
join.dat.3[join.dat.3$idnr == "C002", "Event_Status"] <- "Censored"
join.dat.3[join.dat.3$idnr == "C522", "Event_Status"] <- "Deceased"
join.dat.3[join.dat.3$idnr == "C146", "Event_Status"] <- "Censored"

library(ggplot2)

ggplot() + facet_wrap(~ group, scales = 'free_y', nrow = 2, strip.position = "right") + 
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#2d4059'), 
                     panel.border = element_rect(colour = '#2d4059'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO')) + 
  facet_grid(Event_Status ~ Reoperation_Status) + 
  geom_point(data = join.dat.3, aes(x = time, y = Pulmonary_Gradient), size = 1, color = '#2d4059') + 
  geom_vline(join.dat.3, mapping = aes(xintercept = reop.time), linetype = "dashed", color = '#ea5455') + 
  ylab("Pulmonary Gradient (mmHg)") + xlab("Years since RVOT")

ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\PART_I\\suspecific_1.png'), 
       height = 648, width = 1280, units = 'px')

