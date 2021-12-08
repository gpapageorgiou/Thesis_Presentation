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
#font_import(paths = "./Custom_Fonts/")
#fonts()
#loadfonts(device = "win", quiet = TRUE)
#library(showtext)
#font_add(family = "ADAM.CG", regular = "./Custom_Fonts/ADAM.CG PRO.otf")
#font_add(family = 'FontAwesome', regular = './Custom_Fonts/FontAwesome.otf')
#showtext_auto()
library(facetscales)

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
                           'long_smooth_y' = long_smooth_data$y, 
                           'group' = rep(c("Hazard", "Longitudinal outcome"), each = 80), 
                           'panel' = rep(c("a", "b"), each = 80))

# create animation plot
# ggplot(plotdata, aes(time, y)) + facet_wrap(~ group, scales = "free_y", nrow = 2, strip.position = "right") + 
#   theme_bw() + theme(axis.title.y = element_blank(), axis.text = element_blank(), 
#                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                      panel.background = element_rect(fill = '#fcfffd'), 
#                      strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
#                      strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
#                      axis.line = element_line(colour = '#fcfffd'), 
#                      panel.border = element_rect(colour = '#fcfffd'),
#                      panel.spacing = unit(0, 'lines'),
#                      axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
#                      axis.ticks = element_blank())
# 
# 
# ggsave(filename = 'D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\JM_Classic\\White_BG\\JMclassic_Haz_1.png', 
#        height = 648, width = 1280, units = 'px')

sizes_h <- seq(0.1, 0.75, length.out = 80)
sizes = seq(0.25, 2.75, length.out = 15)
sizes = c(sizes, sizes, sizes, sizes)
sizes <- sizes[order(sizes)]

copy <- plotdata[plotdata$group == "Longitudinal outcome", ]

copies <- rbind(copy, copy, copy, copy)

copies <- copies[order(copies$time), ]

myscales <- list(`Hazard` = scale_y_continuous(limits = c(0, 1.009837), 
                                               expand = expansion(mult = c(0, 0.1))), 
                 `Longitudinal outcome` = scale_y_continuous(limits = c(0.1803262, 2.2000000), 
                                                             expand = expansion(mult = c(0.1, 0))))


p1 <- ggplot(plotdata, aes(time, y)) + facet_wrap(~ group, scales = 'free_y', nrow = 2, strip.position = "right") + 
  theme_bw() + theme(axis.title.y = element_blank(), axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#fcfffd'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_blank()) + 
  # geom_point(data = plotdata[plotdata$group == "Longitudinal outcome", ][1:i, ], 
  #            aes(x = time, y = y), fill = '#ea5455', color = '#ea5455', shape = '\U0001f494', 
  #            size = 4)
  geom_blank(data = plotdata[plotdata$group == "Longitudinal outcome", ]) + 
  xlim(0, 15) + 
  geom_line(data = new_plotdata[new_plotdata$group == 'Hazard', ][1:80, ], 
            aes(x = h_smooth_x, y = h_smooth_y), color = '#ea5455', 
            size = sizes_h[1:80]) + 
  geom_blank(data = new_plotdata[new_plotdata$group == "Hazard", ], 
             aes(x = h_smooth_x, y = h_smooth_y)) + 
  geom_line(data = new_plotdata[new_plotdata$group == 'Longitudinal outcome', ][1:80, ], 
            aes(x = long_smooth_x, y = long_smooth_y), color = '#1098f7', 
            size = sizes_h[1:80]) +
  facet_grid_sc(rows = vars(group), scales = list(y = myscales), margins = FALSE) 

# create animation plot
# ggplot(plotdata, aes(time, y)) + facet_wrap(~ group, scales = "free_y", nrow = 2, strip.position = "right") + 
#   theme_bw() + theme(axis.title.y = element_blank(), axis.text = element_blank(), 
#                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                      panel.background = element_rect(fill = '#fcfffd'), 
#                      strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
#                      strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
#                      axis.line = element_line(colour = '#fcfffd'), 
#                      panel.border = element_rect(colour = '#fcfffd'),
#                      panel.spacing = unit(0, 'lines'),
#                      axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
#                      axis.ticks = element_blank())
# 
# 
# ggsave(filename = 'D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\JM_Classic\\White_BG\\JMclassic_Haz_1.png', 
#        height = 648, width = 1280, units = 'px')

sizes_h <- seq(0.1, 0.75, length.out = 80)
sizes = seq(0.25, 2.75, length.out = 15)
sizes = c(sizes, sizes, sizes, sizes)
sizes <- sizes[order(sizes)]

intercept <- c(seq(0, 15, length.out = 80))

new_plotdata$intercept <- c(intercept, intercept)

myscales <- list(`Hazard` = scale_y_continuous(limits = c(0, 1.009837), 
                                               expand = expansion(mult = c(0, 0.1))), 
                 `Longitudinal outcome` = scale_y_continuous(limits = c(0.1803262, 2.2000000), 
                                                             expand = expansion(mult = c(0.1, 0))))

smoothdat <- ggplot_build(p1)$data[[4]]

#plotdata$x1 <- rep(c(NA, smoothdat[1, ]$x - 0.5), each = 15)
#plotdata$xend <- rep(c(NA, smoothdat[1, ]$x + 0.5), each = 15)
#plotdata$y1 <- rep(c(NA, smoothdat[1, ]$y + 0.05), each = 15)
#plotdata$yend <- rep(c(NA, smoothdat[2, ]$y + 0.04), each = 15)



y1 <- smoothdat$y 
yend <- y1

y1[1]
yend[1]
y1[1] <- y1[1] + 0.015
yend[1] <- yend[1] + 0.0125

y1[2]
yend[2]
y1[2] <- y1[2] + 0.015
yend[2] <- yend[2] + 0.015

y1[3]
yend[3]
y1[3] <- y1[3] + 0.03
yend[3] <- yend[3] + 0.005

y1[4]
yend[4]
y1[4] <- y1[4] + 0.03
yend[4] <- yend[4] + 0

y1[5]
yend[5]
y1[5] <- y1[5] + 0.03
yend[5] <- yend[5] + 0

y1[6]
yend[6]
y1[6] <- y1[6] + 0.035
yend[6] <- yend[6] - 0.005

y1[7]
yend[7]
y1[7] <- y1[7] + 0.04
yend[7] <- yend[7] - 0.01

y1[8]
yend[8]
y1[8] <- y1[8] + 0.045
yend[8] <- yend[8] - 0.015

y1[9]
yend[9]
y1[9] <- y1[9] + 0.0475
yend[9] <- yend[9] - 0.0175

y1[10]
yend[10]
y1[10] <- y1[10] + 0.0525
yend[10] <- yend[10] - 0.0225

y1[11]
yend[11]
y1[11] <- y1[11] + 0.0575
yend[11] <- yend[11] - 0.0275

y1[12]
yend[12]
y1[12] <- y1[12] + 0.0625
yend[12] <- yend[12] - 0.0325

y1[13]
yend[13]
y1[13] <- y1[13] + 0.0625
yend[13] <- yend[13] - 0.0325

y1[14]
yend[14]
y1[14] <- y1[14] + 0.065
yend[14] <- yend[14] - 0.035

y1[15] <- y1[15] + 0.0675
yend[15] <- yend[15] - 0.0375

y1[16] <- y1[16] + 0.07
yend[16] <- yend[16] - 0.0415

y1[17] <- y1[17] + 0.0715
yend[17] <- yend[17] - 0.0435

y1[18] <- y1[18] + 0.0785
yend[18] <- yend[18] - 0.0475

y1[19] <- y1[19] + 0.09
yend[19] <- yend[19] - 0.06

y1[20] <- y1[20] + 0.11
yend[20] <- yend[20] - 0.075

y1[21] <- y1[21] + 0.115
yend[21] <- yend[21] - 0.08

y1[22] <- y1[22] + 0.125
yend[22] <- yend[22] - 0.09

y1[23] <- y1[23] + 0.125
yend[23] <- yend[23] - 0.09

y1[24] <- y1[24] + 0.135
yend[24] <- yend[24] - 0.0975

y1[25] <- y1[25] + 0.135
yend[25] <- yend[25] - 0.0975

y1[26] <- y1[26] + 0.125
yend[26] <- yend[26] - 0.0955

y1[27] <- y1[27] + 0.09
yend[27] <- yend[27] - 0.125

y1[28] <- y1[28] + 0.085
yend[28] <- yend[28] - 0.115

y1[29] <- y1[29] + 0.075
yend[29] <- yend[29] - 0.1125

y1[30] <- y1[30] + 0.0725
yend[30] <- yend[30] - 0.110

y1[31] <- y1[31] + 0.070
yend[31] <- yend[31] - 0.105

y1[32] <- y1[32] + 0.070
yend[32] <- yend[32] - 0.105

y1[33] <- y1[33] + 0.070
yend[33] <- yend[33] - 0.105

y1[34] <- y1[34] + 0.070
yend[34] <- yend[34] - 0.105

y1[35] <- y1[35] + 0.070
yend[35] <- yend[35] - 0.105

y1[36] <- y1[36] + 0.070
yend[36] <- yend[36] - 0.105

y1[37] <- y1[37] + 0.0675
yend[37] <- yend[37] - 0.10

y1[38] <- y1[38] + 0.0645
yend[38] <- yend[38] - 0.0975

y1[39] <- y1[39] + 0.0575
yend[39] <- yend[39] - 0.0915

y1[40] <- y1[40] + 0.0525
yend[40] <- yend[40] - 0.085

y1[41] <- y1[41] + 0.048
yend[41] <- yend[41] - 0.079

y1[42] <- y1[42] + 0.045
yend[42] <- yend[42] - 0.077

y1[43] <- y1[43] + 0.0425
yend[43] <- yend[43] - 0.075

y1[44] <- y1[44] + 0.040
yend[44] <- yend[44] - 0.070

y1[45] <- y1[45] + 0.0375
yend[45] <- yend[45] - 0.065

y1[46] <- y1[46] + 0.030
yend[46] <- yend[46] - 0.06

y1[47] <- y1[47] + 0.025
yend[47] <- yend[47] - 0.055

y1[48] <- y1[48] + 0.02
yend[48] <- yend[48] - 0.05

y1[49] <- y1[49] + 0.01
yend[49] <- yend[49] - 0.045

y1[50] <- y1[50] + 0.0075
yend[50] <- yend[50] - 0.0375

y1[51] <- y1[51] + 0.0015
yend[51] <- yend[51] - 0.0275

y1[52] <- y1[52] - 0.008
yend[52] <- yend[52] - 0.02

y1[53] <- y1[53] - 0.012
yend[53] <- yend[53] - 0.02

y1[54] <- y1[54] - 0.0175
yend[54] <- yend[54] - 0.016

y1[55] <- y1[55] - 0.0175
yend[55] <- yend[55] - 0.016

y1[56] <- y1[56] - 0.02
yend[56] <- yend[56] - 0.01

y1[57] <- y1[57] - 0.0275
yend[57] <- yend[57] - 0.005

y1[58] <- y1[58] - 0.0325
yend[58] <- yend[58] - 0.00

y1[59] <- y1[59] - 0.04
yend[59] <- yend[59] + 0.005

y1[60] <- y1[60] - 0.05
yend[60] <- yend[60] + 0.0125

y1[61] <- y1[61] - 0.06
yend[61] <- yend[61] + 0.02

y1[62] <- y1[62] - 0.065
yend[62] <- yend[62] + 0.03

y1[63] <- y1[63] - 0.07
yend[63] <- yend[63] + 0.04

y1[64] <- y1[64] - 0.08
yend[64] <- yend[64] + 0.05

y1[65] <- y1[65] - 0.085
yend[65] <- yend[65] + 0.05

y1[66] <- y1[66] - 0.1
yend[66] <- yend[66] + 0.065

y1[67] <- y1[67] - 0.1
yend[67] <- yend[67] + 0.065

y1[68] <- y1[68] - 0.115
yend[68] <- yend[68] + 0.07

y1[69] <- y1[69] - 0.115
yend[69] <- yend[69] + 0.07

y1[70] <- y1[70] - 0.115
yend[70] <- yend[70] + 0.075

y1[71] <- y1[71] - 0.12
yend[71] <- yend[71] + 0.08

y1[72] <- y1[72] - 0.13
yend[72] <- yend[72] + 0.089

y1[73] <- y1[73] - 0.14
yend[73] <- yend[73] + 0.1

y1[74] <- y1[74] - 0.14
yend[74] <- yend[74] + 0.1

y1[75] <- y1[75] - 0.15
yend[75] <- yend[75] + 0.11

y1[76] <- y1[76] - 0.15
yend[76] <- yend[76] + 0.11

y1[77] <- y1[77] - 0.16
yend[77] <- yend[77] + 0.125

y1[78] <- y1[78] - 0.16
yend[78] <- yend[78] + 0.125

y1[79] <- y1[79] - 0.175
yend[79] <- yend[79] + 0.14

y1[80] <- y1[80] - 0.185
yend[80] <- yend[80] + 0.15


for (i in 1:length(intercept)) {
  new_plotdata$x1 <- rep(c(NA, intercept[i] - 0.4), each = 80)
  new_plotdata$y1 <- rep(c(NA, y1[i]), each = 80)
  new_plotdata$xend <- rep(c(NA, intercept[i] + 0.4), each = 80)
  new_plotdata$yend <- rep(c(NA, yend[i]), each = 80)
  ggplot(plotdata, aes(time, y)) + facet_wrap(~ group, scales = 'free_y', nrow = 2, strip.position = "right") + 
    theme_bw() + theme(axis.title.y = element_blank(), axis.text = element_blank(), 
                       panel.grid = element_blank(), 
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                       panel.background = element_rect(fill = '#fcfffd'), 
                       strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                       strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                       axis.line = element_line(colour = '#fcfffd'), 
                       panel.border = element_rect(colour = '#fcfffd'),
                       panel.spacing = unit(-0.5, 'lines'),
                       axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                       axis.ticks = element_blank()) + 
    # geom_point(data = plotdata[plotdata$group == "Longitudinal outcome", ][1:i, ], 
    #            aes(x = time, y = y), fill = '#ea5455', color = '#ea5455', shape = '\U0001f494', 
    #            size = 4)
    # geom_point(data = copies[1:60, ], 
    #            aes(x = time, y = y), color = '#1098f7', 
    #            size = sizes[1:60]) + 
    geom_line(data = new_plotdata[new_plotdata$group == 'Hazard', ][1:80, ], 
              aes(x = h_smooth_x, y = h_smooth_y), color = '#ea5455', 
              size = sizes_h[1:80]) + 
    geom_blank(data = new_plotdata[new_plotdata$group == "Hazard", ], 
               aes(x = h_smooth_x, y = h_smooth_y)) + 
    geom_line(data = new_plotdata[new_plotdata$group == 'Longitudinal outcome', ][1:80, ], 
              aes(x = long_smooth_x, y = long_smooth_y), color = '#1098f7', 
              size = sizes_h[1:80]) + 
    geom_segment(data = new_plotdata[new_plotdata$group == 'Hazard', ][i, ], 
                 aes(x = intercept, xend = intercept, y = 0, yend = h_smooth_y), 
                 color = '#2d4059') + 
    geom_point(data = new_plotdata[new_plotdata$group == 'Hazard', ][i, ], 
               aes(x = intercept, y = h_smooth_y), size = 2, alpha = 0.4, color = '#2d4059') + 
    geom_segment(data = new_plotdata[new_plotdata$group == 'Longitudinal outcome', ][i, ], 
                 aes(x = intercept, xend = intercept, y = long_smooth_y, yend = 2.2), 
                 color = '#2d4059') + 
    geom_segment(data = new_plotdata, aes(x = x1, y = y1, xend = xend, yend = yend), 
                 size = 0.2, arrow = arrow(length = unit(0.05, "npc")), 
                 alpha = 0.4, color = "#2d4059") +
    facet_grid_sc(rows = vars(group), scales = list(y = myscales)) + 
    geom_blank(data = plotdata[plotdata$group == "Longitudinal outcome", ])
  
  ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\JM_Classic\\White_BG\\assoc_slope_', i, '.png'), 
         height = 648, width = 1280, units = 'px')
}

paths <- rep(NA, 80)
for(i in 1:80) {
  paths[i] <- paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\JM_Classic\\White_BG\\assoc_slope_',
                     i, '.png')
}

gif_file <- gifski(paths, delay = 0.05, loop = FALSE, width = 1280, height = 648,
                   gif_file = 'D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\JM_Classic\\White_BG\\animation_assoc_slope.gif')

