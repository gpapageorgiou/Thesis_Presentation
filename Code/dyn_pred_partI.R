# ILLUSTARTION OF DYNAMIC PREDCITIONS UNDER DIFFERENT SCENARIOS
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

load(file = 'RData\\dyn_pred_adapt_plot_withpreds_jm3.RData')

sizes = seq(0.25, 1.75, length.out = 7)
sizes_l = seq(0.1, 0.5, length.out = 7)

pred_surv_reop <- list(NULL)
reop_times <- c(0.001, 0.5, 1, 2, 3, 4, 5, 6)

for (i in 1:length(reop_times)) {
  newdata_tmp <- newdata[1:7, ]
  newrow <- rep(NA, ncol(newdata_tmp))
  newrow <- newdata_tmp[nrow(newdata_tmp), ]
  newdata_tmp <- rbind(newdata_tmp, newrow)
  newdata_tmp[8, "time"] <- newdata_tmp[8, "time"] + reop_times[i]
  newdata_tmp[8, "reop.index"] <- 1
  newdata_tmp$reop.time <- newdata_tmp[8, "time"]
  newdata_tmp$time_relative <- pmax(newdata_tmp$time - newdata_tmp$reop.time, 0)
  newdata_tmp$Pulmonary_Gradient[8] <- NA
  pred_surv_reop[[i]] <- survfitJM(object = jm3, idVar = "idnr", newdata = newdata_tmp, 
                                   survTimes = seq(newdata_tmp$time[7], 27.5, 0.01), last.time = newdata_tmp$time[7])
}


for (i in 1:7) {
  ggplot() + 
    theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                       axis.text = element_blank(), 
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                       panel.background = element_rect(fill = '#fcfffd'), 
                       strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                       strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                       axis.line = element_line(colour = '#fcfffd'), 
                       panel.border = element_rect(colour = '#fcfffd'),
                       panel.spacing = unit(0, 'lines'),
                       axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                       axis.ticks = element_blank()) + 
    geom_point(aes(x = pred_surv[[i]]$obs.times$C043, y = pred_surv[[i]]$y$C043$Pulmonary_Gradient), 
               size = sizes[1:i], color = '#2d4059') + 
    geom_line(aes(x = pred_surv[[i]]$fitted.times$C043, y = pred_surv[[i]]$fitted.y$C043$Pulmonary_Gradient), 
              color = '#1098f7', size = sizes_l[1:i]) + 
    geom_vline(aes(xintercept = pred_surv[[i]]$last.time$C043), linetype = 2, color = '#ea5455', alpha = 1, size = 0.3) + 
    geom_line(aes(x = pred_surv[[i]]$summaries$C043[, 1], y = pred_surv[[i]]$summaries$C043[, 2] * 150), 
              color = '#2d4059', size = 0.4) + 
    geom_ribbon(aes(x = pred_surv[[i]]$summaries$C043[, 1], ymin = (pred_surv[[i]]$summaries$C043[, 4]) * 150, 
                    ymax = (pred_surv[[i]]$summaries$C043[, 5]) * 150), alpha = 0.2, fill = '#2d4059') +
    geom_line(aes(x = pred_surv[[i]]$summaries$C043[, 1], y = (pred_surv[[i]]$summaries$C043[, 4]) * 150), 
              color = '#2d4059', size = 0.4, linetype = 2) + 
    geom_line(aes(x = pred_surv[[i]]$summaries$C043[, 1], y = (pred_surv[[i]]$summaries$C043[, 5]) * 150), 
              color = '#2d4059', size = 0.4, linetype = 2) + 
    scale_y_continuous(limits = c(0, 150), sec.axis = sec_axis(~./150, name = "Event-Free Probability")) + 
    ylab('Pulmonary Gradient (mmHg)') + xlab('Time')
  
  ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\PART_I\\dyn_pred_', i, '.png'), 
         height = 648, width = 1280, units = 'px')
}


for (i in 1:length(reop_times)) {
  ggplot() + 
    theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                       axis.text = element_blank(), 
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                       panel.background = element_rect(fill = '#fcfffd'), 
                       strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                       strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                       axis.line = element_line(colour = '#fcfffd'), 
                       panel.border = element_rect(colour = '#fcfffd'),
                       panel.spacing = unit(0, 'lines'),
                       axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                       axis.ticks = element_blank()) + 
    geom_point(aes(x = pred_surv[[7]]$obs.times$C043, y = pred_surv[[7]]$y$C043$Pulmonary_Gradient), 
               size = sizes[1:7], color = '#2d4059') + 
    geom_line(aes(x = pred_surv[[7]]$fitted.times$C043, y = pred_surv[[7]]$fitted.y$C043$Pulmonary_Gradient), 
              color = '#1098f7', size = sizes_l[1:7]) + 
    geom_vline(aes(xintercept = pred_surv[[7]]$last.time$C043), linetype = 2, color = '#ea5455', alpha = 1, size = 0.3) + 
    geom_line(aes(x = pred_surv[[7]]$summaries$C043[, 1], y = pred_surv[[7]]$summaries$C043[, 2] * 150), 
              color = '#2d4059', size = 0.4) + 
    geom_ribbon(aes(x = pred_surv[[7]]$summaries$C043[, 1], ymin = (pred_surv[[7]]$summaries$C043[, 4]) * 150, 
                    ymax = (pred_surv[[7]]$summaries$C043[, 5]) * 150), alpha = 0.2, fill = '#2d4059') +
    geom_line(aes(x = pred_surv[[7]]$summaries$C043[, 1], y = (pred_surv[[7]]$summaries$C043[, 4]) * 150), 
              color = '#2d4059', size = 0.4, linetype = 2) + 
    geom_line(aes(x = pred_surv[[7]]$summaries$C043[, 1], y = (pred_surv[[7]]$summaries$C043[, 5]) * 150), 
              color = '#2d4059', size = 0.4, linetype = 2) + 
    geom_line(aes(x = pred_surv_reop[[i]]$summaries$C043[, 1], y = pred_surv_reop[[i]]$summaries$C043[, 2] * 150), 
              color = '#1098f7', size = 0.4) + 
    geom_ribbon(aes(x = pred_surv_reop[[i]]$summaries$C043[, 1], ymin = (pred_surv_reop[[i]]$summaries$C043[, 4]) * 150, 
                    ymax = (pred_surv_reop[[i]]$summaries$C043[, 5]) * 150), alpha = 0.5, fill = '#1098f7') + 
    geom_vline(aes(xintercept = pred_surv_reop[[i]]$last.time + reop_times[i]), linetype = 2, 
               color = '#1098f7', size = 0.3) + 
    # geom_line(aes(x = pred_surv_reop[[i]]$summaries$C043[, 1], y = (pred_surv_reop[[i]]$summaries$C043[, 4]) * 150), 
    #           color = '#1098f7', size = 0.75, linetype = 2) + 
    # geom_line(aes(x = pred_surv_reop[[i]]$summaries$C043[, 1], y = (pred_surv_reop[[i]]$summaries$C043[, 5]) * 150), 
    #           color = '#1098f7', size = 0.75, linetype = 2) +
    scale_y_continuous(limits = c(0, 150), sec.axis = sec_axis(~./150, name = "Event-Free Probability")) + 
    ylab('Pulmonary Gradient (mmHg)') + xlab('Time')
  
  ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\PART_I\\dyn_pred_', i + 7, '.png'), 
         height = 648, width = 1280, units = 'px')
}

paths <- rep(NA, 15)
for(i in 1:15) {
  paths[i] <- paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\PART_I\\dyn_pred_', 
                     i, '.png')
}

gif_file <- gifski(paths, delay = 1.25, loop = FALSE, width = 1280, height = 648, 
                   gif_file = 'D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\PART_I\\dyn_pred.gif')




