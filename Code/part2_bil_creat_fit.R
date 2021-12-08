load(file = '.\\RData\\results.RData')

library(splines)
library(ggplot2)
library(gridExtra)

newdata <- expand.grid("time_years" = seq(0, 0.75, length.out = 500), 
                       "implant_time_years" = c(1e+5, 0.25), 
                       "BMI" = median(newdat.id2$BMI), 
                       "age" = median(newdat.id$age))

newdata$time_relative <- pmax(0, newdata$time_years - newdata$implant_time_years)
newdata$LVAD <- ifelse(newdata$implant_time_years < 100, "Yes", "No")

plotdata <- effectPlot_mvJointModelBayes(fit_mstate_1_horse_rel, newdata, newdat)

plotdata$time_years <- plotdata$time_years * 365
plotdata$implant_time_years <- plotdata$implant_time_years * 365

ggplot(plotdata, aes(time_years, pred_1)) + 
  geom_line(data = plotdata[plotdata$LVAD == "Yes", ], 
            aes(time_years, pred_1, color = "Yes"), size = 0.4) + 
  geom_line(data = plotdata[plotdata$LVAD == "No", ], 
            aes(time_years, pred_1, color = "No"), size = 0.4) + 
  geom_vline(aes(xintercept = min(plotdata$implant_time_years), 
                 linetype = 'LVAD implantation'), alpha = 1, size = 0.4, color = '#1098f7') + 
  geom_ribbon(data = plotdata[plotdata$LVAD == "Yes", ], 
              aes(time_years, ymin = low_1, ymax = upp_1, fill = "Yes", alpha = "Yes")) + 
  geom_ribbon(data = plotdata[plotdata$LVAD == "No", ], 
              aes(time_years, ymin = low_1, ymax = upp_1, fill = "No", alpha = "No")) + 
  ylim(-2, 2) + scale_x_continuous(limits = c(0, 250), expand = c(0.005, 0.005)) +
  scale_color_manual("LVAD", values = c("Yes" = "#1098f7", "No" = "#2d4059")) + 
  scale_fill_manual("LVAD", values = c("Yes" = "#1098f7", "No" = "#2d4059")) + 
  scale_alpha_manual("LVAD", values = c("Yes" = 0.2, "No" = 0.2)) + 
  scale_linetype_manual("", values = c(2)) +
  ylab("log(Total Bilirubin)") + xlab("Time (in days)") + 
  theme_bw() + 
  theme(panel.spacing = unit(0, "lines"), legend.position = 'bottom', 
        axis.title.x = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
        axis.title.y = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
        legend.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'),
        legend.key.size = unit(0.25, units = 'cm'), 
        legend.title = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
        axis.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\part2_bilir_fit.png'), 
       height = 600, width = 1280, units = 'px')




ggplot(plotdata, aes(time_years, pred_2)) + 
  geom_line(data = plotdata[plotdata$LVAD == "Yes", ], 
            aes(time_years, pred_2, color = "Yes"), size = 0.4) + 
  geom_line(data = plotdata[plotdata$LVAD == "No", ], 
            aes(time_years, pred_2, color = "No"), size = 0.4) + 
  geom_vline(aes(xintercept = min(plotdata$implant_time_years), 
                 linetype = 'LVAD implantation'), alpha = 1, size = 0.4, color = '#1098f7') + 
  geom_ribbon(data = plotdata[plotdata$LVAD == "Yes", ], 
              aes(time_years, ymin = low_2, ymax = upp_2, fill = "Yes", alpha = "Yes")) + 
  geom_ribbon(data = plotdata[plotdata$LVAD == "No", ], 
              aes(time_years, ymin = low_2, ymax = upp_2, fill = "No", alpha = "No")) + 
  ylim(-2, 2) + scale_x_continuous(limits = c(0, 250), expand = c(0.005, 0.005)) +
  scale_color_manual("LVAD", values = c("Yes" = "#1098f7", "No" = "#2d4059")) + 
  scale_fill_manual("LVAD", values = c("Yes" = "#1098f7", "No" = "#2d4059")) + 
  scale_alpha_manual("LVAD", values = c("Yes" = 0.2, "No" = 0.2)) + 
  scale_linetype_manual("", values = c(2)) +
  ylab("log(Creatinine)") + xlab("Time (in days)") + 
  theme_bw() + 
  theme(panel.spacing = unit(0, "lines"), legend.position = 'bottom', 
        axis.title.x = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
        axis.title.y = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
        legend.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'),
        legend.key.size = unit(0.25, units = 'cm'), 
        legend.title = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
        axis.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\part2_creat_fit.png'), 
       height = 600, width = 1280, units = 'px')

