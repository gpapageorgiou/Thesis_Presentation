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
library(gridExtra)
library(cowplot)

load(file = 'RData\\shrinkage_fits.RData')

results <- data.frame("Shrinkage" = rep(c("No", "HS", "R"), each = 18), 
                      "Alphas" = NA,
                      "Alphas_Low" = NA,
                      "Alphas_High" = NA,
                      "Outcome" = rep(c("log(TotalBilirubin)", "log(Creatinine)"), each = 9),
                      "Association" = rep(rep(c("bold(Value)", "bold(Slope)", 
                                                "bold(Area)"), each = 3), 2), 
                      "Transition" = rep(rep(c("bold(HF %->% Complication)", 
                                               "bold(HF %->% Death)", 
                                               "bold(Complication %->% Death)"), 3), 2))


results$Alphas <- c(fit_mstate_4$statistics$postMeans$alphas, 
                    fit_mstate_4_HS$statistics$postMeans$alphas, 
                    fit_mstate_4_R$statistics$postMeans$alphas)

cis <- rbind(t(fit_mstate_4$statistics$CIs$alphas), 
             t(fit_mstate_4_HS$statistics$CIs$alphas), 
             t(fit_mstate_4_R$statistics$CIs$alphas))

results[, c("Alphas_Low", "Alphas_High")] <- cis

results$Shrinkage <- factor(results$Shrinkage, levels = c("No", "HS", "R"))
results$Transition <- factor(results$Transition, levels = c("bold(HF %->% Complication)", 
                                                            "bold(HF %->% Death)", 
                                                            "bold(Complication %->% Death)"))
results$Association <- factor(results$Association, 
                              levels = c("bold(Value)", "bold(Slope)", 
                                         "bold(Area)"))

results$Shrinkage <- factor(results$Shrinkage, 
                            levels = levels(results$Shrinkage)[c(3, 2, 1)])

labeller_fun <- function(labels, multi_line = TRUE) {
  labels <- label_value(labels, multi_line = multi_line)
  #labels$Outcome <- c("", "log(TotalBilirubin)", "")
  if (multi_line) {
    lapply(unname(labels), lapply, function(values) {
      print(values)
      c(parse(text = as.character(values)))
    })
  }
  else {
    lapply(labels, function(values) {
      values <- paste0("list(", values, ")")
      lapply(values, function(expr) c(parse(text = expr)))
    })
  }
}

########### DENSITY PLOTS
n.models <- 3
n.mcmc <- 1000

results_dens <- data.frame("Shrinkage" = rep(c("No", "HS", "R"), 
                                             each = length(fit_mstate_4$mcmc$alphas)), 
                           "Alphas" = NA,
                           "Outcome" = rep(c("log(TotalBilirubin)", "log(Creatinine)"), 
                                           each = length(fit_mstate_4$mcmc$alphas)/
                                             length(fit_mstate_4$model_info$families)),
                           "Association" = rep(rep(c("bold(Value)", "bold(Slope)", 
                                                     "bold(Area)"), 
                                                   each = n.mcmc*length(fit_mstate_4$control$knots)), 
                                               length(fit_mstate_4$model_info$families)), 
                           "Transition" = rep(rep(c("bold(HF %->% Complication)", 
                                                    "bold(HF %->% Death)", 
                                                    "bold(Complication %->% Death)"), 
                                                  each = n.mcmc), 
                                              n.models))

results_dens$Alphas <- c(c(fit_mstate_4$mcmc$alphas), 
                         c(fit_mstate_4_HS$mcmc$alphas), 
                         c(fit_mstate_4_R$mcmc$alphas))

results_dens$Shrinkage <- factor(results_dens$Shrinkage, levels = c("No", "HS", "R"))
results_dens$Transition <- factor(results_dens$Transition, levels = c("bold(HF %->% Complication)", 
                                                                      "bold(HF %->% Death)", 
                                                                      "bold(Complication %->% Death)"))
results_dens$Association <- factor(results_dens$Association, 
                                   levels = c("bold(Value)", "bold(Slope)", 
                                              "bold(Area)"))

results_dens$Shrinkage <- factor(results_dens$Shrinkage, 
                                 levels = levels(results_dens$Shrinkage)[c(3, 2, 1)])

###################################
# LUMC PRESENTATION
##################################

g1 <- ggplot() + geom_density(data = results_dens[results_dens$Outcome == "log(TotalBilirubin)" & results_dens$Association == 'bold(Value)', ], 
                              aes(x = Alphas, color = Shrinkage, fill = Shrinkage, alpha = Shrinkage), 
                              size = 0.1) + 
  #geom_vline(data = results_dens[results_dens$Outcome == "log(TotalBilirubin)", ], 
  #aes(xintercept = Alphas), size = 1.2, color = "#9E1F33") + 
  facet_grid( ~ Transition, labeller = labeller_fun, scales = 'free_x') + 
  #geom_vline(xintercept = 0, color = "#4C59AD", linetype = 2) + 
  scale_color_manual(values = c("No" = "#2d4059", "HS" = "#1098f7", "R" = "#ea5455")) + 
  scale_fill_manual(values = c("No" = "#2d4059", "HS" = "#1098f7", "R" = "#ea5455")) + 
  scale_alpha_manual(values = c("No" = 0.2, "HS" = 0.2, "R" = 0.2)) + 
  theme_bw() + theme(axis.title.y =  element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#fcfffd'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_blank(), 
                     legend.text =  element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     legend.position = 'none') +
  xlab("") + ylab("Value")

g2 <- ggplot() + geom_density(data = results_dens[results_dens$Outcome == "log(TotalBilirubin)" & results_dens$Association == 'bold(Slope)', ], 
                              aes(x = Alphas, color = Shrinkage, fill = Shrinkage, alpha = Shrinkage), 
                              size = 0.1) + 
  #geom_vline(data = results_dens[results_dens$Outcome == "log(TotalBilirubin)", ], 
  #aes(xintercept = Alphas), size = 1.2, color = "#9E1F33") + 
  facet_grid( ~ Transition, labeller = labeller_fun, scales = 'free_x') + 
  #geom_vline(xintercept = 0, color = "#4C59AD", linetype = 2) + 
  scale_color_manual(values = c("No" = "#2d4059", "HS" = "#1098f7", "R" = "#ea5455")) + 
  scale_fill_manual(values = c("No" = "#2d4059", "HS" = "#1098f7", "R" = "#ea5455")) + 
  scale_alpha_manual(values = c("No" = 0.2, "HS" = 0.2, "R" = 0.2)) + 
  theme_bw() + theme(axis.title.y =  element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#fcfffd'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_blank(), 
                     legend.text =  element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     legend.position = 'none') + 
  xlab("") + ylab("Slope")

g3 <- ggplot() + geom_density(data = results_dens[results_dens$Outcome == "log(TotalBilirubin)" & results_dens$Association == 'bold(Area)', ], 
                              aes(x = Alphas, color = Shrinkage, fill = Shrinkage, alpha = Shrinkage), 
                              size = 0.1) + 
  #geom_vline(data = results_dens[results_dens$Outcome == "log(TotalBilirubin)", ], 
  #aes(xintercept = Alphas), size = 1.2, color = "#9E1F33") + 
  facet_grid( ~ Transition, labeller = labeller_fun, scales = 'free_x') + 
  #geom_vline(xintercept = 0, color = "#4C59AD", linetype = 2) + 
  scale_color_manual(values = c("No" = "#2d4059", "HS" = "#1098f7", "R" = "#ea5455")) + 
  scale_fill_manual(values = c("No" = "#2d4059", "HS" = "#1098f7", "R" = "#ea5455")) + 
  scale_alpha_manual(values = c("No" = 0.2, "HS" = 0.2, "R" = 0.2)) + 
  theme_bw() + theme(axis.title.y =  element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#fcfffd'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_blank(), 
                     legend.text =  element_text(colour = '#2d4059', size = 3, family = 'ADAM.CG PRO'), 
                     legend.position = 'bottom', 
                     legend.key.size = unit(0.5, 'lines'), 
                     legend.title = element_text(colour = '#2d4059', size = 3, family = 'ADAM.CG PRO')) + 
  xlab("") + ylab("Area")

g4 <- plot_grid(g1, g2, g3, align = 'v', rel_heights = c(1/3.25, 1/3.25, 1.25/3), nrow = 3,
                scale = 1)

ggsave(filename = "D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\shrinkage_fits.png", g4, 
       height = 648, width = 1280, units = 'px', dpi = 220)
