# LOAD PACKAGES
library(JM)
library(nlme)
library(survival)
library(ggplot2)
library(splines)
library(JMbayes)

# Attach data
aids <- aids
aids.id <- aids.id

##########################
# DATA PREPARATION
##########################

# Remove unnecessary columns form the dataset
aids <- aids[, !colnames(aids) %in% c("start", "stop", "event")]
aids.id <- aids.id[, !colnames(aids.id) %in% c("start", "stop", "event")]

# source function mis_pattern
devtools::source_url('https://raw.githubusercontent.com/gpapageorgiou/DropOut_WorkInprog/master/R/mis_pattern.R', 
                     sha1 = "bd1c8e6d6266bc5a967d588add9931d606e3eb24")

# split the dataset by id
aids.split <- split(aids, aids$patient)

# lapply mis_pattern to aids.split
aids.split <- lapply(aids.split, mis_pattern)

# bind aids_split back
aids_new <- do.call(rbind, aids.split)

# create id version of new dataset
aids_new.id <- aids_new[!duplicated(aids_new$patient), ]

# create new variable mistype
aids_new$mistype <- factor(aids_new$pattern)
aids_new$mistype <- factor(aids_new$mistype, levels = levels(aids_new$mistype), 
                           labels = c('Completer', 'Dropout', 'Intermittent', 
                                      'Dropout', 'Intermittent', 'Intermittent', 
                                      'Intermittent', 'Dropout', 'Intermittent', 
                                      'Intermittent', 'Intermittent', 'Intermittent', 
                                      'Intermittent', 'Dropout'))
# recreate id version
aids_new.id <- aids_new[!duplicated(aids_new$patient), ]

# remove intermittent missingness
aids_new <- aids_new[aids_new$mistype %in% c('Completer', 'Dropout'), ]
aids_new$mistype <- factor(aids_new$mistype)
aids_new$patient <- factor(aids_new$patient)
aids_new.id <- aids_new[!duplicated(aids_new$patient), ]

# create dropout and dTime variable
aids_new.split <- split(aids_new, aids_new$patient)
d_outcomes <- function(dat.id) {
  if (any(dat.id$mistype %in% 'Completer')) {
    dat.id$dTime <- 18
    dat.id$d <- 0 
  } else {
    dTime <- max(dat.id$obstime)
    dat.id$dTime <- dTime
    dat.id$d <- 1
  }
  dat.id
}

# SPLIT DATASET BY ID AND APPLY D_outcomes to create new dropout time and event indicators
aids_new.split <- lapply(aids_new.split, d_outcomes)

aids_new <- do.call(rbind, aids_new.split)

aids_new.id <- aids_new[!duplicated(aids_new$patient), ]

aids_new.id2 <- aids_new.id

aids_new.id2$d <- ifelse(aids_new.id$death == 1, 1, 0)
#------------------------------------------------------------------------------------------

#############
# MODELS
#############

# BAYESIAN LINEAR MIXED-EFFECTS MODEL
mv_lme_model <- mvglmer(list(sqrt(CD4) ~ obstime * drug + (obstime | patient)), 
                        data = aids_new, families = list(gaussian))

# Mixed-effects model using nlme to use in fitting joint model
lme_model_1 <- lme(sqrt(CD4) ~ obstime * drug, 
                   random = ~ obstime | patient, 
                   data = aids_new, 
                   control = list(opt = 'optim'))

# Time-to-dropout model for Scenario 1 MNAR model (all cases MNAR)
cox_d <- coxph(Surv(dTime, d) ~ drug, data = aids_new.id, model = TRUE, x = TRUE)
jm_d_bayes <- jointModelBayes(lmeObject = lme_model_1, survObject = cox_d, timeVar = 'obstime', 
                              control = list(lng.in.kn = 3, ObsTimes.knots = TRUE))

# Time-to-dropout model for Scenario 2 MNAR model (cases MNAR or MAR depending on cause of dropout)
cox_d2 <- coxph(Surv(dTime, d) ~ drug, data = aids_new.id2, model = TRUE, x = TRUE)
jm_d2_bayes <- jointModelBayes(lmeObject = lme_model_1, survObject = cox_d2, timeVar = 'obstime', 
                               control = list(lng.in.kn = 3, ObsTimes.knots = TRUE))


library(JM)
library(kableExtra)
library(knitr)
library(ggplot2)
library(gridExtra)

ranef_jm_d <- jm_d_bayes$postMeans$b
ranef_lme <- mv_lme_model$postMeans$b
ranef_jm_d2 <- jm_d2_bayes$postMeans$b

g1 <- ggplot() + geom_point(aes(x = ranef_jm_d[, 1], y = ranef_lme[, 1]), color = '#2d4059', size = 0.4) +  
  geom_smooth(aes(x = ranef_jm_d[, 1], y = ranef_lme[, 1]), color = '#ea5455', se = F, size = 0.2) + 
  geom_abline(aes(intercept = 0, slope = 1), color = '#1098f7', alpha = 0.8, size = 0.2) +
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#2d4059'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_line(colour = '#2d4059')) +
  ylab('Random Intercepts (MNAR)') + 
  xlab('Random Intercepts (MAR)') + 
  ylim(-2.5, 2.5) + xlim(-2.5, 2.5)
g2 <- ggplot() + geom_point(aes(x = ranef_jm_d[, 2], y = ranef_lme[, 2]), color = '#2d4059', size = 0.4) + 
  geom_smooth(aes(x = ranef_jm_d[, 2], y = ranef_lme[, 2]), color = '#ea5455', se = F, size = 0.2) + 
  geom_abline(aes(intercept = 0, slope = 1), color = '#1098f7', alpha = 0.8, size = 0.2) +
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#2d4059'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_line(colour = '#2d4059')) +
  ylab('Random Slopes (MNAR)') + 
  xlab('Random Slopes (MAR)') + 
  ylim(-0.5, 0.5) + xlim(-0.5, 0.5)


g1 <- ggplotGrob(g1)
g2 <- ggplotGrob(g2)

fplot <- gridExtra::arrangeGrob(g1, g2, ncol = 2)

ggsave(filename = "D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\part_iii_a.png", 
       fplot, height = 648, width = 1280, units = 'px')

g4 <- ggplot() + geom_point(aes(x = ranef_jm_d2[, 1], y = ranef_lme[, 1]), color = '#2d4059', size = 0.4) +  
  geom_smooth(aes(x = ranef_jm_d2[, 1], y = ranef_lme[, 1]), color = '#ea5455', se = F, size = 0.2) + 
  geom_abline(aes(intercept = 0, slope = 1), color = '#1098f7', alpha = 0.8, size = 0.2) +
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#2d4059'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_line(colour = '#2d4059')) +
  ylab('Random Intercepts (MNAR)') + 
  xlab('Random Intercepts (MAR)') + 
  ylim(-2.5, 2.5) + xlim(-2.5, 2.5)
g5 <- ggplot() + geom_point(aes(x = ranef_jm_d2[, 2], y = ranef_lme[, 2]), color = '#2d4059', size = 0.4) + 
  geom_smooth(aes(x = ranef_jm_d2[, 2], y = ranef_lme[, 2]), color = '#ea5455', se = F, size = 0.2) + 
  geom_abline(aes(intercept = 0, slope = 1), color = '#1098f7', alpha = 0.8, size = 0.2) +
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#2d4059'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_line(colour = '#2d4059')) +
  ylab('Random Slopes (MNAR)') + 
  xlab('Random Slopes (MAR)') + 
  ylim(-0.5, 0.5) + xlim(-0.5, 0.5)

g4 <- ggplotGrob(g4)
g5 <- ggplotGrob(g5)

fplot2 <- gridExtra::arrangeGrob(g4, g5, ncol = 2)

ggsave(filename = "D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\part_iii_b.png", 
       fplot2, height = 648, width = 1280, units = 'px')

g6 <- ggplot() + geom_point(aes(x = ranef_jm_d[, 1], y = ranef_jm_d2[, 1]), color = '#2d4059', size = 0.4) +  
  geom_smooth(aes(x = ranef_jm_d[, 1], y = ranef_jm_d2[, 1]), color = '#ea5455', se = F, size = 0.2) + 
  geom_abline(aes(intercept = 0, slope = 1), color = '#1098f7', alpha = 0.8, size = 0.2) +
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#2d4059'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_line(colour = '#2d4059')) +
  ylab('Random Intercepts (MNAR 1st Scenario)') + 
  xlab('Random Intercepts (MNAR 2nd Scenario)') + 
  ylim(-2.5, 2.5) + xlim(-2.5, 2.5)
g7 <- ggplot() + geom_point(aes(x = ranef_jm_d[, 2], y = ranef_jm_d2[, 2]), color = '#2d4059', size = 0.4) + 
  geom_smooth(aes(x = ranef_jm_d[, 2], y = ranef_jm_d2[, 2]), color = '#ea5455', se = F, size = 0.2) + 
  geom_abline(aes(intercept = 0, slope = 1), color = '#1098f7', alpha = 0.8, size = 0.2) +
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#fcfffd'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#2d4059'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_line(colour = '#2d4059')) +
  ylab('Random Slopes (MNAR 1st Scenario)') + 
  xlab('Random Slopes (MNAR 2nd Scenario)') + 
  ylim(-0.5, 0.5) + xlim(-0.5, 0.5)


g6 <- ggplotGrob(g6)
g7 <- ggplotGrob(g7)

fplot3 <- gridExtra::arrangeGrob(g6, g7, ncol = 2)

ggsave(filename = "D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\part_iii_c.png", 
       fplot3, height = 648, width = 1280, units = 'px')