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

library(nlme)
library(lattice)
library(matrixcalc)
library(Matrix)
library(MASS)
library(survival)
library(ggplot2)
library(JMbayes)

set.seed(567)

# specify number of unique subjects
N <- 1000

# set number of measurements per subject
n <- 10

# create vector of ids
id <- rep(1:N, each = n)

# set time limits
min.time <- 0
max.time <- 10

# Sample time-points of measurements
time <- NULL

for (i in 1:N) {
  tmp <- c(0, sort(runif(n - 1, min = min.time, max = max.time)))
  time <- c(time, tmp)
}

# start data frame
simdat <- data.frame("id" = id, "time" = time)

X1 <- model.matrix(~ 1 + time + I(time^2) + I(time^3), data = simdat)
Z1 <- model.matrix(~ 1 + time, data = simdat)

true.betas <- c(12.457, 0.656, 0.218, -0.0465)

D <- matrix(NA, ncol = 2, nrow = 2)
D[1, 1] <- 5.687
D[2, 2] <- 2.657
D[2, 1] <- D[1, 2] <- 1.657
D <- as.matrix(nearPD(D)$mat)

b <- mvrnorm(n = N, mu = rep(0, ncol(D)), D)

eta.y <- as.vector(X1 %*% true.betas + rowSums(Z1 * b[id, ])) 

sigma.e <- 2.3678

y <- rnorm(n * N, mean = eta.y, sd = sigma.e)

simdat$y <- y

alpha <- 0.5878

phi <- exp(1.4890)

gammas <- c("(Intercept)" = -24.1442)

W <- cbind("(Intercept)1"= rep(1, N))

eta.t <- as.vector(W %*% gammas)

invS <- function(t, u, i) {
  h <- function(s) {
    XX <- cbind(1, s, s^2, s^3)
    ZZ <- cbind(1, s)
    f1 <- as.vector(XX %*% true.betas + rowSums(ZZ * b[rep(i, nrow(ZZ)), ]))
    exp(log(phi) + phi*log(s) + eta.t[i] + f1*alpha)
  }
  integrate(h, lower = 0, upper = t, subdivisions = 10000)$value + log(u)
}

u <- runif(N, 0, 1)
trueT <- numeric(N)

for (i in 1:N) {
  Up <- 50
  tries <- 5
  Root <- try(uniroot(invS, interval = c(1e-05, Up), u = u[i], i = i)$root, TRUE)
  while(inherits(Root, "try-error") && tries > 0) {
    tries <- tries - 1
    Up <- Up + 200
    Root <- try(uniroot(invS, interval = c(1e-05, Up), u = u[i], i = i)$root, TRUE)
  }
  trueT[i] <- if (!inherits(Root, "try-error")) Root else NA
}

na.ind <- !is.na(trueT)
trueT <- trueT[na.ind]
W <- W[na.ind, , drop = FALSE]
long.na.ind <- rep(na.ind, each = n)
simdat <- simdat[long.na.ind, ]
N <- length(trueT)
simdat$Ctime <- NA
simdat$keepMAR <- NA

simdat.split <- split(simdat, simdat$id)

keep_MAR <- function(x) {
  indxs <- which(x$y > 22)
  if (length(indxs) > 0) {
    x$Ctime <- x[min(indxs), 'time']
    x$keepMAR <- rep(c(1, 0), times = c(length(1:min(indxs)), nrow(x) - length(1:min(indxs))))
  } else {
    x$Ctime <- tail(x$time, n = 1L)
    x$keepMAR <- 1
  }
  x
}

simdat.split <- lapply(simdat.split, keep_MAR)
simdat_2 <- do.call(rbind, simdat.split)

n_per_subj <- tapply(simdat_2$id, simdat_2$id, length)

simdat_2$trueT <- rep(trueT, times = n_per_subj)
simdat_2$Time <- pmin(simdat_2$Ctime, simdat_2$trueT)
simdat_2$event <- as.numeric(simdat_2$trueT < simdat_2$Ctime)
simdat_2$keep <- ifelse(simdat_2$time < simdat_2$Time, 1, 0)

mix_all <- lme(y ~ time + I(time^2) + I(time^3), 
               random = ~ time | id, 
               data = simdat_2, control = list(opt = "optim"))

simdat_3 <- simdat_2[simdat_2$keep == 1, ]

mix_MAR <- lme(y ~ time + I(time^2) + I(time^3), 
               random = ~ time | id, 
               data = simdat_3, control = list(opt = "optim"))


effectPlotData <- function (object, newdata, orig_data) {
  form <- formula(object)
  namesVars <- all.vars(form)
  betas <- if (!inherits(object, "lme")) coef(object) else fixef(object)
  V <- if (inherits(object, "geeglm")) object$geese$vbeta else vcov(object)
  orig_data <- orig_data[complete.cases(orig_data[namesVars]), ]
  Terms <- delete.response(terms(form))
  mfX <- model.frame(Terms, data = orig_data)
  Terms_new <- attr(mfX, "terms")
  mfX_new <- model.frame(Terms_new, newdata, xlev = .getXlevels(Terms, mfX))
  X <- model.matrix(Terms_new, mfX_new)
  pred <- c(X %*% betas)
  ses <- sqrt(diag(X %*% V %*% t(X)))
  newdata$pred <- pred
  newdata$low <- pred - 1.96 * ses
  newdata$upp <- pred + 1.96 * ses
  newdata
}

last_rows <- tapply(rownames(simdat_3), simdat_3$id, tail, n = 1L)
simdat_3.id <- simdat_3[rownames(simdat_3) %in% last_rows, ]

coxmod <- coxph(Surv(Time, event) ~ 1, data = simdat_3.id, x = TRUE, model = TRUE)
jm_MAR_MNAR <- jointModelBayes(mix_MAR, coxmod, timeVar = 'time')

effectPlot_JointModelBayes <- function (JMbayesObject, newdata, orig_data) {
  betas <- JMbayesObject$mcmc$betas  
  Terms <- JMbayesObject$Terms$termsYx
  form <- formula(Terms)
  Terms <- delete.response(form)
  mfX <- model.frame(Terms, data = orig_data)
  Terms_new <- delete.response(attr(mfX, "terms"))
  mfX_new <- model.frame(Terms_new, data = newdata)
  X <- model.matrix(Terms_new, mfX_new)
  pred <- betas %*% t(X)
  pred_f <- apply(pred, 2, mean)
  low <- apply(pred, 2, quantile, probs = 0.025)
  upp <- apply(pred, 2, quantile, probs = 0.975)
  newdata$pred <- pred_f
  newdata$low <- low
  newdata$upp <- upp
  newdata
}

newdata <- data.frame('time' = rep(NA, 1000))
newdata$time <- seq(0, 10, length.out = 1000)
plotdata_all <- effectPlotData(mix_all, newdata = newdata, orig_data = simdat_2)
plotdata_MAR <- effectPlotData(mix_MAR, newdata = newdata, orig_data = simdat_3)
plotdata_MAR_MNAR <- effectPlot_JointModelBayes(jm_MAR_MNAR, newdata = newdata, orig_data = simdat_3)

ggplot() + geom_line(data = simdat_3, aes(x = time, y = y, group = id), 
                     alpha = 0.8, lty = 'solid', color = '#fcfffd', size = 0.2) + 
  geom_line(data = simdat_2[simdat_2$keep == 0, ], aes(x = time, y = y, group = id), 
            color = '#fcfffd', lty = 'solid', alpha = 0.8, size = 0.2) + 
  ylab('Longitudinal Outcome') + 
  xlab('Time') + 
  #scale_color_manual('Fitted Model', values = c('Complete Dataset' = '#FF0136', 
  #                                              'MAR' = '#026BFF', 
  #                                              'MNAR' = '#008F4A')) + 
  scale_linetype_manual('Fitted Model', values = c('Complete Dataset' = 1, 
                                                   'MAR' = 2, 
                                                   'MNAR' = 3)) +
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#2d4059'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#fcfffd'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_blank(),
                     legend.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'),
                     legend.key.size = unit(0.25, units = 'cm'), 
                     legend.title = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                     legend.position = 'bottom')

ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\miss_dat_1.png'), 
       height = 600, width = 1280, units = 'px')
  
ggplot() + geom_line(data = simdat_3, aes(x = time, y = y, group = id), 
                     alpha = 0.8, lty = 'solid', color = '#fcfffd', size = 0.2) + 
  geom_line(data = simdat_2[simdat_2$keep == 0, ], aes(x = time, y = y, group = id), 
            color = '#fcfffd', lty = 'solid', alpha = 0.1, size = 0.2) + 
  ylab('Longitudinal Outcome') + 
  xlab('Time') + 
  #scale_color_manual('Fitted Model', values = c('Complete Dataset' = '#FF0136', 
  #                                              'MAR' = '#026BFF', 
  #                                              'MNAR' = '#008F4A')) + 
  scale_linetype_manual('Fitted Model', values = c('Complete Dataset' = 1, 
                                                   'MAR' = 2, 
                                                   'MNAR' = 3)) +
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#2d4059'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#fcfffd'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_blank(),
                     legend.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'),
                     legend.key.size = unit(0.25, units = 'cm'), 
                     legend.title = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                     legend.position = 'bottom')

ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\miss_dat_2.png'), 
       height = 600, width = 1280, units = 'px')

ggplot() + geom_line(data = simdat_3, aes(x = time, y = y, group = id), 
                     alpha = 0.8, lty = 'solid', color = '#fcfffd', size = 0.2) + 
  geom_line(data = simdat_2[simdat_2$keep == 0, ], aes(x = time, y = y, group = id), 
            color = '#fcfffd', lty = 'solid', alpha = 0.1, size = 0.2) + 
  geom_line(data = plotdata_all, aes(x = time, y = pred, linetype = 'Complete Dataset', 
                                     color = 'Complete Dataset'), 
            size = 0.4) + 
  geom_line(data = plotdata_MAR, aes(x = time, y = pred, linetype = 'MAR', color = 'MAR'), 
            size = 0.4) + 
  geom_line(data = plotdata_MAR_MNAR, aes(x = time, y = pred, linetype = 'MNAR', color = 'MNAR'), 
            size = 0.4) + 
  ylab('Longitudinal Outcome') + 
  xlab('Time') + 
  scale_color_manual('Fitted Model', values = c('Complete Dataset' = '#1098f7', 
                                                'MAR' = '#ffb400', 
                                                'MNAR' = '#ea5455')) + 
  scale_linetype_manual('Fitted Model', values = c('Complete Dataset' = 1, 
                                                   'MAR' = 1, 
                                                   'MNAR' = 1)) +
  theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.text = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = '#2d4059'), 
                     strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                     strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.line = element_line(colour = '#fcfffd'), 
                     panel.border = element_rect(colour = '#fcfffd'),
                     panel.spacing = unit(0, 'lines'),
                     axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                     axis.ticks = element_blank(),
                     legend.text = element_text(colour = '#fcfffd', size = 4, family = 'ADAM.CG PRO'),
                     legend.key.size = unit(0.25, units = 'cm'), 
                     legend.title = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
                     legend.position = c(0.2, 0.8), 
                     legend.background = element_rect(fill = '#2d4059'), 
                     legend.key = element_rect(fill = '#2d4059'))

ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Static_Images\\miss_dat_3.png'), 
       height = 600, width = 1280, units = 'px')


# ggplot() + geom_line(data = simdat_3, aes(x = time, y = y, group = id), 
#                      alpha = 1, lty = 'solid', color = '#fcfffd') + 
#   geom_line(data = simdat_2[simdat_2$keep == 0, ], aes(x = time, y = y, group = id), 
#             color = 'gray', lty = 'solid', alpha = 0.5) + 
#   geom_line(data = plotdata_all, aes(x = time, y = pred, linetype = 'Complete Dataset'), 
#             size = 1.2, color = 'black') + 
#   geom_line(data = plotdata_MAR, aes(x = time, y = pred, linetype = 'MAR'), 
#             size = 1.2, color = 'black') + 
#   geom_line(data = plotdata_MAR_MNAR, aes(x = time, y = pred, linetype = 'MNAR'), 
#             size = 1.2, color = 'black') + 
#   ylab('Longitudinal Outcome') + 
#   xlab('Time') + 
#   #scale_color_manual('Fitted Model', values = c('Complete Dataset' = '#FF0136', 
#   #                                              'MAR' = '#026BFF', 
#   #                                              'MNAR' = '#008F4A')) + 
#   scale_linetype_manual('Fitted Model', values = c('Complete Dataset' = 1, 
#                                                    'MAR' = 2, 
#                                                    'MNAR' = 3)) +
#   theme_bw() + theme(axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
#                      axis.text = element_blank(), 
#                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                      panel.background = element_rect(fill = '#2d4059'), 
#                      strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
#                      strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
#                      axis.line = element_line(colour = '#fcfffd'), 
#                      panel.border = element_rect(colour = '#fcfffd'),
#                      panel.spacing = unit(0, 'lines'),
#                      axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
#                      axis.ticks = element_blank(),
#                      legend.text = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'),
#                      legend.key.size = unit(0.25, units = 'cm'), 
#                      legend.title = element_text(colour = '#2d4059', size = 4, family = 'ADAM.CG PRO'), 
#                      legend.position = 'bottom')
