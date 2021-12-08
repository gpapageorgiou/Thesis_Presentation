library(JMbayes2)

fm1 <- lme(log(serBilir) ~ ns(year, 3) + age, data = pbc2,
           random = ~ ns(year, 3) | id, control = lmeControl(opt = 'optim'))

pbc2.id$event <- as.numeric(pbc2.id$status != "alive")

CoxFit <- coxph(Surv(years, event) ~ age, data = pbc2.id, model = TRUE)

jointFit <- jm(CoxFit, list(fm1), time_var = "year")

pbc2$sex

nd <- data.frame(serBilir = 1, 'year' = 0, 'age' = 50, 
                 'id' = 1)


predlong <- predict(jointFit, newdata = nd, times = seq(1, 14, length.out = 1000),
        return_newdata = TRUE)

ggplot() +
  geom_line(data = predlong$newdata2, 
            aes(x = year, y = pred_serBilir), 
            color = '#1098F7', 
            #size = predLong2$newdata[predLong2$newdata$id == 93, 'year'] * 0.20) + 
            size = seq(0.2, 0.8, length.out = 1001)) +
  #geom_segment(aes(x = -0.25, xend = -0.25, y = -1.4, yend = 0.8), color = '#FF6392') + 
  #geom_segment(aes(x = rep(-0.35, 10), xend = rep(-0.15, 10), 
  #                 y = seq(-1.4, 0.8, length.out = 10), yend = seq(-1.4, 0.8, length.out = 10)), 
  #             color = '#FF6392') + 
  #geom_segment(aes(x = -0.5, xend = 4, y = 0.7, yend = 0.7), color = '#FF6392') + 
  #geom_segment(aes(x = rep(-0.35, 10), xend = rep(-0.15, 10), 
  #                 y = seq(-1.4, 0.8, length.out = 10), yend = seq(-1.4, 0.8, length.out = 10)), 
  #             color = '#FF6392') +
  ylim(-1, 4) +
  theme_void() +
  theme(panel.background = element_rect(fill = '#2d4059', color = '#2d4059'), 
        panel.grid = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"), 
        strip.background = element_blank())