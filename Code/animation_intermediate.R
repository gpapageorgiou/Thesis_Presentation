betas1 <- c(20.7, 1.6, -15.5, -0.76)
betas4 <- c(20.7, 1.6, 0, 0)

time <- seq(0, 30, length.out = 200)
reop.time <- 11.85479
reop.index <- as.numeric(time >= reop.time)
time_relative <- pmax(time - reop.time, 0)
yends <- pmax(time - (reop.time - 2), 0)

dat <- data.frame("time" = rep(time, 1), "reop.index" = rep(reop.index, 1), 
                  "time_relative" = rep(time_relative, 1), 
                  "reop.time" = reop.time, 
                  'yends' = yends)

datfit <- data.frame("time" = time, "reop.index" = reop.index, "time_relative" = time_relative)

form1 <- formula( ~ time + reop.index + time_relative) 

y1 <- model.matrix(object = form1, data = datfit) %*% betas1

dat$y <- c(y1)

y4 <- model.matrix(object = form1, data = datfit) %*% betas4

dat$y4 <- c(y4)

dat$yends[dat$yends != 0] <- c(seq(0, 40, length.out = 14), rep(80, 120))

#library(ggplot2)
#library(extrafont)
#i < -100

for(i in 1:200) {
  if (dat$time[i] < dat$reop.time[i]) {
    ggplot() + 
      theme_bw() + theme(axis.text = element_blank(), 
                         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                         panel.background = element_rect(fill = '#fcfffd'), 
                         strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                         strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                         axis.line = element_line(colour = '#fcfffd'), 
                         panel.border = element_rect(colour = '#fcfffd'),
                         panel.spacing = unit(0, 'lines'),
                         axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'),
                         axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'),
                         axis.ticks = element_blank()) + 
      geom_line(data = dat[1:i, ], aes(x = time, y = y), color = '#2d4059', size = 1.2) + 
      geom_segment(data = dat[1:i, ], aes(x = reop.time, xend = reop.time, y = 0, yend = yends), 
                   color = '#ea5455', linetype = 2) + 
      #geom_line(data = dat, aes(x = time, y = y4), color = '#2d4059', alpha = 0.5, size = 1.2) + 
      xlab('Time') + ylab('Pulmonary Gradient') + 
      #geom_blank(data = dat, aes(x = time, y = y4)) + 
      ylim(0,80) + xlim(0, 30)
      
  } else {
    ggplot() + 
      theme_bw() + theme(axis.text = element_blank(), 
                         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                         panel.background = element_rect(fill = '#fcfffd'), 
                         strip.background = element_rect(fill = '#fcfffd', colour = '#fcfffd'), 
                         strip.text = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'), 
                         axis.line = element_line(colour = '#fcfffd'), 
                         panel.border = element_rect(colour = '#fcfffd'),
                         panel.spacing = unit(0, 'lines'),
                         axis.title.x = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'),
                         axis.title.y = element_text(colour = '#2d4059', size = 5, family = 'ADAM.CG PRO'),
                         axis.ticks = element_blank()) + 
      geom_line(data = dat[1:i, ], aes(x = time, y = y), color = '#2d4059', size = 1.2) + 
      geom_segment(data = dat[1:i, ], aes(x = reop.time, xend = reop.time, y = 15, yend = yends), 
                   color = '#ea5455', linetype = 2) + 
      geom_line(data = dat[1:i, ], aes(x = time, y = y4), color = '#2d4059', alpha = 0.5, size = 1.2) + 
      xlab('Time') + ylab('Pulmonary Gradient') + 
      #geom_blank(data = dat, aes(x = time, y = y4)) + 
      ylim(0,80) + 
      xlim(0, 30)
  }
  ggsave(filename = paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\PART_I\\Intermediate_', i, '.png'), 
         height = 648, width = 1280, units = 'px')
}

paths <- rep(NA, 200)
for(i in 1:200) {
  paths[i] <- paste0('D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\PART_I\\Intermediate_', 
                     i, '.png')
}

gif_file <- gifski(paths, delay = 0.025, loop = FALSE, width = 1280, height = 648, 
                   gif_file = 'D:\\gpapageorgiou\\myPresentations\\Thesis_Presentation\\Animations\\PART_I\\animation_intermediate.gif')
