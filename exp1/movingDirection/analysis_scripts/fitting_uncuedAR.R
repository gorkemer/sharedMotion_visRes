library(data.table)
plotFigures <- function(drawUncued, drawCued){
  # Set color and alpha values
  regLine <- "red"; dataPointsCol1 <- alpha(rgb(0,0,0.3), 0.005);dataPointsCol2 <- alpha(rgb(0,0.7,0.0), 0.005)
  error_color <- alpha("darkred", 0.80);line_color <- alpha(rgb(0,0,0.8), 0.56)
  dataPointAlphaLevel <- 0.28; meanPointsColor <- "darkred"
  if (drawUncued == 1){
    YLABEL <- ""#"Mean Reported Aspect-Ratio (log)"
    XLABEL <- ""#"Uncued Aspect Ratio (log)"
    print("running Uncued Plot...")
    fgmdata.c <- data.table(fgmdata.c)
    sum_stats <- fgmdata.c[, .(mean_response = mean(response_error),
                               sd_response = sd(response_error),
                               se_response = sd(response_error) / sqrt(.N),
                               sample_n = .N),
                           by = .(sub, cuedAR, uncuedAR, sameDirection1S0D)];
    sum_stats_2 <- sum_stats[, .(mean_response = mean(mean_response),
                                 sd_response = sd(mean_response),
                                 se_response = sd(mean_response) / sqrt(.N),
                                 sample_n = .N),
                             by = .(uncuedAR, sameDirection1S0D)] 
    getFig <- ggplot(sum_stats_2, aes(x = uncuedAR, y = mean_response, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D)))+#, group = as.factor(sub), color = as.factor(sub)))+ #))+ #, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D))) +
      stat_summary(fun = mean, geom = "point", shape = 1, size = 3, fill = meanPointsColor) +
      geom_errorbar(data = sum_stats_2, aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.015) +
      geom_vline(xintercept = 0, color = "gray20", linetype = "dotdash") +
      geom_hline(yintercept = 0, color = "gray20", linetype = "dotdash") +
      labs(x = XLABEL, y = YLABEL, title = "") +
      theme_bw() +
      coord_cartesian(ylim = c(-0.1, 0.1))+
      #coord_cartesian(ylim = c(-0.5, 0.5))+
      theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 1), panel.border = element_rect(size = 1), axis.text = element_text(color = "black")) +
      theme(legend.position = "none")+
      scale_color_manual(values = c("#252525", "#F8766D"))+ #+ 
      geom_smooth(method = "lm", se = FALSE) +
      theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") );
    getFig;
  }
  if (drawCued == 1){
    print("running Cued Plot...")
    YLABEL <- ""#"Mean Reported Aspect-Ratio (log)"
    XLABEL <- ""#"Uncued Aspect Ratio (log)"
    fgmdata.c <- data.table(fgmdata.c)
    sum_stats <- fgmdata.c[, .(mean_response = mean(responseAR),
                               sd_response = sd(responseAR),
                               se_response = sd(responseAR) / sqrt(.N),
                               sample_n = .N),
                           by = .(sub, cuedAR, uncuedAR, sameDirection1S0D)];
    sum_stats_2 <- sum_stats[, .(mean_response = mean(mean_response),
                                 sd_response = sd(mean_response),
                                 se_response = sd(mean_response) / sqrt(.N),
                                 sample_n = .N),
                             by = .(cuedAR, sameDirection1S0D)] 
    getFig <- ggplot(sum_stats, aes(x = cuedAR, y = mean_response, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D)))+#, group = as.factor(sub), color = as.factor(sub)))+ #))+ #, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D))) +
      stat_summary(fun = mean, geom = "point", shape = 1, size = 3, fill = meanPointsColor) +
      geom_errorbar(data = sum_stats_2, aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.015) +
      geom_vline(xintercept = 0, color = "gray20", linetype = "dotdash") +
      geom_hline(yintercept = 0, color = "gray20", linetype = "dotdash") +
      labs(x = XLABEL, y = YLABEL, title = "") +
      theme_bw() +
      geom_density_2d(alpha = 0.15) +
      coord_cartesian(ylim = c(-0.5, 0.5))+
      theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 1), panel.border = element_rect(size = 1), axis.text = element_text(color = "black")) +
      theme(legend.position = "none")+
      scale_color_manual(values = c("#252525", "#F8766D"))+ #+ 
      geom_smooth(method = "lm", se = FALSE) +
      theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") );
  }
  return(getFig)
}
exp1_uncuedAR <- plotFigures(1,0); exp1_uncuedAR;
exp1_cuedAR <- plotFigures(0,1); exp1_cuedAR;

exp1_uncuedAR + theme(aspect.ratio = 1)
exp1_cuedAR + theme(aspect.ratio = 1)
grid.arrange(exp1_cuedAR, exp1_uncuedAR, ncol = 2)
# response errors below #
exp1_uncuedAR_RE <- getFig; exp1_uncuedAR_RE;
exp2_uncuedAR_RE <- getFig; exp2_uncuedAR_RE;
grid.arrange(exp1_uncuedAR_RE + theme(aspect.ratio = 1), exp2_uncuedAR_RE + theme(aspect.ratio = 1), ncol = 1)
#
testFig;
uncuedAR_plot <- ggplot(sum_stats_2, aes(x = uncuedAR, y = mean_response, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D)))+#, group = as.factor(sub), color = as.factor(sub)))+ #))+ #, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D))) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3, fill = meanPointsColor) +
  geom_errorbar(data = sum_stats_2, aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.015) +
  geom_vline(xintercept = 0, color = "gray20", linetype = "dotdash") +
  geom_hline(yintercept = 0, color = "gray20", linetype = "dotdash") +

  labs(x = "Uncued Aspect Ratio (log)", y = "Mean Reported Aspect-Ratio (log)", title = "") +
  theme_bw() +
  #ylim(c(-0.5,0.5))+
  #ylim(c(-0.6, 0.6)) +
  #geom_density_2d(alpha = 0.15) +
  coord_cartesian(ylim = c(-0.08, 0.08))+
  #coord_cartesian(ylim = c(-0.5, 0.5))+
  theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 1), panel.border = element_rect(size = 1), axis.text = element_text(color = "black")) +
  #facet_wrap(~sub) +
  theme(legend.position = "none")+
  scale_color_manual(values = c("#252525", "#F8766D"))+ #+ 
  geom_smooth(method = "lm", se = FALSE) +
  #geom_line(stat="smooth", method = "lm", formula = y ~ derivative_gaussian(x, a = 1, b = 0, c = 1),
  #          size = 1.5,
  #          linetype =1, #"dashed",
  #          alpha = 0.85,
  #          se = FALSE)+
  #geom_smooth(method = "lm", formula = y ~ gaussian_derivative(x, a = 1, b = 0, c = 1), se = FALSE, alpha = 1/60)+
  theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") );
library(gridExtra);
uncuedAR_plot <- uncuedAR_plot + theme(aspect.ratio = 1);
uncuedAR_plot;

uncuedAR_plot;
exp1_cuedAR <- cuedAR_plot #c
exp1_uncuedAR <- uncuedAR_plot #c
#
#
grid.arrange(exp1_cuedAR, exp1_uncuedAR, ncol = 2)#
grid.arrange(cuedAR_plot, uncuedAR_plot, ncol = 2)#
grid.arrange(exp1_cuedAR, exp1_uncuedAR,
             exp2_cuedAR, exp2_uncuedAR,
             exp2_cuedAR_normed, exp2_uncuedAR_normed,
             ncol = 2)


# triple 3333 #
fgmdata.c <- data.table(fgmdata.c)
sum_stats <- fgmdata.c[, .(mean_response = mean(responseAR_normed),
                           sd_response = sd(responseAR_normed),
                           se_response = sd(responseAR_normed) / sqrt(.N),
                           sample_n = .N),
                       by = .(sub, cuedAR, uncuedAR, sameDirection1S0D)];
sum_stats_2 <- sum_stats[, .(mean_response = mean(mean_response),
                             sd_response = sd(mean_response),
                             se_response = sd(mean_response) / sqrt(.N),
                             sample_n = .N),
                         by = .(uncuedAR, sameDirection1S0D)] 
uncuedAR_plot_normed <- ggplot(sum_stats_2, aes(x = uncuedAR, y = mean_response, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D)))+#, group = as.factor(sub), color = as.factor(sub)))+ #))+ #, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D))) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3, fill = meanPointsColor) +
  geom_errorbar(data = sum_stats_2, aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.015) +
  geom_vline(xintercept = 0, color = "gray20", linetype = "dotdash") +
  geom_hline(yintercept = 0, color = "gray20", linetype = "dotdash") +
  #geom_density_2d(alpha = 0.25) +
  labs(x = "Uncued AR", y = "Mean Reported Aspect-Ratio | Normed", title = "") +
  theme_bw() +
  #ylim(c(-0.6, 0.6)) +
  coord_cartesian(ylim = c(-0.1, 0.1))+
  #coord_cartesian(ylim = c(-0.5, 0.5))+
  theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 1), panel.border = element_rect(size = 1), axis.text = element_text(color = "black")) +
  #facet_wrap(~sub) +
  theme(legend.position = "none")+
  scale_color_manual(values = c("#252525", "#F8766D"))+ #+ 
  #geom_smooth(method = "lm", se = TRUE) +
  geom_line(stat="smooth", method = "lm", formula = y ~ derivative_gaussian(x, a = 1, b = 0, c = 1),
            size = 1.5,
            linetype =1, #"dashed",
            alpha = 0.85,
            se = FALSE)+
  #geom_smooth(method = "lm", formula = y ~ gaussian_derivative(x, a = 1, b = 0, c = 1), se = FALSE, alpha = 1/60)+
  theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") ); uncuedAR_plot_normed
grid.arrange(cuedAR_plot, uncuedAR_plot, uncuedAR_plot_normed, ncol = 3)#


#
grid.arrange(cuedAR_plot, uncuedAR_plot, cuedAR_plot_normed, uncuedAR_plot_normed, ncol = 2)#
# end.