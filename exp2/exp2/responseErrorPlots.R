rm(list=ls()) 
regLine <- "red"; dataPointsCol1 <- alpha(rgb(0,0,0.3), 0.005);dataPointsCol2 <- alpha(rgb(0,0.7,0.0), 0.005)
error_color <- alpha("darkred", 0.80);line_color <- alpha(rgb(0,0,0.8), 0.56);meanPointsColor <- "darkred"
dataPointAlphaLevel <- 0.28; meanPointsColor <- "darkred"
fgmdata.c_exp1 <- loadExp1();
fgmdata.c_exp2 <- loadExp2();
fgmdata.c_exp1$response_error <- fgmdata.c_exp1$responseError;
fgmdata.c_exp1$exp <- 1; 
fgmdata.c_exp2$exp <- 2;
totalData <- rbind.fill(fgmdata.c_exp1, fgmdata.c_exp2);
YLABEL <- ""#"Mean Reported Aspect-Ratio (log)"
XLABEL <- ""#"Uncued Aspect Ratio (log)"
totalData <- data.table(totalData)
sum_stats <- totalData[, .(mean_response = mean(response_error),
                           sd_response = sd(response_error),
                           se_response = sd(response_error) / sqrt(.N),
                           sample_n = .N),
                       by = .(sub, cuedAR, uncuedAR, sameDirection1S0D, exp)];
sum_stats_2 <- sum_stats[, .(mean_response = mean(mean_response),
                             sd_response = sd(mean_response),
                             se_response = sd(mean_response) / sqrt(.N),
                             sample_n = .N),
                         by = .(uncuedAR, sameDirection1S0D, exp)] 
getFig <- ggplot(sum_stats, aes(x = uncuedAR, y = mean_response, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D)))+#, group = as.factor(sub), color = as.factor(sub)))+ #))+ #, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D))) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3, fill = meanPointsColor) +
  geom_errorbar(data = sum_stats_2, aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.015) +
  geom_vline(xintercept = 0, color = "gray20", linetype = "dotdash") +
  geom_hline(yintercept = 0, color = "gray20", linetype = "dotdash") +
  labs(x = XLABEL, y = YLABEL, title = "") +
  theme_bw() +
  facet_wrap(~exp)+
  coord_cartesian(ylim = c(-0.1, 0.1))+
  #coord_cartesian(ylim = c(-0.5, 0.5))+
  theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 1), panel.border = element_rect(size = 1), axis.text = element_text(color = "black")) +
  theme(legend.position = "none")+
  scale_color_manual(values = c("#252525", "#F8766D"))+ #+ 
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") );

getFig + theme(aspect.ratio = 1)
