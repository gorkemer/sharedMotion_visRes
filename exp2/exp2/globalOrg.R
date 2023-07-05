library(ggplot2)
library(minpack.lm)
#
gaussian_derivative <- function(x, a, b, c) {
  a * (-2 * (x - b) * c * exp(-c * (x - b)^2))
}
fgmdata.c <- data.table(fgmdata.c)
sum_stats <- fgmdata.c[, .(mean_response = mean(responseAR),
                           sd_response = sd(responseAR),
                           se_response = sd(responseAR) / sqrt(.N),
                           sample_n = .N),
                       by = .(sub, cuedAR, global_org1W0B)];
sum_stats_2 <- sum_stats[, .(mean_response = mean(mean_response),
                             sd_response = sd(mean_response),
                             se_response = sd(mean_response) / sqrt(.N),
                             sample_n = .N),
                         by = .(cuedAR, global_org1W0B)] 
globalOrg_exp2 <- ggplot(sum_stats, aes(x = cuedAR, y = mean_response, group = as.factor(global_org1W0B), color = as.factor(global_org1W0B))) +
  #geom_point(alpha = 1/5)+
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3, fill = meanPointsColor) +
  geom_errorbar(data = sum_stats_2, aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.015) +
  geom_vline(xintercept = 0, color = "gray20", linetype = "dotdash") +
  geom_hline(yintercept = 0, color = "gray20", linetype = "dotdash") +
  geom_density_2d(alpha = 0.1) +
  labs(x = "Cued AR", y = "Mean Reported Aspect-Ratio", title = "") +
  theme_bw() +
  #ylim(c(-0.6, 0.6)) +
  #coord_cartesian(ylim = c(-0.1, 0.1))+
  coord_cartesian(ylim = c(-0.5, 0.5))+
  theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 1), panel.border = element_rect(size = 1), axis.text = element_text(color = "black")) +
  #facet_wrap(~sub) +
  theme(legend.position = "none")+
  scale_color_manual(values = c("#252525", "#959595"))+ #+ 
  #geom_smooth(method = "lm", se = TRUE) +
  geom_line(stat="smooth", method = "lm", formula = y ~ gaussian_derivative(x, a = 1, b = 0, c = 1),
            size = 1.5,
            linetype =1, #"dashed",
            alpha = 0.85,
            se = FALSE)+
  #geom_smooth(method = "lm", formula = y ~ gaussian_derivative(x, a = 1, b = 0, c = 1), se = FALSE, alpha = 1/60)+
  theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") );globalOrg_exp2
##
globalOrg_model <- lmer(mean_response ~ cuedAR + as.factor(global_org) + (1 | sub), data = sum_stats, REML = FALSE)#lmer(responseAR ~ uncuedAR + (uncuedAR:sameDirection1S0D) + (1 | sub), data = fgmdata, REML = FALSE)
summary(globalOrg_model)
#
grid.arrange(globalOrg_exp1, globalOrg_exp2, ncol = 2)
  