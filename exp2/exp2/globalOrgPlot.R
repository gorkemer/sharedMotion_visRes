plotGlobalOrgFigures <- function(experiment){
  # Set color and alpha values
  regLine <- "red"; dataPointsCol1 <- alpha(rgb(0,0,0.3), 0.005);dataPointsCol2 <- alpha(rgb(0,0.7,0.0), 0.005)
  error_color <- alpha("darkred", 0.80);line_color <- alpha(rgb(0,0,0.8), 0.56);meanPointsColor <- "darkred"
  dataPointAlphaLevel <- 0.28; meanPointsColor <- "darkred"
  fgmdata.c_exp1 <- loadExp1();
  fgmdata.c_exp2 <- loadExp2();
  fgmdata.c_exp2$global_org <- fgmdata.c_exp2$global_org1W0B;
  fgmdata.c_exp1$exp <- 1; 
  fgmdata.c_exp2$exp <- 2;
  totalData <- rbind.fill(fgmdata.c_exp1, fgmdata.c_exp2);
  YLABEL <- ""#"Mean Reported Aspect-Ratio (log)"
  XLABEL <- ""#"Uncued Aspect Ratio (log)"
  totalData <- data.table(totalData)
  sum_stats <- totalData[, .(mean_response = mean(responseAR),
                             sd_response = sd(responseAR),
                             se_response = sd(responseAR) / sqrt(.N),
                             sample_n = .N),
                         by = .(sub, cuedAR, uncuedAR, global_org, exp)];
  sum_stats_2 <- sum_stats[, .(mean_response = mean(mean_response),
                               sd_response = sd(mean_response),
                               se_response = sd(mean_response) / sqrt(.N),
                               sample_n = .N),
                           by = .(cuedAR, global_org, exp)] 
  getFig <- ggplot(sum_stats_2, aes(x = cuedAR, y = mean_response, group = as.factor(global_org), color = as.factor(global_org)))+#, group = as.factor(sub), color = as.factor(sub)))+ #))+ #, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D))) +
    stat_summary(fun = mean, geom = "point", shape = 1, size = 3, fill = meanPointsColor) +
    geom_errorbar(data = sum_stats_2, aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.015) +
    geom_vline(xintercept = 0, color = "gray30", linetype = "dotdash") +
    geom_hline(yintercept = 0, color = "gray30", linetype = "dotdash") +
    labs(x = XLABEL, y = YLABEL, title = "") +
    theme_bw() +
    #geom_density_2d(alpha = 0.15) +
    coord_cartesian(ylim = c(-0.5, 0.5))+
    facet_wrap(~exp)+
    theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 1), panel.border = element_rect(size = 1), axis.text = element_text(color = "black")) +
    theme(legend.position = "bottom")+
    scale_color_manual(values = c("#999999", "black"))+ #+ 
    geom_line(lty = 1, size = 1)+
    #geom_smooth(method = "lm", se = FALSE, size = 1.5, alpha = 0.85) +
    #geom_line(stat="smooth", method = "lm", formula = y ~ gaussian_derivative(x, a = 1, b = 0, c = 1),
    #          size = 1.5,
    #          linetype =1, #"dashed",
    #          alpha = 0.85,
    #          se = FALSE)+
    theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") ); 
  getFig;
  getFig + theme(aspect.ratio = 1)
######
  
  
sum_stats <- fgmdata[, .(mean_response = mean(responseAR_normed),
                         sd_response = sd(responseAR_normed),
                         se_response = sd(responseAR_normed) / sqrt(.N),
                         sample_n = .N),
                     by = .(sub, cuedAR, uncuedAR, global_org1W0B)];
globalOrgModel <- lmer(mean_response ~ cuedAR + uncuedAR + global_org1W0B + (1 | sub), data = sum_stats, REML = FALSE)
summary(globalOrgModel);
getFig <- ggplot(sum_stats, aes(x = cuedAR, y = mean_response, group = as.factor(global_org1W0B), color = as.factor(global_org1W0B)))+#, group = as.factor(sub), color = as.factor(sub)))+ #))+ #, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D))) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3, fill = meanPointsColor) +
  #geom_errorbar(data = sum_stats_2, aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.015) +
  geom_vline(xintercept = 0, color = "gray30", linetype = "dotdash") +
  geom_hline(yintercept = 0, color = "gray30", linetype = "dotdash") +
  labs(x = XLABEL, y = YLABEL, title = "") +
  theme_bw() +
  #geom_density_2d(alpha = 0.15) +
  coord_cartesian(ylim = c(-0.5, 0.5))+
  #facet_wrap(~global_org1W0B)+
  theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 1), panel.border = element_rect(size = 1), axis.text = element_text(color = "black")) +
  theme(legend.position = "top")+
  scale_color_manual(values = c("#999999", "black"))+ #+ 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, alpha = 0.85) +
  #geom_line(stat="smooth", method = "lm", formula = y ~ gaussian_derivative(x, a = 1, b = 0, c = 1),
  #          size = 1.5,
  #          linetype =1, #"dashed",
  #          alpha = 0.85,
  #          se = FALSE)+
  theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") ); 
getFig;
  
  
########
  x <- sum_stats_2[sum_stats_2$exp == 2,]$cuedAR
  y <- sum_stats_2[sum_stats_2$exp == 2,]$mean_response
  z <- sum_stats_2[sum_stats_2$exp == 2,]$global_org
  gaussian_model <- nls(y ~ a * exp(-(x - b)^2 / (2 * c^2)) * (x - b) / c^2 + d * z, start = list(a = 1, b = 1, c = 1, d = 1))
  #gaussian_model <- nls(y ~ a * exp(-(x - b)^2 / (2 * c^2)) * (x - b) / c^2, start = list(a = 1, b = 1, c = 1))
  # Calculate the predicted values from the model
  predicted <- predict(gaussian_model)
  # Calculate the residual sum of squares (RSS)
  rss <- sum((y - predicted)^2)
  # Calculate the total sum of squares (TSS)
  tss <- sum((y - mean(y))^2)
  # Calculate the number of observations
  n <- length(y)
  # Calculate the number of predictors (parameters) in the model
  num_predictors <- length(coef(gaussian_model))
  # Calculate the R-squared
  r_squared <- 1 - rss / tss
  # Calculate the adjusted R-squared
  adjusted_r_squared <- 1 - (rss / (n - num_predictors - 1)) / (tss / (n - 1)); adjusted_r_squared;
  #
  #
  #
  linear_model <- lm(y ~ x + z)
  # Calculate AIC for the Gaussian derivative model
  gaussian_aic <- AIC(gaussian_model); gaussian_aic;
  # Calculate AIC for the linear regression model
  linear_aic <- AIC(linear_model); linear_aic;
  # Compare AIC or BIC values
  if (gaussian_aic < linear_aic) {
    # Gaussian derivative model has a better fit
    print("Gaussian derivative model has a better fit")
  } else {
    # Linear regression model has a better fit
    print("Linear regression model has a better fit")
  }
  # Calculate the difference in AIC values
  delta_aic <- gaussian_aic - linear_aic
  
  # Determine the degrees of freedom
  df <- length(coef(linear_model)) - length(coef(gaussian_model))
  df <- 1
  # Calculate the p-value using the chi-squared distribution
  p_value <- pchisq(delta_aic, df, lower.tail = FALSE)
  p_value;
  # Do the comparison
  df = 1;
  lrt = -2 * (gaussian_aic - linear_aic);
  lrtp = 1 - pchisq(lrt, df);
  
  print(sprintf('LRT = %.1f, p = %.2g.', lrt, lrtp))
  
  #
  gaussian_derivative <- function(x, a, b, c) {
    a * (-2 * (x - b) * c * exp(-c * (x - b)^2))
  }
  