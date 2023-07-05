#
# clear the workspace
rm(list=ls()) 
# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker", "lmerTest", "nlme", "GGally", "minpack.lm"); require(x);lapply(x, require, character.only = TRUE)
# rm(list=ls()) #options(scipen = 100)
loadExp1 <- function(){
  fgmdata <- read.csv('fgmdata.csv', header = TRUE);
  fgmdata$respTall <- ifelse(fgmdata$responseAR > 0, 1, ifelse(fgmdata$responseAR < 0, 0, -1));
  fgmdata$cuedAR <- round(fgmdata$cuedAR, digits = 2)
  fgmdata$uncuedAR <- round(fgmdata$uncuedAR, digits = 2)
  fgmdata$responseAR <- round(fgmdata$responseAR, digits = 2)
  fgmdata$uncuedCat = ifelse(fgmdata$uncuedAR < 0, -1, ifelse(fgmdata$uncuedAR==-0, 0, 1))
  fgmdata$uncuedCat <- as.factor(fgmdata$uncuedCat)
  fgmdata$cuedCat = ifelse(fgmdata$cuedAR < 0, -1, ifelse(fgmdata$cuedAR==-0, 0, 1))
  fgmdata$respAcc <- ifelse( (fgmdata$responseAR > 0 & fgmdata$cuedCat == 1)  | (fgmdata$responseAR < 0 & fgmdata$cuedCat == -1) | (fgmdata$responseAR == 0 & fgmdata$cuedCat == 0), 1, 0)
  fgmdata$globalMotion <- as.factor(ifelse(fgmdata$cued_motion_dir == 90 | fgmdata$cued_motion_dir == 270, 1, -1))
  regLine <- "red"
    dataPointsCol1 <- alpha(rgb(0,0,0.3), 0.005)
    dataPointsCol2 <- alpha(rgb(0,0.7,0.0), 0.005)
    error_color <- alpha("darkred", 0.80)
    line_color <- alpha(rgb(0,0,0.8), 0.56)
    dataPointAlphaLevel <- 0.30
    meanPointsColor <- "darkred"
      # Example data
    correct_responses <- c(0, 1, 1, 1, 0, 0, 1, 1, 1, 1)
    stimulus_levels <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
    sample_data <- aggregate(respTall ~ cuedAR + sub, fgmdata[fgmdata$respTall %in% c(0,1),], mean) 
    correct_responses <- sample_data$respTall
    stimulus_levels <- sample_data$cuedAR
    # Define the cumulative Gaussian function
    cumulative_gaussian <- function(x, mu, sigma) {
      pnorm(x, mean = mu, sd = sigma)
    }
    gaussian_derivative <- function(x, a, b, c) {
      a * (-2 * (x - b) * c * exp(-c * (x - b)^2))
    }
    # Define the residual function for fitting
    residuals_function <- function(params, x, y) {
      mu <- params[1]
      sigma <- params[2]
      y_pred <- cumulative_gaussian(x, mu, sigma)
      residuals <- y - y_pred
      return(residuals)
    }
    
    # Fit the cumulative Gaussian function to the data
    fit <- nlsLM(correct_responses ~ cumulative_gaussian(stimulus_levels, mu, sigma),
                 start = list(mu = mean(stimulus_levels), sigma = sd(stimulus_levels)),
                 algorithm = "LM",
                 trace = FALSE,
                 lower = c(-Inf, 0)) # enforce positive sigma
    error = function(e) NULL
    
    # Extract model parameters
    mu <- coef(fit)["mu"]
    sigma <- coef(fit)["sigma"]
    
    # Calculate the JND as 1.732 * sigma (assuming a symmetric cumulative Gaussian)
    JND <- 1.732 * sigma
    threshold_50 <- qnorm(0.5, mean = mu, sd = sigma)
    threshold_75 <- qnorm(0.75, mean = mu, sd = sigma)
    JND <- threshold_75 - threshold_50# 1.732 * sigma
    
    # Print the results
    cat("Estimated mu:", mu, "\n")
    cat("Estimated sigma:", sigma, "\n")
    cat("Just Noticeable Difference (JND):", JND, "\n")
    
    # Generate data for plotting the fitted function
    x <- seq(min(stimulus_levels), max(stimulus_levels), length = 100)
    y <- cumulative_gaussian(x, mu, sigma)
    
    # Create a scatter plot with the fitted function
    plot <- ggplot(data.frame(stimulus_levels, correct_responses), aes(x = stimulus_levels, y = correct_responses)) +
      geom_point() +
      #geom_step(data = data.frame(x, y), aes(x = x, y = y), color = "red") +
      labs(x = "Stimulus Levels", y = "Correct Responses") +
      ggtitle("Fitted Cumulative Gaussian Function")
    
    # Print the plot
    print(plot)
    
    ##### doing at a scale ####
    jnd_list <- NA
    id_list <- NA
    for (i in unique(fgmdata$sub)){
      print(i)
      id_list <- append(id_list, i)
      tmpdata <- fgmdata[fgmdata$sub == i,]
      tmpdata <- aggregate(respTall ~ cuedAR, fgmdata[fgmdata$respTall %in% c(0,1) & fgmdata$sub == i,], mean) 
      stimulus_levels <- tmpdata$cuedAR
      correct_responses <- tmpdata$respTall
      
      fit <- tryCatch(nlsLM(correct_responses ~ cumulative_gaussian(stimulus_levels, mu, sigma),
                            start = list(mu = mean(stimulus_levels), sigma = sd(stimulus_levels)),
                            algorithm = "LM",
                            trace = FALSE,
                            lower = c(-Inf, 0)), # enforce positive sigma
                      error = function(e) NULL
      )  # Check if the fit was successful
      if (!is.null(fit)) {
        # Extract model parameters
        mu <- coef(fit)['mu']
        sigma <- coef(fit)['sigma']
        
        # Calculate the just noticeable difference (JND)
        threshold_50 <- qnorm(0.5, mean = mu, sd = sigma)
        threshold_75 <- qnorm(0.75, mean = mu, sd = sigma)
        JND <-  threshold_75 - threshold_50# 1.732 * sigma #threshold_75 - threshold_50 #The value of 1.732 is used to scale the standard deviation (sigma) to estimate the JND #  Specifically, for a cumulative Gaussian distribution, the range from the 25th percentile to the 75th percentile is approximately 1.732 times the standard deviation
        jnd_list <- append(jnd_list, JND);
        # Print the results
        cat("Estimated mu:", mu, "\n")
        cat("Estimated sigma:", sigma, "\n")
        cat("Just Noticeable Difference (JND):", JND, "\n")
      } else {
        jnd_list <- append(jnd_list, 1);
        cat("Fit unsuccessful. Check your data and initial parameter estimates.\n")
      }
    }
    jnd_df <- data.frame(id_list[-1], jnd_list[-1]); colnames(jnd_df) <- c("id", "jnd");
    hist(jnd_list) 
    ggplot(jnd_df, aes(id, jnd, label = id)) + geom_label(alpha = 1/3) 
    jnd_df$id[jnd_df$jnd >0.5] # these people should be gone. 
    jnd_df$id[jnd_df$jnd >0.43] # these people should be gone. 
    mean(jnd_df$jnd)
    sd(jnd_df$jnd)
    hist(jnd_df$jnd)
    excluded_people <- jnd_df$id[jnd_df$jnd >0.6]; excluded_people;
    fgmdata.c <- fgmdata[!(fgmdata$sub %in% excluded_people), ]
    print(length(unique(fgmdata$sub)))
    print(length(unique(fgmdata.c$sub)))
    return(fgmdata.c)
}
fgmdata.c <- loadExp1()#
#
##
#
#
mean(jnd_df$jnd[jnd_df$jnd < 3])
#
ggplot(jnd_df[!(jnd_df$id %in% excluded_people),], aes(id, jnd, label = id)) + geom_label(alpha = 1/3)
sample_data <- aggregate(responseAR ~ cuedAR + uncuedAR + sameDirection1S0D + sub, fgmdata, mean);#fgmdata.c[fgmdata.c$randomTrialsR1C0 ==0,], mean)
modelCuedAR <- lmer(responseAR ~ cuedAR + uncuedAR + sameDirection1S0D + (uncuedAR * sameDirection1S0D) + (1 | sub), data = sample_data, REML = FALSE)
summary(modelCuedAR)
#
ggplot(jnd_df[!(jnd_df$id %in% excluded_people),], aes(id, jnd, label = id)) + geom_label(alpha = 1/3)
# 
#
library(data.table)
fgmdata.c <- data.table(fgmdata.c)
sum_stats <- fgmdata.c[, .(mean_response = mean(responseAR),
                    sd_response = sd(responseAR),
                    se_response = sd(responseAR) / sqrt(.N),
                    sample_n = .N),
                by = .(sub, cuedAR, sameDirection1S0D)];
sum_stats_2 <- sum_stats[, .(mean_response = mean(mean_response),
                           sd_response = sd(mean_response),
                           se_response = sd(mean_response) / sqrt(.N),
                           sample_n = .N),
                       by = .(cuedAR, sameDirection1S0D)] 
cuedAR_plot <- ggplot(sum_stats, aes(x = cuedAR, y = mean_response, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D))) +
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
  scale_color_manual(values = c("#252525", "#F8766D"))+ #+ 
  geom_smooth(method = "lm", se = TRUE) +
  # geom_line(stat="smooth", method = "lm", formula = y ~ derivative_gaussian(x, a = 1, b = 0, c = 1),
  #           size = 1.5,
  #           linetype =1, #"dashed",
  #           alpha = 0.85,
  #           se = FALSE)+
  #geom_smooth(method = "lm", formula = y ~ gaussian_derivative(x, a = 1, b = 0, c = 1), se = FALSE, alpha = 1/60)+
  theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") );cuedAR_plot
##
# normed normed #
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
                         by = .(cuedAR, sameDirection1S0D)] 
cuedAR_plot_normed <- ggplot(sum_stats, aes(x = cuedAR, y = mean_response, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D)))+#, group = as.factor(sub), color = as.factor(sub)))+ #))+ #, group = as.factor(sameDirection1S0D), color = as.factor(sameDirection1S0D))) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 3, fill = meanPointsColor) +
  geom_errorbar(data = sum_stats_2, aes(ymin = mean_response - se_response, ymax = mean_response + se_response), width = 0.015) +
  geom_vline(xintercept = 0, color = "gray20", linetype = "dotdash") +
  geom_hline(yintercept = 0, color = "gray20", linetype = "dotdash") +
  geom_density_2d(alpha = 0.1) +
  labs(x = "CuedAR", y = "Mean Reported Aspect-Ratio | Normed", title = "") +
  theme_bw() +
  #ylim(c(-0.6, 0.6)) +
  #coord_cartesian(ylim = c(-0.1, 0.1))+
  coord_cartesian(ylim = c(-0.5, 0.5))+
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
  theme(legend.key = element_blank(), strip.background = element_rect(colour="white", fill="white") ); cuedAR_plot_normed



#

