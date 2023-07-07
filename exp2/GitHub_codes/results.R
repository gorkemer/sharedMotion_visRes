#
#
#
rm(list=ls()) 
options(scipen = 100)
# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker", "lmerTest", "nlme", "GGally", "minpack.lm"); require(x);lapply(x, require, character.only = TRUE)
#
#
getDemogs <- function(experiment){
  
  if (runningEXP == 1){
    fgmdata.c <- loadExp1()# 
    #
    # step 1: demographs #
    demog <- read.csv("demog_fgm.csv");
    # mean age:
    mean(as.numeric(demog$Q4), na.rm = TRUE)
  }
  if (runningEXP == 2){
    fgmdata.c <- loadExp2()#
    demog <- read.csv("demographics-bgm.csv");
    demog <- demog[!duplicated(demog$id), ]
  }
    
  # step 1: demographs #
  subs_list <- unique(fgmdata.c$sub);  length(subs_list);
  demog_selected <- demog[demog$id %in% subs_list,] # or sub for exp1
  # age:
  mean(as.numeric(demog_selected$Q4), na.rm = TRUE)
  # gender:
  table(as.factor(demog_selected$Q5))
}
#
getStats <- function(){
  #
  fgmdata.c <- loadExp2()#loadExp1()# 
  # baseline model
  fgmdata <- fgmdata.c
  fgmdata <- data.table(fgmdata);
  fgmdata$sub <- as.factor(fgmdata$sub); head(fgmdata$sub);
  sum_stats <- fgmdata[, .(mean_response = mean(responseAR),
                             sd_response = sd(responseAR),
                             se_response = sd(responseAR) / sqrt(.N),
                             sample_n = .N),
                         by = .(sub, cuedAR, uncuedAR, sameDirection1S0D, global_org)]; #global_org1W0B #global_org
  #
  sum_stats_2 <- sum_stats[, .(mean_response = mean(mean_response),
                           sd_response = sd(mean_response),
                           se_response = sd(mean_response) / sqrt(.N),
                           sample_n = .N),
                       by = .(cuedAR, uncuedAR, sameDirection1S0D,)];
  # ADDING TO MANUSCRIPT #
  number_of_sub <- unique(sum_stats$sub)
  #  cued model:
  cuedAR_model <- lmer(mean_response ~ cuedAR + (1 | sub), data = sum_stats, REML = FALSE)
  summary(cuedAR_model)
  #  uncued model:
  uncuedAR_model <- update(cuedAR_model, .~. + uncuedAR * as.factor(sameDirection1S0D), REML = TRUE); summary(uncuedAR_model);
  summary(uncuedAR_model)
  # compare:
  reduced_model <- update(cuedAR_model, .~. + uncuedAR + (uncuedAR: as.factor(sameDirection1S0D)), REML = TRUE); summary(reduced_model);
  summary(reduced_model)
  anova(cuedAR_model, uncuedAR_model)
  # globalorg:
  globalOrgModel <- update(uncuedAR_model, .~. + global_org); #lmer(mean_response ~ cuedAR + (uncuedAR * as.factor(sameDirection1S0D)) + as.factor(global_org1W0B)  + (1 | sub), data = sum_stats, REML = FALSE) 
  summary(globalOrgModel)
  anova(cuedAR_model, uncuedAR_model);
  anova(cuedAR_model, globalOrgModel);
  #
#### Step 3: ####
# for exp 2: do the random motion
  fgmdata <- bgmdata.r;
  fgmdata <- fgmdata[!(fgmdata$sub %in% excluded_people),]
  fgmdata <- data.table(fgmdata);
  fgmdata$sub <- as.factor(fgmdata$sub); head(fgmdata$sub);
  sum_stats <- fgmdata[, .(mean_response = mean(responseAR_normed),
                           sd_response = sd(responseAR_normed),
                           se_response = sd(responseAR_normed) / sqrt(.N),
                           sample_n = .N),
                       by = .(sub, cuedAR, uncuedAR, global_org1W0B)];
  #  cued model:
  simplerModel <- lmer(mean_response ~ cuedAR + (1 | sub), data = sum_stats, REML = FALSE)
  summary(simplerModel)
  #  uncued model:
  complexModel <- update(cuedAR_model, .~. + uncuedAR + as.factor(global_org1W0B), REML = TRUE); summary(complexModel);
  summary(complexModel)
  anova(simplerModel, complexModel);

#### Step 4: ####
# for exp2: do the normalized analysis
#
fgmdata <- data.table(fgmdata.c_exp2);
sum_stats <- fgmdata[, .(mean_response = mean(responseAR_normed),
                         sd_response = sd(responseAR_normed),
                         se_response = sd(responseAR_normed) / sqrt(.N),
                         sample_n = .N),
                     by = .(sub, cuedAR, uncuedAR, sameDirection1S0D, global_org1W0B)];
#  cued model:
cuedAR_model <- lmer(mean_response ~ cuedAR + (1 | sub), data = sum_stats, REML = FALSE)
summary(cuedAR_model)
#  uncued model:
uncuedAR_model <- update(cuedAR_model, .~. + uncuedAR * as.factor(sameDirection1S0D), REML = TRUE); summary(uncuedAR_model);
summary(uncuedAR_model)
# globalorg:
globalOrgModel <- lmer(mean_response ~ cuedAR + (uncuedAR * as.factor(sameDirection1S0D)) + as.factor(global_org1W0B)  + (1 | sub), data = sum_stats, REML = FALSE) 
globalOrgModel_simpler <- lmer(mean_response ~ cuedAR + as.factor(global_org1W0B)  + (1 | sub), data = sum_stats, REML = FALSE) 
summary(globalOrgModel);
summary(globalOrgModel_simpler)
# compare:
anova(uncuedAR_model, globalOrgModel);

##### step 5: response errors for appendix ####

fgmdata.c <- data.table(fgmdata.c)
sum_stats <- fgmdata.c[, .(mean_response = mean(responseError),
                           sd_response = sd(responseError),
                           se_response = sd(responseError) / sqrt(.N),
                           sample_n = .N),
                       by = .(sub, cuedAR, uncuedAR, sameDirection1S0D, global_org)];
sum_stats_2 <- sum_stats[, .(mean_response = mean(mean_response),
                           sd_response = sd(mean_response),
                           se_response = sd(mean_response) / sqrt(.N),
                           sample_n = .N),
                       by = .(sub, uncuedAR, sameDirection1S0D)];
#  cued model:
cuedAR_model <- lmer(mean_response ~ cuedAR + (1 | sub), data = sum_stats, REML = FALSE)
summary(cuedAR_model)
#  uncued model:
uncuedAR_model <-update(cuedAR_model, .~. + uncuedAR * as.factor(sameDirection1S0D), REML = TRUE); summary(uncuedAR_model);
summary(uncuedAR_model)
# globalorg:
globalOrgModel <- update(uncuedAR_model, .~. + as.factor(global_org), REML = TRUE); summary(globalOrgModel);#lmer(mean_response ~ cuedAR + (uncuedAR * as.factor(sameDirection1S0D)) + as.factor(global_org1W0B)  + (1 | sub), data = sum_stats, REML = FALSE) 
summary(globalOrgModel);
summary(globalOrgModel_simpler)





  #summary(fullModel)
  #
  # 2) global organization stats
  # a) with all interactions
  globalOrg_model <- lmer(responseError ~ uncuedAR * sameDirection1S0D * global_org  + (1 | sub), data = fgmdata,  REML = FALSE)
  summary(globalOrg_model)
  extract_eq(globalOrg_model, wrap = TRUE, terms_per_line = 2)
  # APPENDIX ANALYSES #
  # ResponseAR-appendix) main analyses but using responseAR as dependent variable 
  uncuedAR_model <- lmer(responseAR ~ uncuedAR * sameDirection1S0D + (1 | sub), data = fgmdata, REML = FALSE)#lmer(responseAR ~ uncuedAR + (uncuedAR:sameDirection1S0D) + (1 | sub), data = fgmdata, REML = FALSE)
  summary(uncuedAR_model)
  extract_eq(uncuedAR_model, wrap = TRUE, terms_per_line = 2)
  # betaCoef-appendix) uncued beta coeff stats
  
  
  
  
  #
}







fgmdata <- read.csv('fgmdata.csv', header = TRUE);
fgmdata <- read.csv('bgmdata.csv', header = TRUE);
bgmdata = read.csv("bgmdata.csv",header=TRUE, quote="\"") 
bgmdata.c = subset(bgmdata, bgmdata$randomTrialsR1C0 == 0)
bgmdata.r = subset(bgmdata, bgmdata$randomTrialsR1C0 == 1)
# load bgmn data 
bgmndata = read.csv("bgmndata.csv",header=TRUE, quote="\"") 
bgmndata.c = subset(bgmndata, bgmndata$randomTrialsR1C0 == 0)
bgmdata.c$responseAR_normed <- bgmndata.c$normedR_indv
bgmdata.c$response_error_normed <- bgmndata.c$normedR_indv - bgmndata.c$cuedAR
bgmndata.r = subset(bgmndata, bgmndata$randomTrialsR1C0 == 1)
fgmdata <- bgmdata.c
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
    
  
  
  
  
#### tim's graphs
  
  non_overlapping_df <- fgmdata[!(fgmdata$sub %in% fgmdata.c$sub)]
  length(unique(non_overlapping_df$sub)); non_overlapping_df_folks <- unique(non_overlapping_df$sub)
  palette <- magma(55)#viridis(55)
  palette1 <- magma(55)
  n <- 55;  # Number of tones
  palette1 <- plasma(72)#colorRampPalette(c("#C10000", "#F90000"))(n);
  library(colorRamps); n <- 72  # Number of shades;
  palette2 <- gray.colors(72) #plasma(55) #colorRampPalette(c("white", "black"))(n)#
  ggplot(sum_stats_2, aes(x = cuedAR, y = mean_response, group = as.factor(sub))) +
    geom_vline(xintercept = 0, color = "gray20", linetype = "dotdash") +
    geom_hline(yintercept = 0, color = "gray20", linetype = "dotdash") +
    #geom_density_2d(alpha = 0.1) +
    labs(x = "Cued AR", y = "Mean Reported Aspect-Ratio", title = "") +
    theme_bw() +
    coord_cartesian(ylim = c(-0.5, 0.5))+
    theme(panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 1), panel.border = element_rect(size = 1), axis.text = element_text(color = "black")) +
    theme(legend.position = "none")+
    geom_smooth(data = sum_stats[(sum_stats$sub %in% non_overlapping_df_folks),], method = "lm", se = FALSE, alpha = 0.5, color = "gray")+
    #geom_smooth(data = sum_stats[!(sum_stats$sub %in% non_overlapping_df_folks),],method = "lm", se = FALSE, aes(colour = as.factor(sub)))+
    scale_color_manual(values = c(palette1, palette1))
  #  
  
  
  
  
  

  subject_counts <- table(fgmdata.c$sub)
  
  