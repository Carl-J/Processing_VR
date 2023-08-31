setwd("D:/Carl/VR/Paper/Data")
library(dplyr)
library(reshape2)
library(lme4)
library(statmod)
library(geepack)
library(ggplot2)
library("readr")
library("broom")
library("lm.beta")   
library("pwr")

#preliminary processing, assigning all the datasheet
HR <- read.csv("HR.csv")
lnssd <- read.csv("Inssd.csv")
rmssd <- read.csv("rmssd.csv")
scl <- read.csv("SCL.csv")
SBP<- read.csv("BP-SBP.csv")
DBP<- read.csv("BP-DBP.csv")
survey2 <- read.csv("Check-out Survey_211127.csv")
survey1 <- read.csv("Screening survey_211127.csv")
AU <- read.csv("AU.csv")
AU_ori <- read.csv("AU score.csv")
num_test <- read.csv("num_test.csv")

#the measurement right before observing each virtual scene
#served as a baseline parameter and was deducted from the measurement 
#right after or during the intervention to get the change we need
HR <- HR %>% mutate(nonbio_diff=nonbiophilic-nonbiophilic_bas,
                    indo_diff=indoor-indoor_bas,
                    outd_diff=outdoor-outdoor_bas,
                    airpo_diff=airpollution-airpollution_bas,
                    comb_diff=combine-combine_bas)

lnssd <- lnssd %>% mutate(nonbio_diff=nonbiophilic-nonbiophilic_bas,
                          indo_diff=indoor-indoor_bas,
                          outd_diff=outdoor-outdoor_bas,
                          airpo_diff=airpollution-airpollution_bas,
                          comb_diff=combine-combine_bas)

rmssd <- rmssd %>% mutate(nonbio_diff=nonbiophilic-nonbiophilic_bas,
                          indo_diff=indoor-indoor_bas,
                          outd_diff=outdoor-outdoor_bas,
                          airpo_diff=airpollution-airpollution_bas,
                          comb_diff=combine-combine_bas)

scl <- scl %>% mutate(nonbio_diff=nonbiophilic-nonbiophilic_bas,
                          indo_diff=indoor-indoor_bas,
                          outd_diff=outdoor-outdoor_bas,
                          airpo_diff=airpollution-airpollution_bas,
                          comb_diff=combine-combine_bas)

#merge the recruitment survey with the check-out survey
survey <- merge(survey1,
                survey2,
                by="ID",
                all.x= T)
#selecting the variables we need
survey_model <- select(survey, -c("name", "ethnicity", "stress_level_0", "stress_level_1", "stress_level_2",
                                  "stress_level_3", "stress_level_4", "connection.with.nature_0",        
                                  "connection.with.nature_1",         "connection.with.nature_2",        
                                  "connection.with.nature_3",         "connection.with.nature_4",        
                                  "b1iophilic.patterns.preference_1", "b1iophilic.patterns.preference_2",
                                  "b1iophilic.patterns.preference_3"))

#check the outliers
boxplot(rmssd$nonbio_diff)
boxplot(rmssd$nonbio_diff, plot=FALSE)$out
#[1] -36.74676 \-76.75803 (28, too high a difference than others)
boxplot(rmssd$indo_diff)
boxplot(rmssd$indo_diff, plot=FALSE)$out
#[1] -29.81193 \-73.36879 (19, abnormal value for indoor, the indoor measurement is only around 4) 
#-65.58023 (28, too high a difference than others)
boxplot(rmssd$outd_diff)
boxplot(rmssd$outd_diff, plot=FALSE)$out
#[1] -37.66557 \-56.01626 (28, too high a difference than others)
boxplot(rmssd$airpo_diff)
boxplot(rmssd$airpo_diff, plot=FALSE)$out
#[1] -29.11905 -27.58426  19.57488 \-66.41421 (28, too high a difference than others)
boxplot(rmssd$comb_diff)
boxplot(rmssd$comb_diff, plot=FALSE)$out
#numeric(0)
boxplot(HR$nonbio_diff)
boxplot(HR$nonbio_diff, plot=FALSE)$out
#[1] 23.70648
boxplot(HR$indo_diff)
boxplot(HR$indo_diff, plot=FALSE)$out
#[1] 14.317724 -2.521696
boxplot(HR$outd_diff)
boxplot(HR$outd_diff, plot=FALSE)$out
#1] \-22.20036 (31, abnormal outdoor baseline, significantly higher than other values for this participant)
boxplot(HR$airpo_diff)
boxplot(HR$airpo_diff, plot=FALSE)$out
#numeric(0)
boxplot(HR$comb_diff)
boxplot(HR$comb_diff, plot=FALSE)$out
#[1] 17.87586
boxplot(lnssd$nonbio_diff)
boxplot(lnssd$nonbio_diff, plot=FALSE)$out
#[1]  0.3597899 -0.5290823 \-1.0204154 (28, too high a difference than others)
boxplot(lnssd$indo_diff)
boxplot(lnssd$indo_diff, plot=FALSE)$out
#[1] \-2.8487418 (19, abnormal value for indoor, the indoor measurement is only around 1.5) 
# -0.9205854 (28, too high a difference than others)
boxplot(lnssd$outd_diff)
boxplot(lnssd$outd_diff, plot=FALSE)$out
#[1] \-0.7514548 (28, too high a difference than others)
boxplot(lnssd$airpo_diff)
boxplot(lnssd$airpo_diff, plot=FALSE)$out
#[1] \-0.9030459 (28, too high a difference than others)
boxplot(lnssd$comb_diff)
boxplot(lnssd$comb_diff, plot=FALSE)$out
#numeric(0)
boxplot(scl$nonbio_diff)
boxplot(scl$nonbio_diff, plot=FALSE)$out
#[1] \3.115345 (10, too high a difference than others)
boxplot(scl$indo_diff)
boxplot(scl$indo_diff, plot=FALSE)$out
#[1] -1.612541  \2.528371 (10, too high a difference than others)
boxplot(scl$outd_diff)
boxplot(scl$outd_diff, plot=FALSE)$out
#[1] \2.789957 (10, too high a difference than others)
boxplot(scl$airpo_diff)
boxplot(scl$airpo_diff, plot=FALSE)$out
#[1] \3.360759 (35, too high a difference than others)
boxplot(scl$comb_diff)
boxplot(scl$comb_diff, plot=FALSE)$out
#[1] \2.540118 (10, too high a difference than others) -1.313632
boxplot(num_test$non.biophilic)
boxplot(num_test$non.biophilic, plot=FALSE)$out
#[1] 2
boxplot(num_test$indoor)
boxplot(num_test$indoor, plot=FALSE)$out
#numeric(0)
boxplot(num_test$outdoor)
boxplot(num_test$outdoor, plot=FALSE)$out
#[1]  9  4 10  9
boxplot(num_test$air.pollution)
boxplot(num_test$air.pollution, plot=FALSE)$out
#numeric(0)
boxplot(num_test$combined)
boxplot(num_test$combined, plot=FALSE)$out
#numeric(0)
boxplot(AU_ori$nonbiophilic)
boxplot(AU_ori$nonbiophilic, plot=FALSE)$out
#numeric(0)
boxplot(AU_ori$indoor)
boxplot(AU_ori$indoor, plot=FALSE)$out
#numeric(0)
boxplot(AU_ori$outdoor)
boxplot(AU_ori$outdoor, plot=FALSE)$out
#numeric(0)
boxplot(AU_ori$air.pollution)
boxplot(AU_ori$air.pollution, plot=FALSE)$out
#numeric(0)
boxplot(AU_ori$combine)
boxplot(AU_ori$combine, plot=FALSE)$out
#numeric(0)
boxplot(SBP$nonbio_sp_diff)
boxplot(SBP$nonbio_sp_diff, plot=FALSE)$out
#numeric(0)
boxplot(SBP$ind_sp_diff)
boxplot(SBP$ind_sp_diff, plot=FALSE)$out
#[1]  -8  -9 \-34 (31, abnormal post-scene measurement, only 85, 
#                  significantly lower than himself's and others')
boxplot(SBP$outd_sp_diff)
boxplot(SBP$outd_sp_diff, plot=FALSE)$out
#[1] -10
boxplot(SBP$airpo_sp_diff)
boxplot(SBP$airpo_sp_diff, plot=FALSE)$out
#[1] 13
boxplot(SBP$comb_sp_diff)
boxplot(SBP$comb_sp_diff, plot=FALSE)$out
#[1] -10  11
boxplot(DBP$nonbio_dp_diff)
boxplot(DBP$nonbio_dp_diff, plot=FALSE)$out
#[1]  11 -12
boxplot(DBP$ind_dp_diff)
boxplot(DBP$ind_dp_diff, plot=FALSE)$out
#numeric(0)
boxplot(DBP$outd_dp_diff)
boxplot(DBP$outd_dp_diff, plot=FALSE)$out
#numeric(0)
boxplot(DBP$airpo_dp_diff)
boxplot(DBP$airpo_dp_diff, plot=FALSE)$out
#[1] 15
boxplot(DBP$comb_dp_diff)
boxplot(DBP$comb_dp_diff, plot=FALSE)$out
#numeric(0)

#remove the outliers, for the number test, there are too many outliers, and the outliers are actually
#not too far away from the main body. In the meantime, they represent more of personal ability. So
#we decided not to delete the outliers in number tests
rmssd$nonbio_diff <- replace(rmssd$nonbio_diff, rmssd$nonbio_diff %in% 
                               boxplot(rmssd$nonbio_diff, plot=FALSE)$out[2], NA)
rmssd$indo_diff <- replace(rmssd$indo_diff, rmssd$indo_diff %in% 
                             boxplot(rmssd$indo_diff, plot=FALSE)$out[c(2,3)], NA)
column <- rmssd$outd_diff
rmssd$outd_diff <- replace(column, column %in% 
                             boxplot(column, plot=FALSE)$out[c(2)], NA)
column <- rmssd$airpo_diff
rmssd$airpo_diff <- replace(column, column %in% 
                              boxplot(column, plot=FALSE)$out[c(4)], NA)
column <- HR$outd_diff
HR$outd_diff <- replace(column, column %in% 
                          boxplot(column, plot=FALSE)$out[c(1)], NA)
column <- lnssd$nonbio_diff
lnssd$nonbio_diff <- replace(column, column %in% 
                               boxplot(column, plot=FALSE)$out[c(3)], NA)
column <- lnssd$indo_diff
lnssd$indo_diff <- replace(column, column %in% 
                             boxplot(column, plot=FALSE)$out[c(1, 2)], NA)
column <- lnssd$outd_diff
lnssd$outd_diff <- replace(column, column %in% 
                             boxplot(column, plot=FALSE)$out[c(1)], NA)
column <- lnssd$airpo_diff
lnssd$airpo_diff <- replace(column, column %in% 
                              boxplot(column, plot=FALSE)$out[c(1)], NA)
column <- scl$nonbio_diff
scl$nonbio_diff <- replace(column, column %in% 
                             boxplot(column, plot=FALSE)$out[c(1)], NA)
column <- scl$indo_diff
scl$indo_diff <- replace(column, column %in% 
                           boxplot(column, plot=FALSE)$out[c(2)], NA)
column <- scl$outd_diff
scl$outd_diff <- replace(column, column %in% 
                           boxplot(column, plot=FALSE)$out[c(1)], NA)
column <- scl$airpo_diff
scl$airpo_diff <- replace(column, column %in% 
                            boxplot(column, plot=FALSE)$out[c(1)], NA)
column <- scl$comb_diff
scl$comb_diff <- replace(column, column %in% 
                           boxplot(column, plot=FALSE)$out[c(1)], NA)
column <- SBP$ind_sp_diff
SBP$ind_sp_diff <- replace(column, column %in% 
                             boxplot(column, plot=FALSE)$out[c(3)], NA)
boxplot(rmssd$nonbio_diff, plot=FALSE)$out
boxplot(rmssd$nonbio_diff)

#continue processing, preparation for linear regression by melting values in each column
melt_num <- melt(num_test,id.vars = 1)
melt_au <- melt(AU_ori,id.vars = 1)
melt_rmssd <- melt(select(rmssd, c("ID", "nonbio_diff", "indo_diff", "outd_diff", "airpo_diff",
                                   "comb_diff")),id.vars = 1)
melt_lnssd <- melt(select(lnssd, c("ID", "nonbio_diff", "indo_diff", "outd_diff", "airpo_diff",
                                   "comb_diff")),id.vars = 1)
melt_HR <- melt(select(HR, c("ID", "nonbio_diff", "indo_diff", "outd_diff", "airpo_diff",
                                "comb_diff")),id.vars = 1)
melt_scl <- melt(select(scl, c("ID", "nonbio_diff", "indo_diff", "outd_diff", "airpo_diff",
                                 "comb_diff")),id.vars = 1)
melt_SBP <- melt(SBP,id.vars = 1)
melt_DBP <- melt(DBP,id.vars = 1)

#create merged tables for physiological indicators and cognitive assessments with surveys,
#This step was meant for the linear models with factors in surveys considered. However,
#the factors are only used for cognitive assessments, but not for physiological indicators
survey_au <- merge(survey_model,
                   melt_au,
                   by="ID",
                   all.x= T)
survey_num <- merge(survey_model,
                   melt_num,
                   by="ID",
                   all.x= T)
survey_rmssd <- merge(survey_model,
                   melt_rmssd,
                   by="ID",
                   all.x= T)
survey_lnssd <- merge(survey_model,
                      melt_lnssd,
                      by="ID",
                      all.x= T)
survey_HR <- merge(survey_model,
                      melt_HR,
                      by="ID",
                      all.x= T)
survey_scl <- merge(survey_model,
                      melt_scl,
                      by="ID",
                      all.x= T)
survey_SBP <- merge(survey_model,
                      melt_SBP,
                      by="ID",
                      all.x= T)
survey_DBP <- merge(survey_model,
                      melt_DBP,
                      by="ID",
                      all.x= T)

#Relevel the primary variable to be non.biophilic so that the linear models use it
#as the reference
survey_au <- survey_au %>% mutate(variable2 = relevel(variable,ref="nonbiophilic"))
survey_num <- survey_num %>% mutate(variable2 = relevel(variable,ref="non.biophilic"))
survey_rmssd <- survey_rmssd %>% mutate(variable2 = relevel(variable,ref="nonbio_diff"))
survey_lnssd <- survey_lnssd %>% mutate(variable2 = relevel(variable,ref="nonbio_diff"))
survey_HR <- survey_HR %>% mutate(variable2 = relevel(variable,ref="nonbio_diff"))
survey_scl <- survey_scl %>% mutate(variable2 = relevel(variable,ref="nonbio_diff"))
survey_SBP <- survey_SBP %>% mutate(variable2 = relevel(variable,ref="nonbio_sp_diff"))
survey_DBP <- survey_DBP %>% mutate(variable2 = relevel(variable,ref="nonbio_dp_diff"))


# AU score ---- Trials to normalize the data, but not used in the final analysis
colnames(survey)
table1(~Your.age+Your.gender+health_condition+caffeinated.beverage+sleep.quality ,survey)

colnames(AU)
AU_2 <- melt(AU, id.vars=1)

survey_au <- merge(survey,
                   AU_2,
                   by="ID")

colnames(survey_au)

mixed <- lmer(value~variable+caffeinated.beverage+(1|ID),data=survey_au)
install.packages("lmerTest")
install.packages("statmod")
Anova(mixed, test="F") 
confint.default()
anova(mixed)
summary(mixed)


summary(geeglm(value~variable+caffeinated.beverage+Your.gender,
            family=gaussian,
            id=ID,
            data=survey_au))
summary(geeglm(value~variable,
               family=gaussian,
               id=ID,
               data=survey_au))

summary(lm(value~variable,
               data=survey_au))

boxplot(value~variable,
        data=survey_au)

summary(AU_ori)

AU_2 <- melt(AU, id.vars=1)
AU_ori_2 <- melt(AU_ori,id.vars=1)

survey_au2 <- merge(survey,
                   AU_ori_2,
                   by="ID")

summary(geeglm(value~variable+caffeinated.beverage+Your.gender,
               family=gaussian,
               id=ID,
               data=survey_au2))


#examine the learning effect
num_test <- read.csv("number test no-order.csv")
newnumcol <- c("ID",1,2,3,4,5)
colnames(num_test)<- newnumcol
melt_num <- melt(num_test,id.vars = 1)
class(melt_num$variable) <- "integer"
class(melt_num$variable)
linear_num <- lm(value ~ variable, melt_num)
summary(linear_num)
plot(linear_num)
with(melt_num,plot(variable, value, xlab="Environment in Order", ylab = "Test Score", 
                   main ="Learning Effect in Verbal Backward Digit Span Task"))
abline(linear_num)
#tidy the values with broom::tidy
broom::tidy(linear_num, conf.int = TRUE)

au_test_no_order <- read.csv("AU z-score in original order.csv")
newnumcol <- c("ID",1,2,3,4,5)
colnames(au_test_no_order)<- newnumcol
melt_au_no_order <- melt(au_test_no_order,id.vars = 1)
class(melt_au_no_order$variable) <- "integer"
class(melt_au_no_order$variable)
linear_au <- lm(value ~ variable, melt_au_no_order)
summary(linear_au)
with(melt_au_no_order,plot(variable, value, xlab="Environment in Order", ylab = "Test Score", 
                   main ="Learning Effect in Alternative Use Test"))
abline(linear_au)
broom::tidy(linear_au, conf.int = TRUE)

#write_csv(melt_num, path = "melt_num.csv")


#linear model with all factors considered. We processed all the data at first, but
#only used the models for cognitive assessments with all factors because literature
#empirically evaluated more impacts from previous physical conditions on cognitive 
#function but not physiological indicators.
col_name <- c("indoor", "outdoor", "airpollution", "combined")
hypf <- "("
hypb <- ")"
colnames(survey_au)
fit_au <- geeglm(value~variable2+Your.gender+short.sighted+
               health_condition+caffeinated.beverage+sleep.quality,
               family=gaussian,
               id=ID,
               data=survey_au)
summary(fit_au)
broom::tidy(fit_au, conf.int = TRUE)
fit<- fit_au
res_au <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_au
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                          res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_au$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
#output the variables we need
res_au <- select(res_au, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))


fit_num <- geeglm(value~variable2+Your.gender+short.sighted+
                   health_condition+caffeinated.beverage+sleep.quality,
                 family=gaussian,
                 id=ID,
                 data=survey_num)
summary(fit_num)
broom::tidy(fit_num, conf.int = TRUE)
fit<- fit_num
res_num <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_num
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_num$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_num <- select(res_num, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))

fit_rmssd <- geeglm(value~variable2+Your.gender+short.sighted+
                    health_condition+caffeinated.beverage+sleep.quality,
                  family=gaussian,
                  id=ID,
                  data=survey_rmssd)
summary(fit_rmssd)
broom::tidy(fit_rmssd, conf.int = TRUE)
fit<- fit_rmssd
res_rmssd <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_rmssd
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_rmssd$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_rmssd <- select(res_rmssd, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))
colnames(res_rmssd) <- c("col_name", "sloperm", "intrm")

fit_lnssd <- geeglm(value~variable2+Your.gender+short.sighted+
                      health_condition+caffeinated.beverage+sleep.quality,
                    family=gaussian,
                    id=ID,
                    data=survey_lnssd)
summary(fit_lnssd)
broom::tidy(fit_lnssd, conf.int = TRUE)
fit<- fit_lnssd
res_lnssd <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_lnssd
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_lnssd$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_lnssd <- select(res_lnssd, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))
colnames(res_lnssd) <- c("col_name", "slopeln", "intln")

fit_HR <- geeglm(value~variable2+Your.gender+short.sighted+
                      health_condition+caffeinated.beverage+sleep.quality,
                    family=gaussian,
                    id=ID,
                    data=survey_HR)
summary(fit_HR)
broom::tidy(fit_HR, conf.int = TRUE)
fit<- fit_HR
res_HR <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_HR
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_HR$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_HR <- select(res_HR, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))
colnames(res_HR) <- c("col_name", "slopehr", "inthr")

fit_scl <- geeglm(value~variable2+Your.gender+short.sighted+
                      health_condition+caffeinated.beverage+sleep.quality,
                    family=gaussian,
                    id=ID,
                    data=survey_scl)
summary(fit_scl)
broom::tidy(fit_scl, conf.int = TRUE)
fit<- fit_scl
res_scl <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_scl
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_scl$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_scl <- select(res_scl, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))
colnames(res_scl) <- c("col_name", "slopescl", "intscl")

fit_SBP <- geeglm(value~variable2+Your.gender+short.sighted+
                      health_condition+caffeinated.beverage+sleep.quality,
                    family=gaussian,
                    id=ID,
                    data=survey_SBP)
summary(fit_SBP)
broom::tidy(fit_SBP, conf.int = TRUE)
fit<- fit_SBP
res_SBP <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_SBP
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_SBP$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_SBP <- select(res_SBP, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))
colnames(res_SBP) <- c("col_name", "slopesbp", "intsbp")

fit_DBP <- geeglm(value~variable2+Your.gender+short.sighted+
                      health_condition+caffeinated.beverage+sleep.quality,
                    family=gaussian,
                    id=ID,
                    data=survey_DBP)
summary(fit_DBP)
broom::tidy(fit_DBP, conf.int = TRUE)
fit<- fit_DBP
res_DBP <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_DBP
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_DBP$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_DBP <- select(res_DBP, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))
colnames(res_DBP) <- c("col_name", "slopedbp", "intdbp")

#merge all the data to one document for the models with all factors considered
Linear_all <- merge(res_SBP, res_DBP,
                    by="col_name",
                    all.x= T)

Linear_all <- merge(Linear_all, res_HR,
                    by="col_name",
                    all.x= T)

Linear_all <- merge(Linear_all, res_rmssd,
                    by="col_name",
                    all.x= T)

Linear_all <- merge(Linear_all, res_lnssd,
                    by="col_name",
                    all.x= T)

Linear_all <- merge(Linear_all, res_scl,
                    by="col_name",
                    all.x= T)

Linear_all <- merge(Linear_all, res_num,
                    by="col_name",
                    all.x= T)

Linear_all <- merge(Linear_all, res_au,
                    by="col_name",
                    all.x= T)

write_csv(Linear_all, path = "Linear_all.csv")


#linear model with no factor considered. Again, we first processed both physiological indicators
#and cognitive functions, but at last only the physiological indicators used models with no
#factors considered because cognitive assessments can be influenced by prior physiological conditions
col_name <- c("indoor", "outdoor", "airpollution", "combined")
hypf <- "("
hypb <- ")"
colnames(survey_au)
fit_au <- geeglm(value~variable2,
                 family=gaussian,
                 id=ID,
                 data=survey_au)
summary(fit_au)
broom::tidy(fit_au, conf.int = TRUE)
fit<- fit_au
res_au <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_au
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_au$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_au <- select(res_au, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))

fit_num <- geeglm(value~variable2,
                  family=gaussian,
                  id=ID,
                  data=survey_num)
summary(fit_num)
broom::tidy(fit_num, conf.int = TRUE)
fit<- fit_num
res_num <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                      hypf,
                      broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                      broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_num
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_num$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_num <- select(res_num, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                             "confinter"))

fit_rmssd <- geeglm(value~variable2,
                    family=gaussian,
                    id=ID,
                    data=survey_rmssd)
summary(fit_rmssd)
broom::tidy(fit_rmssd, conf.int = TRUE)
fit<- fit_rmssd
res_rmssd <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                        hypf,
                        broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                        broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_rmssd
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_rmssd$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_rmssd <- select(res_rmssd, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                                 "confinter"))
colnames(res_rmssd) <- c("col_name", "sloperm", "intrm")

fit_lnssd <- geeglm(value~variable2,
                    family=gaussian,
                    id=ID,
                    data=survey_lnssd)
summary(fit_lnssd)
broom::tidy(fit_lnssd, conf.int = TRUE)
fit<- fit_lnssd
res_lnssd <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                        hypf,
                        broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                        broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_lnssd
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_lnssd$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_lnssd <- select(res_lnssd, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                                 "confinter"))
colnames(res_lnssd) <- c("col_name", "slopeln", "intln")

fit_HR <- geeglm(value~variable2,
                 family=gaussian,
                 id=ID,
                 data=survey_HR)
summary(fit_HR)
broom::tidy(fit_HR, conf.int = TRUE)
fit<- fit_HR
res_HR <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                     hypf,
                     broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                     broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_HR
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_HR$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_HR <- select(res_HR, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                           "confinter"))
colnames(res_HR) <- c("col_name", "slopehr", "inthr")

fit_scl <- geeglm(value~variable2,
                  family=gaussian,
                  id=ID,
                  data=survey_scl)
summary(fit_scl)
broom::tidy(fit_scl, conf.int = TRUE)
fit<- fit_scl
res_scl <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                      hypf,
                      broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                      broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_scl
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_scl$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_scl <- select(res_scl, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                             "confinter"))
colnames(res_scl) <- c("col_name", "slopescl", "intscl")

fit_SBP <- geeglm(value~variable2,
                  family=gaussian,
                  id=ID,
                  data=survey_SBP)
summary(fit_SBP)
broom::tidy(fit_SBP, conf.int = TRUE)
fit<- fit_SBP
res_SBP <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                      hypf,
                      broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                      broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_SBP
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_SBP$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_SBP <- select(res_SBP, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                             "confinter"))
colnames(res_SBP) <- c("col_name", "slopesbp", "intsbp")

fit_DBP <- geeglm(value~variable2,
                  family=gaussian,
                  id=ID,
                  data=survey_DBP)
summary(fit_DBP)
broom::tidy(fit_DBP, conf.int = TRUE)
fit<- fit_DBP
res_DBP <- data.frame(col_name, broom::tidy(fit, conf.int = TRUE)$estimate[c(2,3,4,5)], 
                      hypf,
                      broom::tidy(fit, conf.int = TRUE)$conf.low[c(2,3,4,5)], 
                      broom::tidy(fit, conf.int = TRUE)$conf.high[c(2,3,4,5)], hypb)
res <- res_DBP
res$confinter <- paste(res$broom..tidy.fit..conf.int...TRUE..conf.low.c.2..3..4..5.., 
                       res$broom..tidy.fit..conf.int...TRUE..conf.high.c.2..3..4..5.., sep=", ")
res_DBP$confinter <- paste(res$hypf, res$confinter, res$hypb, sep="")
res_DBP <- select(res_DBP, c("col_name", "broom..tidy.fit..conf.int...TRUE..estimate.c.2..3..4..5..",
                             "confinter"))
colnames(res_DBP) <- c("col_name", "slopedbp", "intdbp")

Linear_none <- merge(res_SBP, res_DBP,
                    by="col_name",
                    all.x= T)

Linear_none <- merge(Linear_none, res_HR,
                    by="col_name",
                    all.x= T)

Linear_none <- merge(Linear_none, res_rmssd,
                    by="col_name",
                    all.x= T)

Linear_none <- merge(Linear_none, res_lnssd,
                    by="col_name",
                    all.x= T)

Linear_none <- merge(Linear_none, res_scl,
                    by="col_name",
                    all.x= T)

Linear_none <- merge(Linear_none, res_num,
                    by="col_name",
                    all.x= T)

Linear_none <- merge(Linear_none, res_au,
                    by="col_name",
                    all.x= T)

write_csv(Linear_none, path = "Linear_none.csv")



#effect size calculated by standardized regression coefficient
lm.beta(fit_SBP)
lm.beta(fit_DBP)
lm.beta(fit_HR)
lm.beta(fit_rmssd)
lm.beta(fit_lnssd)
lm.beta(fit_scl)
lm.beta(fit_num)
lm.beta(fit_au)

#after-hand power analysis using the largest effect size calculated above
power.t.test(d = 0.25, sig.level = 0.05, power = 0.8, alternative = "one.sided")
pwr.f2.test(u = 4, f2 = 0.163, sig.level = 0.05, power = 0.8)
pwr.f2.test(u = 4, v=30-4-1, f2=0.163, sig.level = 0.05)
