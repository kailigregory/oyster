#Kaili Gregory
#kaili.gregory2012@gmail.com
#Reproductive oyster phenology project

# Diversity Index generalized linear models

library(tidyverse)
library(ggpubr)
library(emmeans)
library(rstatix)
library(car)

data <- read.csv("diversity_index.csv")

#subsetting each year
data18 <- data[data$year=="2018",]
data19 <- data[data$year=="2019",]

#### 2018 models ####
str(data18)
#converting to factors
data18$month <- factor(data18$month, levels=c("june", "july", "august"))
data18$location <- factor(data18$location, levels=c("hh", "sb", "pgb"))
data18$strain <- factor(data18$strain, levels=c("native", "hatchery", "aquaculture"))

#three way anova 
##test assumptions
data18 %>% #no outliers
  group_by(strain, month, location) %>% 
  identify_outliers(diversity)

model  <- lm(diversity ~ strain*month*location, data = data18)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))


headache %>%
  group_by(gender, risk, treatment) %>%
  identify_outliers(pain_score)






## GLM assumptions
#1.independence of data points
#2. correct distribution of residuals
#3. correct specification of variance structure
#4. linear relationship between response and linear predictor

#building models- same formula as ordinal logistic
m1 <- glm(diversity ~ location + month, data=data18, family=Gamma)

m2 <- glm(diversity ~ location + month + location*month, data=data18, family=Gamma)

m3 <- glm(diversity ~ location + month + strain, data=data18, family=Gamma)

m4 <- glm(diversity ~ location + month + strain + month*strain, data=data18, family = Gamma)

m5 <- glm(diversity ~ location + month + strain + location*month, data=data18, family=Gamma)

m6 <- glm(diversity ~ location + month + strain + location*strain, data=data18, family=Gamma)

m7 <- glm(diversity ~ location + month + strain + location*month + location*strain + month*strain, data=data18, family=Gamma)

#model selection
model.sel(m1,m2,m3,m4,m5,m6,m7, rank="AIC") #m4 and m7 top

#post hoc test
emmeans(m4, pairwise ~ location | month+strain)
emmeans(m4, pairwise ~ month | location +strain)


#post hoc test
emmeans(m7, pairwise ~ location | month+strain)
emmeans(m7, pairwise ~ month | location +strain)
emmeans(m7, pairwise ~ strain | location +month)

