## Generalized

library(tidyverse)
library(glm2)
library(emmeans)
library(ordinal)
library(MuMIn)
library(car)

data <- read_csv("Mall.csv")

#recoding categorical stage names to numbers
index <- c()
for(i in 1:nrow(data)) {
  if(data$ds[i] == "early active") {
    index[i] <- 1
  }
  if(data$ds[i] == "late active") {
    index[i] <- 2
  }
  if(data$ds[i] == "mature") {
    index[i] <- 3
  }
  if(data$ds[i] == "spawned") {
    index[i] <- 4
  }
  if(data$ds[i] == "reabsorbing") {
    index[i] <- 5
  }
}
data <- cbind(data, index) 

#setting factors
data$month <- factor(Mall$month, levels=c("june", "july", "august"))
data$location <- factor(Mall$location, levels=c("HH", "SB", "PGB"))
data$strain <- factor(Mall$strain, levels=c("wild", "hatchery", "aquaculture"))
data$sex <- factor(Mall$sex, levels=c("f", "m"))



#building models
m1 <- glm(index ~ location + month, data=data)

m2 <- glm(index ~ location + month + location*month, data=data)

m3 <- glm(index ~ location + month + strain, data=data)

m4 <- glm(index ~ location + month + strain + month*strain, data=data)

m5 <- glm(index ~ location + month + strain + location*month, data=data)

m6 <- glm(index ~ location + month + strain + location*strain, data=data)

m7 <- glm(index ~ location + month + strain + location*month + location*strain + month*strain, data=data)

#models with sex variable
m8 <- glm(index ~ location + month + sex, data=data)
m9 <- glm(index ~ location + month + location*month + sex, data=data)
m10 <- glm(index ~ location + month + strain + sex, data=data)
m11 <- glm(index ~ location + month + strain + location*month + location*strain + month*strain + sex, data=data)


model.sel(m1, m2,m3,m4,m5,m6,m7,m8,m9,m10,m11, rank="AIC")
model.sel(m1,m2,m3,m4,m5,m6,m7, rank="AIC")

#post hoc test
emmeans(m7, pairwise ~ location | month+strain)
emmeans(m7, pairwise ~ month | location +strain)

# check assumptions for linear models

#linearity of data
plot(m7, 1) 
#no pattern in residual plot- assume linear relationship between predictor & outcome vars

#homogeneity of variance
plot(m7, 3)

#normality of results
plot(m7, 2) #there are a few outliers
plot(m7, 5) #there is only 1 outlier that exceeds 3 standard deviations
