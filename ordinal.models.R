#Kaili Gregory
#Ordinal logistic models
#Hudson River Estuary oyster project

library(ordinal)
library(MuMIn)
library(emmeans)
library(dplyr)

################## 2018 MODELS #############################
Mall <- read.csv("Mall.csv")
View(Mall) 

Mall$month <- factor(Mall$month, levels=c("june", "july", "august"))
Mall$ds <- factor(Mall$ds, levels=c("early active", "late active", "mature", "spawned", "reabsorbing"), ordered=T)
Mall$location <- factor(Mall$location, levels=c("HH", "SB", "PGB"))
Mall$strain <- factor(Mall$strain, levels=c("wild", "hatchery", "aquaculture"))
Mall$sex <- factor(Mall$sex, levels=c("f", "m"))

m1 <- clm(ds ~ location + month, data=Mall, Hess=T)

m2 <- clm(ds ~ location + month + location*month, data=Mall, Hess=T)

m3 <- clm(ds ~ location + month + strain, data=Mall, Hess=T)

m4 <- clm(ds ~ location + month + strain + month*strain, data=Mall, Hess=T)

m5 <- clm(ds ~ location + month + strain + location*month, data=Mall, Hess=T)

m6 <- clm(ds ~ location + month + strain + location*strain, data=Mall, Hess=T)

m7 <- clm(ds ~ location + month + strain + location*month + location*strain + month*strain, data=Mall, Hess=T)

#models with sex variable
m8 <- clm(ds ~ location + month + sex, data=Mall, Hess=T)
m9 <- clm(ds ~ location + month + location*month + sex, data=Mall, Hess=T)
m10 <- clm(ds ~ location + month + strain + sex, data=Mall, Hess=T)
m11 <- clm(ds ~ location + month + strain + location*month + location*strain + month*strain + sex, data=Mall, Hess=T)
#model 7 is still the best model, but followed closely by the above model 11

#use AICc if # observations(n) : # of parameters(k) < 40
nrow(Mall)/7 #7 is max number of parameters in the model list
# = 63.85, so just use AIC

model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11, rank="AIC")

#post hoc for best model
emmeans(m7, pairwise ~ location | month+strain)
emmeans(m7, pairwise ~ month | location +strain)
emmeans(m7, pairwise ~ strain | location +month)


nominal_test(m7)


#emmip(m7, ds.f ~ location.f | month.f+strain.f, CIs = TRUE, mode='prob')
#predicted relationship visualization


################# 2019 MODELS #########################
M19 <- read.csv("M19.csv")
View(M19)

M19$month <- factor(M19$month, levels=c("July", "Sept"))
M19$dev.stage <- factor(M19$dev.stage, levels=c("early active", "late active", "mature", "spawned", "reabsorbing"), ordered=T)
M19$location <- factor(M19$location, levels=c("IRV", "HH", "PGB", "KCC"))
M19$strain <- factor(M19$strain, levels=c("wild", "hatchery", "aquaculture"))

m1.19 <- clm(dev.stage ~ location + month, data=M19, Hess=T)

m2.19 <- clm(dev.stage ~ location + month + location*month, data=M19, Hess=T)

m3.19 <- clm(dev.stage ~ location + month+ strain, data=M19, Hess=T)

m4.19 <- clm(dev.stage ~ location + month + strain + month*strain, data=M19, Hess=T)

m5.19 <- clm(dev.stage ~ location + month + strain + location*month, data=M19, Hess=T)

m6.19 <- clm(dev.stage ~ location + month + strain + location*month + month*strain, data=M19, Hess=T)

#use AICc if # observations(n) : # of parameters(k) < 40
nrow(M19)/5 #5 is max number of parameters in the model list
# = 48, so just use AIC

model.sel(m1.19,m2.19,m3.19,m4.19,m5.19,m6.19, rank="AIC" )
summary(m4.19)

#post hoc for best model
emmeans(m4.19, pairwise ~ location | month+ strain)
#comparing locations at every month strain combo outputs the same p values 
#because location does not have an interaction effect in the model

emmeans(m4.19, pairwise ~ location)

emmeans(m4.19, pairwise ~ strain | month)

#models and data WITHOUT aquaculture
M19 <- data.frame(M19)
w.M19 <- subset(M19, strain!="aquaculture")
View(w.M19)

w.M19$month <- factor(w.M19$month, levels=c("July", "Sept"))
w.M19$dev.stage <- factor(w.M19$dev.stage, levels=c("early active", "late active", "mature", "spawned", "reabsorbing"), ordered=T)
w.M19$location <- factor(w.M19$location, levels=c("IRV", "HH", "PGB", "KCC"))
w.M19$strain <- factor(w.M19$strain, levels=c("wild", "hatchery", "aquaculture"))

w.m1.19 <- clm(dev.stage ~ location + month, data=w.M19, Hess=T)

w.m2.19 <- clm(dev.stage ~ location + month + location*month, data=w.M19, Hess=T)

w.m3.19 <- clm(dev.stage ~month, data=w.M19, Hess=T)


#use AICc if # observations(n) : # of parameters(k) < 40
nrow(w.M19)/3 #5 is max number of parameters in the model list
# = 53.33, so just use AIC

#model selection no longer included in the paper, only model 2 results
#model.sel(w.m1.19, w.m2.19, w.m3.19, rank="AIC" )
#model.avg(w.m1.19, w.m3.19)

#emmeans(w.m1.19, pairwise ~ w.location.19)

#emmeans(w.m1.19, pairwise ~ w.month.19)

emmeans(w.m2.19, pairwise ~ month + location)

emmeans(w.m2.19, pairwise ~ month)

emmeans(w.m2.19, pairwise ~ month | location)

