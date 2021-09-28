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

month.f <- factor(Mall$month, levels=c("june", "july", "august"))
ds.f <- factor(Mall$ds, levels=c("early active", "late active", "mature", "spawned", "reabsorbing"), ordered=T)
location.f <- factor(Mall$location, levels=c("HH", "SB", "PGB"))
strain.f <- factor(Mall$strain, levels=c("wild", "hatchery", "aquaculture"))
sex <- factor(Mall$sex, levels=c("f", "m"))

m1 <- clm(ds.f ~ location.f + month.f, data=Mall, Hess=T)

m2 <- clm(ds.f ~ location.f + month.f + location.f*month.f, data=Mall, Hess=T)

m3 <- clm(ds.f ~ location.f + month.f + strain.f, data=Mall, Hess=T)

m4 <- clm(ds.f ~ location.f + month.f + strain.f + month.f*strain.f, data=Mall, Hess=T)

m5 <- clm(ds.f ~ location.f + month.f + strain.f + location.f*month.f, data=Mall, Hess=T)

m6 <- clm(ds.f ~ location.f + month.f + strain.f + location.f*strain.f, data=Mall, Hess=T)

m7 <- clm(ds.f ~ location.f + month.f + strain.f + location.f*month.f + location.f*strain.f + month.f*strain.f, data=Mall, Hess=T)

#models with sex variable
m8 <- clm(ds.f ~ location.f + month.f + sex, data=Mall, Hess=T)
m9 <- clm(ds.f ~ location.f + month.f + location.f*month.f + sex, data=Mall, Hess=T)
m10 <- clm(ds.f ~ location.f + month.f + strain.f + sex, data=Mall, Hess=T)
m11 <- clm(ds.f ~ location.f + month.f + strain.f + location.f*month.f + location.f*strain.f + month.f*strain.f + sex, data=Mall, Hess=T)
#model 7 is still the best model, but followed closely by the above model 11

m12 <- clm(ds.f ~  month.f, data=Mall, Hess=T)

#use AICc if # observations(n) : # of parameters(k) < 40
nrow(Mall)/7 #7 is max number of parameters in the model list
# = 63.23, so just use AIC

model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12, rank="AIC")

#post hoc for best model
emmeans(m7, pairwise ~ location.f | month.f+strain.f)

emmip(m7, ds.f ~ location.f | month.f+strain.f, CIs = TRUE, mode='prob') + #precticted relationship visualization
  scale_color_manual(values=c("green", "pink", "purple", "orange", "aquamarine"))


################# 2019 MODELS #########################
M19 <- read.csv("M19.csv")
View(M19)

month.19 <- factor(M19$month, levels=c("July", "Sept"))
ds.19 <- factor(M19$dev.stage, levels=c("early active", "late active", "mature", "spawned", "reabsorbing"), ordered=T)
location.19 <- factor(M19$location, levels=c("IRV", "HH", "PGB", "KCC"))
strain.19 <- factor(M19$strain, levels=c("wild", "hatchery", "aquaculture"))

m1.19 <- clm(ds.19 ~ location.19 + month.19, data=M19, Hess=T)

m2.19 <- clm(ds.19 ~ location.19 + month.19 + location.19*month.19, data=M19, Hess=T)

m3.19 <- clm(ds.19 ~ location.19 + month.19+ strain.19, data=M19, Hess=T)

m4.19 <- clm(ds.19 ~ location.19 + month.19 + strain.19 + month.19*strain.19, data=M19, Hess=T)

m5.19 <- clm(ds.19 ~ location.19 + month.19 + strain.19 + location.19*month.19, data=M19, Hess=T)

m6.19 <- clm(ds.19 ~ location.19 + month.19 + strain.19 + location.19*month.19 + month.19*strain.19, data=M19, Hess=T)

#use AICc if # observations(n) : # of parameters(k) < 40
nrow(M19)/5 #5 is max number of parameters in the model list
# = 48, so just use AIC

model.sel(m1.19,m2.19,m3.19,m4.19,m5.19,m6.19, rank="AIC" )
summary(m4.19)

#post hoc for best model
emmeans(m4.19, pairwise ~ location.19 | month.19+ strain.19)
#comparing locations at every month strain combo outputs the same p values 
#because location does not have an interaction effect in the model

emmeans(m4.19, pairwise ~ location.19)

emmeans(m4.19, pairwise ~ strain.19 | month.19)

#models and data WITHOUT aquaculture
M19 <- data.frame(M19)
w.M19 <- subset(M19, strain!="aquaculture")
View(w.M19)

w.month.19 <- factor(w.M19$month, levels=c("July", "Sept"))
w.ds.19 <- factor(w.M19$dev.stage, levels=c("early active", "late active", "mature", "spawned", "reabsorbing"), ordered=T)
w.location.19 <- factor(w.M19$location, levels=c("IRV", "HH", "PGB", "KCC"))
w.strain.19 <- factor(w.M19$strain, levels=c("wild", "hatchery", "aquaculture"))

w.m1.19 <- clm(w.ds.19 ~ w.location.19 + w.month.19, data=w.M19, Hess=T)

w.m2.19 <- clm(w.ds.19 ~ w.location.19 + w.month.19 + w.location.19*w.month.19, data=w.M19, Hess=T)

w.m3.19 <- clm(w.ds.19 ~w.month.19, data=w.M19, Hess=T)


#use AICc if # observations(n) : # of parameters(k) < 40
nrow(w.M19)/3 #5 is max number of parameters in the model list
# = 53.33, so just use AIC

model.sel(w.m1.19, w.m2.19, w.m3.19, rank="AIC" )
model.avg(w.m1.19, w.m3.19)

emmeans(w.m1.19, pairwise ~ w.location.19)

emmeans(w.m1.19, pairwise ~ w.month.19)

emmeans(w.m2.19, pairwise ~ w.month.19 + w.location.19)

#### non-ordinal model for 2019 data #####

july.w.M19 <- subset(w.M19, month =="July") #subset for July data
head(july.w.M19)
count()

kruskal.test(dev.stage ~ location, data=july.w.M19) #p=0,0583
pairwise.wilcox.test(july.w.M19$dev.stage, july.w.M19$location, p.adjust.method = "BH")

sept.w.M19 <- subset(w.M19, month =="Sept") #subset for Sept data
head(sept.w.M19)
