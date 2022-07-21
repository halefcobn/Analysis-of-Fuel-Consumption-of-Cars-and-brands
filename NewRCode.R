library(readxl)
HgwDATA <- read_excel("//Users/halef/new/R/highwaydata.xlsx",
                 col_types = c("numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric"))
View(HgwDATA)
summary(HgwDATA)
plot(HgwDATA)
#histograms of independet variables
hist(HgwDATA$Engine_Size)
hist(HgwDATA$Cylinders)
hist(HgwDATA$CO2_Emissions)
hist(HgwDATA$Smog_Rating)
hist(HgwDATA$CO2_Rating)
hist(HgwDATA$Fuel_Type)
#-------------------------
pairs(HgwDATA)
plot(HgwDATA)
#paired t test between Fuel Consumption and independent variables
t.test(HgwDATA$`Engine_Size`,HgwDATA$Fuel_ConsumptionHGW,paired = T)
t.test(HgwDATA$Cylinders,HgwDATA$Fuel_ConsumptionHGW,paired = T)
t.test(HgwDATA$CO2_Emissions,HgwDATA$Fuel_ConsumptionHGW,paired = T)
t.test(HgwDATA$Smog_Rating,HgwDATA$Fuel_ConsumptionHGW,paired = T)
t.test(HgwDATA$CO2_Rating,HgwDATA$Fuel_ConsumptionHGW,paired = T)
t.test(HgwDATA$Fuel_Type,HgwDATA$Fuel_ConsumptionHGW,paired = T)

#correlation between fuel consumption and independent variables
#WITH 95% confidence level
cor.test(HgwDATA$Engine_Size,HgwDATA$Fuel_ConsumptionHGW,conf.level = 0.95)
cor.test(HgwDATA$Cylinders,HgwDATA$Fuel_ConsumptionHGW,conf.level = 0.95)
cor.test(HgwDATA$CO2_Emissions,HgwDATA$Fuel_ConsumptionHGW,conf.level = 0.95)
cor.test(HgwDATA$CO2_Rating,HgwDATA$Fuel_ConsumptionHGW,conf.level = 0.95)
cor.test(HgwDATA$Smog_Rating,HgwDATA$Fuel_ConsumptionHGW,conf.level = 0.95)
cor.test(HgwDATA$Fuel_Type,HgwDATA$Fuel_ConsumptionHGW,conf.level = 0.95)
#-------------------------


LMMODEL <- lm(HgwDATA$Fuel_ConsumptionHGW~HgwDATA$Engine_Size+HgwDATA$Cylinders)
summary(LMMODEL)
anova(LMMODEL)


LMMODEL3 <- lm(HgwDATA$Fuel_ConsumptionHGW~Cylinders,data = HgwDATA)
summary(LMMODEL)
anova(LMMODEL)

#-------------------------
install.packages("leaps")
library(leaps)


alldata <- lm(HgwDATA$Fuel_ConsumptionHGW~HgwDATA$Engine_Size+HgwDATA$Cylinders+HgwDATA$CO2_Emissions+HgwDATA$CO2_Rating+HgwDATA$Smog_Rating+HgwDATA$Fuel_Type)
summary(alldata)
anova(alldata)
plot(alldata)
regfordata <- regsubsets(Fuel_ConsumptionHGW ~  Engine_Size  + Cylinders + CO2_Emissions + Smog_Rating +CO2_Rating + Fuel_Type, data = HgwDATA, method="exhaustive", nvmax=2)
summary(regfordata)
summary(regfordata)$adjr2


#-------------------------
summary(regfordata)$adjr2
#higher is better
summary(regfordata)$cp
#lower better
summary(regfordata)$bic
#lower better
#-------------------------
summary(alldata)$cov.unscaled
#-------------------------
plot(alldata)
#-------------------------
qqnorm(summary(alldata)$residuals)



Fuel_Consumption = -0.42 + (0.08*Engine_Size) + (-0.17*Cylinders) + (0.03*CO2_Emissions) + (0.13*Smog_Rating)- 3
plot(Fuel_Consumption.lm)
require(ggplot2)
library(ggplot2)
ggplot(HgwDATA,aes(y=Engine_Size,Cylinders,CO2_Emissions,Smog_Rating,CO2_Emissions,
                   CO2_Rating, Fuel_Type, x=Fuel_Consumption))+geom_point()+geom_smooth(method="lm")

library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("cowplot")
library(cowplot)

ggplot(HgwDATA,aes(y=Engine_Size,Cylinders,CO2_Emissions,Smog_Rating,x=Fuel_ConsumptionHGW,color=factor(DM)))+geom_point()+stat_smooth(method="lm",se=FALSE)

fit4 <- lm(Fuel_ConsumptionHGW~ Engine_Size * Cylinders + I(Smog_Rating^2) , data = HgwDATA)
summary(fit4)
plot(fit4)


equation3=function(x){coef(fit4)[2]*x+coef(fit4)[1]}
equation4=function(x){coef(fit4)[2]*x+coef(fit4)[1]+coef(fit4)[3]}

ggplot(HgwDATA,aes(y=Fuel_ConsumptionHGW,x=Engine_Size, color=Cylinders)+geom_point()+
  stat_function(fun=equation3,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation4,geom="line",color=scales::hue_pal()(2)[1])
  install.packages(ggPredict)
  library(ggeffects)
  ggeffect(fit4,se=TRUE,interactive=TRUE)
  require(ggplot2)
  library(ggplot2)
  ggplot(HgwDATA,aes(y=Fuel_ConsumptionHGW,x=Engine_Size))+geom_point()+geom_smooth(method="lm")

  model1=lm(Fuel_ConsumptionHGW~Cylinders*CO2_Emissions,data=HgwDATA)
  summary(model1)

ggplot(HgwDATA,aes(y=Fuel_ConsumptionHGW,x=Engine_Size, color=Cylinders))+geom_point()+stat_smooth(method="lm",se=FALSE)
install.packages("predict3d")
install.packages("ggeffects")
install.packages("ggiraphExtra")
library(ggiraphExtra)
ggPredict(fit4,se = TRUE,interactive=TRUE)


library(ggPredict)
require(rgl)
library(predict3d)
require(rgl)
ggpredict3d::ggPredict(fit4,se=TRUE,interactive=TRUE)


newData=lm(Fuel_Consumption~Engine_Size+Cylinders+CO2_Emissions+Smog_Rating,data=highwaydata)
summary(newData)
plot(newData)

newData=lm(Fuel_ConsumptionHGW~Engine_Size+Cylinders+CO2_Emissions+Fuel_Type,data=HgwDATA)
summary(newData)
plot(newData)


newModel <- lm(df$Fuel_Consumption ~ df$"Engine_Size"+df$"Cylinders"+df$"CO2_Emissions"+df$"Smog_Rating")
summary(newData)
anova(newData)
library(ggPredict)

fit1=lm(Fuel_ConsumptionHGW~CO2_Emissions*Engine_Size,data=HgwDATA)
summary(fit1)

equation1=function(x){coef(fit1)[2]*x+coef(fit1)[1]}
equation2=function(x){coef(fit1)[2]*x+coef(fit1)[1]+coef(fit1)[3]}

ggplot(HgwDATA,aes(y=Fuel_ConsumptionHGW,x=Engine_Size,color=Cylinders))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

ggPredict(fit1,colorAsFactor=TRUE,interactive=TRUE)

fit2=lm(Fuel_ConsumptionHGW~Engine_Size+Cylinders,data=HgwDATA)
summary(fit2)
ggPredict(fit2,colorAsFactor = TRUE,interactive=TRUE)

#######
###geom_smooth(formula = y ~ x, method = "lm")
ggplot(HgwDATA,aes(y=Fuel_ConsumptionHGW,x=Fuel_Type,color=factor(CO2_Emissions)))+
  geom_point(Fuel_ConsumptionHGW ~ CO2_Emissions, method = "lm")+stat_smooth(method="lm",se=FALSE)

install.packages("TH.data")
require(TH.data)
library(TH.data)

library(dplyr)

ggPredict(fit2,se=TRUE,interactive=TRUE,digits=3)
library(plot3d)
plot3D(Engine_size, Cylinder, CO2_Emissions , col="blue", size=2)

library(visreg)
visreg(highwaydata)
