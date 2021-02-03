library(tidyverse)

veg<- read.csv("Data/Processed/vegfull.csv")
veg$Treatment <- as.factor(veg$Treatment)

#Canopy Cover
## perform ANOVA
CC.aov <- aov(Canopy.Cover ~ Treatment, data = veg)
summary(CC.aov)

CC.aov <- aov(Canopy.Cover ~ Formation * Fire, data = veg)
summary(CC.aov)

##look at interaction
interaction.plot(veg$Fire, veg$Formation, veg$Canopy.Cover)

##test assumptions
hist(CC.aov$residuals)
plot(CC.aov,which=2)
plot(CC.aov,which=1)

##Plot
boxplot(
  Canopy.Cover ~ Formation*Fire,data = veg,
  names = c("Dry Sclerophyll Burnt", "Rainforest Burnt", "Dry Sclerophyll Unburnt", "Rainforest Unburnt"),
  ylab="Canopy Cover (%)",xlab="Treatment",ylim=c(0,100))
