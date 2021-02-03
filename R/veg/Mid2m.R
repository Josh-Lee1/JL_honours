library(tidyverse)

veg<- read.csv("Data/Processed/vegfull.csv")
veg$Treatment <- as.factor(veg$Treatment)

#Midstorey
## perform ANOVA
Mid.aov <- aov(Growth..2m ~ Treatment, data = veg)
summary(Mid.aov)

Mid.aov <- aov(Growth..2m ~ Formation * Fire, data = veg)
summary(Mid.aov)

##look at interaction
interaction.plot(veg$Fire, veg$Formation, veg$Growth..2m)

##test assumptions
hist(Mid.aov$residuals)
plot(Mid.aov,which=2)
plot(Mid.aov,which=1)

##Plot
boxplot(
  Growth..2m ~ Formation*Fire,data = veg,
  names = c("Dry Sclerophyll Burnt", "Rainforest Burnt", "Dry Sclerophyll Unburnt", "Rainforest Unburnt"),
  ylab="Height above 2m (m)",xlab="Treatment",ylim=c(0,40))
