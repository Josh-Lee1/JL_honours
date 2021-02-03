library(tidyverse)

#veg <- read.csv("Data/Raw/veg.csv")%>% 
#  filter(Point == 10 | Point == 20 | Point == 30 | Point == 40 | Point == 50)

veg<- read.csv("Data/Processed/vegfull.csv")
veg$Treatment <- as.factor(veg$Treatment)

#Depth
## perform ANOVA
Ldepth.aov <- aov(Litter.Depth ~ Treatment, data = veg)
summary(Ldepth.aov)

Ldepth.aov <- aov(Litter.Depth ~ Formation * Fire, data = veg)
summary(Ldepth.aov)

##look at interaction
interaction.plot(veg$Fire, veg$Formation, veg$Litter.Depth)

##test assumptions
par(mfrow = c(1,2))
hist(Ldepth.aov$residuals)
plot(Ldepth.aov,which=2)
plot(Ldepth.aov,which=1)

##Plot
boxplot(
  Litter.Depth ~ Formation*Fire,data = veg,
  names = c("Dry Sclerophyll Burnt", "Rainforest Burnt", "Dry Sclerophyll Unburnt", "Rainforest Unburnt"),
  ylab="Litter Depth (mm)",xlab="Treatment",ylim=c(0,100))
##### there is a big outlier at 200mm in unburnt Rainforest



#Cover
## perform ANOVA
Lcover.aov <- aov(Litter.Cover ~ Treatment, data = veg)
summary(Lcover.aov)

Lcover.aov <- aov(Litter.Cover ~ Formation * Fire, data = veg)
summary(Lcover.aov)

##look at interaction
interaction.plot(veg$Fire, veg$Formation, veg$Litter.Cover)

##test assumptions
par(mfrow = c(1,2))
hist(Lcover.aov$residuals)
plot(Lcover.aov,which=2)
plot(Lcover.aov,which=1)

##Plot
boxplot(
  Litter.Cover ~ Formation*Fire,data = veg,
  names = c("Dry Sclerophyll Burnt", "Rainforest Burnt", "Dry Sclerophyll Unburnt", "Rainforest Unburnt"),
  ylab="Litter Cover (%)",xlab="Treatment",ylim=c(0,100))
