library(car)
library(tidyverse)
library(lme4)
library(sjPlot)
library(sjmisc)

#Prepare data
df <- read.csv("Data/Processed/ALLdata.csv") %>% 
  select(-c(X))

X <-  df %>% 
  dplyr::select (Site, Location, Fire, Formation, Treatment, Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct()

#Litter Depth
LD.lmer<- lmer(Litter.Depth ~ Formation * Fire +(1|Location), data = X)
LD.lmer2<- lmer(Litter.Depth ~ Formation + (1|Location), data = X)

summary(LD.lmer)
plot(LD.lmer)
anova(LD.lmer2,LD.lmer)
plot_model(LD.lmer, type = "int")

##Variance inflation
LD.lmer<- lmer(Litter.Depth ~ Formation * Fire +(1|Location), data = X)
vif(LD_LC.lmer)

#Litter Cover
LC.lmer<- lmer(Litter.Cover ~ Formation * Fire +(1|Location), data = X)
LC.lmer2<- lmer(Litter.Cover ~ Formation + (1|Location), data = X)

summary(LC.lmer)
plot(LC.lmer)
anova(LC.lmer2,LC.lmer)
plot_model(LC.lmer, type = "int")

##Variance inflation
vif(LC.lmer)

#Understorey
U.lmer<- lmer(Understory ~ Formation * Fire +(1|Location), data = X)
U.lmer2<- lmer(Understory ~ Formation + (1|Location), data = X)

summary(U.lmer)
plot(U.lmer)
anova(U.lmer2,U.lmer)
plot_model(U.lmer, type = "int")

##Variance inflation
vif(U.lmer)

#Midstorey
M.lmer<- lmer(Mid.height ~ Formation * Fire +(1|Location), data = X)
M.lmer2<- lmer(Mid.height ~ Formation + (1|Location), data = X)

summary(M.lmer)
plot(M.lmer)
anova(M.lmer2,M.lmer)
plot_model(M.lmer, type = "int")

##Variance inflation
vif(M.lmer)

#Canopy Cover
CC.lmer<- lmer(Canopy.Cover ~ Formation * Fire +(1|Location), data = X)
CC.lmer2<- lmer(Canopy.Cover ~ Formation + (1|Location), data = X)

summary(CC.lmer)
plot(CC.lmer)
anova(CC.lmer2, CC.lmer)
plot_model(CC.lmer, type = "int")

##Variance inflation
vif(CC.lmer)
