library(car)
library(tidyverse)
library(ggplot2)
library(lme4)
library(sjPlot)
library(sjmisc)

#Prepare data
df <- read.csv("Data/Processed/ALLdata.csv") %>% 
  select(-c(X))

X <-  df %>% 
  dplyr::select (Site,
                 Location,
                 Fire,
                 Formation,
                 Treatment,
                 Litter.Depth,
                 Litter.Cover,
                 Understory,
                 Mid.height,
                 Canopy.Cover,
                 Estimate_density,
                 Simpson_diversity) %>% 
  dplyr::distinct()

#Litter Depth
LD.lmer<- lmer(Litter.Depth ~ Formation * Fire +(1|Location), data = X)
LD.lmer2<- lmer(Litter.Depth ~ Formation + (1|Location), data = X)

summary(LD.lmer)
plot(LD.lmer)
anova(LD.lmer2,LD.lmer)
ld<- plot_model(LD.lmer, type = "int")
ggsave(filename = "Outputs/lme/veg/Litter_Depth.png", plot = ld)

##Variance inflation
LD.lmer<- lmer(Litter.Depth ~ Formation * Fire +(1|Location), data = X)
vif(LD_LC.lmer)

#Litter Cover
LC.lmer<- lmer(Litter.Cover ~ Formation * Fire +(1|Location), data = X)
LC.lmer2<- lmer(Litter.Cover ~ Formation + (1|Location), data = X)

summary(LC.lmer)
plot(LC.lmer)
anova(LC.lmer2,LC.lmer)
ggsave(filename = "Outputs/lme/veg/Litter_Cover.png", plot =  
         plot_model(LC.lmer, type = "int"))


##Variance inflation
vif(LC.lmer)

#Understorey
U.lmer<- lmer(Understory ~ Formation * Fire +(1|Location), data = X)
U.lmer2<- lmer(Understory ~ Formation + (1|Location), data = X)

summary(U.lmer)
plot(U.lmer)
anova(U.lmer2,U.lmer)
ggsave(filename = "Outputs/lme/veg/Understorey.png", plot = 
         plot_model(U.lmer, type = "int"))

##Variance inflation
vif(U.lmer)

#Midstorey
M.lmer<- lmer(Mid.height ~ Formation * Fire +(1|Location), data = X)
M.lmer2<- lmer(Mid.height ~ Formation + (1|Location), data = X)

summary(M.lmer)
plot(M.lmer)
anova(M.lmer2,M.lmer)
ggsave(filename = "Outputs/lme/veg/Midstory.png", plot = 
         plot_model(M.lmer, type = "int"))

##Variance inflation
vif(M.lmer)

#Canopy Cover
CC.lmer<- lmer(Canopy.Cover ~ Formation * Fire +(1|Location), data = X)
CC.lmer2<- lmer(Canopy.Cover ~ Formation + (1|Location), data = X)

summary(CC.lmer)
plot(CC.lmer)
anova(CC.lmer2, CC.lmer)
ggsave(filename = "Outputs/lme/veg/Canopy_Cover.png", plot = 
         plot_model(CC.lmer, type = "int"))

##Variance inflation
vif(CC.lmer)

#All
div_veg<- lmer(Simpson_diversity ~ Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover +(1|Location), data = X)
vif(div_veg)
dens_veg<- lmer(Estimate_density ~ Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover +(1|Location), data = X)
vif(dens_veg)
pairs(X[6:10])
