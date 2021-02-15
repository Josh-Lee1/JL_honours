library(gllvm)
library(tidyverse)

df <- read.csv("Data/Processed/ALLdata.csv") %>% 
  select(-c(X)) %>% 
  mutate(Burnt = Fire =="Burnt") %>% 
  mutate(Rainforest = Formation =="Rainforest")

df$Burnt<- as.integer(as.logical(df$Burnt))
df$Rainforest<- as.integer(as.logical(df$Rainforest))

df$location.id <- as.numeric(factor(df$Location, 
                                           levels=unique(df$Location)))


mod<- gllvm(formula = Count ~ Burnt * Rainforest + location.id, data = df, num.lv = 2, 
      family = "negative.binomial")
plot(mod)
mod$params
coefplot(mod, which.Xcoef = "Burnt:Rainforest")

mod.null <-  gllvm ( data = df , formula = Count ~ Burnt + Rainforest + location.id, num.lv = 2, family = "negative.binomial")

anova(mod.null , mod )










#Ant Example ######### from: https://cran.r-project.org/web/packages/gllvm/vignettes/vignette1.html

#df stuff
data(antTraits)
y <- as.matrix(antTraits$abund)
X <- scale(as.matrix(antTraits$env))
TR <- antTraits$traits

gllvm(y, family = "negative.binomial")
gllvm(y, X, family = "negative.binomial")
gllvm(y, X, TR, family = "negative.binomial")

yX <- reshape(data.frame(cbind(y, X)), direction = "long", varying =
                colnames(y), v.names = "y", timevar = "sp")
TR2 <- data.frame(sp = 1:41, TR)
datalong <- merge(yX, TR2, by = "sp")
datalong[1:3, ]

#run mods
gllvm(formula = y ~ 1, data = datalong, family = "negative.binomial")

gllvm(formula = y ~ (Bare.ground + Shrub.cover), data = datalong,  
      family = "negative.binomial")

fitp <- gllvm(y, family = poisson())
fitp

fit_ord <- gllvm(y, family = "negative.binomial")
fit_ord

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fitp, var.colors = 1)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fit_ord, var.colors = 1)

ordiplot(fit_ord, biplot = TRUE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Biplot")
ordiplot(fit_ord, biplot = FALSE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Ordination plot", predict.region = TRUE)

rownames(fit_ord$params$theta) <- paste("spp", 1:ncol(fit_ord$y), sep = "")
ordiplot(fit_ord, biplot = TRUE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-2, 1.6), 
         main = "Biplot", jitter = TRUE, cex.spp = 0.8)

criterias <- NULL
for(i in 1:5){
  fiti <- gllvm(y, X, family = "negative.binomial", num.lv = i, sd.errors = FALSE,
                formula = ~ Bare.ground + Canopy.cover + Volume.lying.CWD, seed = 1234)
  criterias[i + 1] <- summary(fiti)$AICc
  names(criterias)[i + 1] = i
}

criterias

fit_env <- gllvm(y, X, family = "negative.binomial", num.lv = 4,
                 formula = ~ Bare.ground + Canopy.cover +
                   Volume.lying.CWD, seed = 1234)

coefplot(fit_env, cex.ylab = 0.7, mar = c(4, 9, 2, 1), 
         xlim.list = list(NULL, NULL, c(-4, 4)), mfrow=c(1,1))


