# I want to make stacked col graphs for each treatment by average number of points at 50cm breaks

library(dplyr)
library(ggplot2)
library(tidyr)

veg<- read.csv("Data/Processed/vegwp1.csv") %>% 
  filter(Point == 10 | Point == 20 | Point == 30 | Point == 40 | Point == 50)

write.csv(veg, "Data/Processed/vegtest.csv")
#Sum was done in exel
veg <- read.csv("Data/Processed/vegtest1.csv")

#getting means
meanptsQ1 <- aggregate(veg[221], list(veg$Site, veg$Treatment), mean, na.rm = TRUE) 
meanptsQ2 <- aggregate(veg[222], list(veg$Site, veg$Treatment), mean, na.rm = TRUE) 
meanptsQ3 <- aggregate(veg[223], list(veg$Site, veg$Treatment), mean, na.rm = TRUE) 
meanptsQ4 <- aggregate(veg[224], list(veg$Site, veg$Treatment), mean, na.rm = TRUE) 
meanptsall <- aggregate(veg[225], list(veg$Site, veg$Treatment), mean, na.rm = TRUE)
meanpts1m <- aggregate(veg[226], list(veg$Site, veg$Treatment), mean, na.rm = TRUE) 

write.csv(meanpts1m, "Data/Processed/UGpts.csv")


#combine to one df
allmeans <- meanptsQ1 %>% cbind(Q2 = meanptsQ2$Q2, Q3 = meanptsQ3$Q3, Q4 = meanptsQ4$Q4) %>% 
  gather("Strata", "Mean_Points", 3:6) %>% 
  rename(Site = Group.1, Treatment = Group.2)

#plot it!
ggplot(allmeans, aes(x = Treatment, y = Mean_Points, colour = Strata)) + geom_boxplot() +ylab("Average # understorey Points")

allmeanssum <- allmeans %>% 
  group_by(Strata, Treatment) %>% 
  summarise(mean(Mean_Points)) %>% 
  rename(mean_points = "mean(Mean_Points)")

ggplot(allmeanssum, aes(x = Treatment, y = mean_points, fill = forcats::fct_rev(Strata))) + 
  geom_col(position = "stack") + 
  ylab("Average # understorey Points") +
  labs(fill = "Understorey Height (cm)") + 
  scale_fill_discrete(labels = c("151-200", "101-150", "51-100", "1-50")) +
  theme_bw() +
  scale_fill_brewer(palette="YlGr")



