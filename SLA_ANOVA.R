SLA <- read.csv("Data/SLA_Canadian cities.csv")
SLA

library(vegan)

library(car)
library(tidyverse)

glimpse(SLA)

unique(SLA$City)

histogram <- ggplot(SLA, aes(x=value,Fill=City)) + facet_wrap(~City)+ geom_histogram(bins = 50)
histogram

ANOVA <- lm (value~City, data = SLA)
summary(ANOVA)

Anova(ANOVA, type = "2")

summary <- SLA %>% group_by(City) %>% summarise(SLA.avg = mean(value),
                                                SLA.sd = sd(value),
                                                SLA.n = length(value),
                                                SLA.se = sd(value)/sqrt(n()))
summary
