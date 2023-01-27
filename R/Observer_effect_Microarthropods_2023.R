
library(tidyverse)
library(dplyr)
library(readxl)
library(vegan)
library(performance)

#Data
Microarth<-read_csv2(file = "data/FUNDER_2023_Microarthropods_composition.csv")

ggplot(Microarth, aes(x=Observer, y=Total_microarthropods)) +
           geom_boxplot()

ggplot(Microarth, aes(x=Observer, y=Total_collembola)) +
  geom_boxplot()

ggplot(Microarth, aes(x=Observer, y=Total_mites)) +
  geom_boxplot()

#Observer effect on Total microarthropods


anova_obs=aov(Microarth$Total_microarthropods~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)

#Log tranform
Microarth<-Microarth %>% mutate (log_microarth = log(Total_microarthropods))
Microarth<-Microarth %>% filter(log_microarth != "Na")
Microarth<-Microarth %>% filter(log_microarth != "-Inf")

anova_obs=aov(Microarth$log_microarth~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)


#posthoc test Kruskal.test
kruskal.test(Microarth$Total_microarthropods~Microarth$Observer)
kruskal.test(Microarth$log_microarth~Microarth$Observer)
#No significant

#Observer effect on Total mites

anova_obs=aov(Microarth$Total_mites~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal

#Log tranform
Microarth<-Microarth %>% mutate (log_mites= log(Total_mites))

anova_obs=aov(log_mites~Observer+Site,data=Microarth)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal

anova_obs=aov(log_mites~Observer*Treatment,data=Microarth)
summary(anova_obs)

#posthoc test Kruskal.test
kruskal.test(Microarth$Total_mites~Microarth$Observer)
kruskal.test(Microarth$log_mites~Microarth$Observer)
#No significant

#Diversity - Number of functional groups


#Observer effect on Mite fungivorous

ggplot(Microarth, aes(x=Observer, y=Mite_fungivorous)) +
  geom_boxplot()

anova_obs=aov(Microarth$Mite_fungivorous~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)

#Log tranform
Microarth<-Microarth %>% mutate (log_microarth = log(Mite_fungivorous))
Microarth<-Microarth %>% filter(log_microarth != "Na")
Microarth<-Microarth %>% filter(log_microarth != "-Inf")

anova_obs=aov(Microarth$log_microarth~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)


#posthoc test Kruskal.test
kruskal.test(Microarth$Mite_fungivorous~Microarth$Observer)
kruskal.test(Microarth$log_microarth~Microarth$Observer)
#No significant

#Observer effect on Mite nematophagous

ggplot(Microarth, aes(x=Observer, y=Mite_nematophagous)) +
  geom_boxplot()

anova_obs=aov(Microarth$Mite_nematophagous~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)

#Log tranform
Microarth<-Microarth %>% mutate (log_microarth = log(Mite_nematophagous))
Microarth<-Microarth %>% filter(log_microarth != "Na")
Microarth<-Microarth %>% filter(log_microarth != "-Inf")

anova_obs=aov(Microarth$log_microarth~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)


#posthoc test Kruskal.test
kruskal.test(Microarth$Mite_nematophagous~Microarth$Observer)
kruskal.test(Microarth$log_microarth~Microarth$Observer)
#No significant

#Observer effect on Mite predaceous

ggplot(Microarth, aes(x=Observer, y=Mite_predaceous)) +
  geom_boxplot()

anova_obs=aov(Microarth$Mite_predaceous~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)

#Log tranform
Microarth<-Microarth %>% mutate (log_microarth = log(Mite_predaceous))
Microarth<-Microarth %>% filter(log_microarth != "Na")
Microarth<-Microarth %>% filter(log_microarth != "-Inf")

anova_obs=aov(Microarth$log_microarth~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)


#posthoc test Kruskal.test
kruskal.test(Microarth$Mite_predaceous~Microarth$Observer)
kruskal.test(Microarth$log_microarth~Microarth$Observer)
#No significant

#Observer effect on Collembola fungivorous

ggplot(Microarth, aes(x=Observer, y=Collembola_fungivorous)) +
  geom_boxplot()

anova_obs=aov(Microarth$Collembola_fungivorous~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)

#Log tranform
Microarth<-Microarth %>% mutate (log_microarth = log(Collembola_fungivorous))
Microarth<-Microarth %>% filter(log_microarth != "Na")
Microarth<-Microarth %>% filter(log_microarth != "-Inf")

anova_obs=aov(Microarth$log_microarth~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)


#posthoc test Kruskal.test
kruskal.test(Microarth$Collembola_fungivorous~Microarth$Observer)
kruskal.test(Microarth$log_microarth~Microarth$Observer)
#No significant

#Observer effect on Collembola predaceous

ggplot(Microarth, aes(x=Observer, y=Collembola_predaceous)) +
  geom_boxplot()

anova_obs=aov(Microarth$Collembola_predaceous~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are not normal
check_model(anova_obs)

#posthoc test Kruskal.test
kruskal.test(Microarth$Collembola_predaceous~Microarth$Observer)
#No significant

#Observer effect on Total collembola

ggplot(Microarth, aes(x=Observer, y=Total_collembola)) +
  geom_boxplot()

anova_obs=aov(Microarth$Total_collembola~Microarth$Observer)
summary(anova_obs)
e_f=resid(anova_obs)
hist(e_f)
qqnorm(e_f)
qqline(e_f)
shapiro.test(e_f)
#residu are normal
check_model(anova_obs)
summary(anova_obs)
#Significant effect of the observer on Total Collembola
