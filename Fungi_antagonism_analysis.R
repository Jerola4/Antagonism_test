###One way-ANOVA###
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(qqplotr)
library(nlme)
library(rcompanion)
library(lsmeans)
library(multcompView)
library(multcomp)
library(emmeans)
library(here)

rm(list = ls())

#Data fungi
Fun_ant=read_xlsx("./Raw_data/Fungi_Antagonimstest_2.xlsx")
Fun_ant$Bacteria=as.factor(Fun_ant$Bacteria)
Fun_ant$Fungi=as.factor(Fun_ant$Fungi)
str(Fun_ant)

boxplot(`Length (mm) from Fungi` ~ Bacteria, Fun_ant) + mtext(paste("outliers: ", paste(out_ind, collapse = ",")))

ggplot(Fun_ant, aes(y = `Length (mm) from Fungi`, x= Bacteria)) + geom_boxplot(aes(fill=Fungi)) 

Fun <- Fun_ant %>% 
  group_by(Bacteria) %>% 
  summarize(length=mean(`Length (mm) from Fungi`))

#summary
summary(Fun_ant$`Length (mm) from Fungi`)
summary(Fun_ant)

t.test(Fun_ant$`Length (mm) from Fungi`, mu=29.68, conf.level = 0.95) #t.test alternative hypothesis is equal to mean
#Plot of means and confidence intervals
Sumary_Fung = groupwiseMean(`Length (mm) from Fungi` ~ Bacteria,
                            data   = Fun_ant,
                            conf   = 0.95,
                            digits = 3,
                            traditional = FALSE,
                            percentile  = TRUE)
ggplot(Sumary_Fun,                
       aes(x = Bacteria,
           y = Mean)) +
  geom_errorbar(aes(ymin = Percentile.lower,
                    ymax = Percentile.upper),
                width = 0.05,
                size  = 0.5) +
  geom_point(shape = 15,
             size  = 4) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  ylab("Mean radius, mm") 

#linear model
model = lm(`Length (mm) from Fungi` ~ Bacteria,
           data = Fun_ant)
summary(model)
#Post-hoc analysis:  mean separation tests
marginal = lsmeans(model,
                   ~ Bacteria)
pairs(marginal,
      adjust="tukey")

CLDFun = cld(marginal,
             alpha   = 0.05,
             Letters = letters,    ### Use lower-case letters for .group
             adjust  = "Sidak")    ### Tukey-adjusted comparisons


#Plot of means, confidence intervals, and mean-separation letters
CLDFun$Bacteria = factor(CLDFun$Bacteria,
                         levels=c("Gmpep3_N_Stop",
                                  "Gmpep3_Stop",
                                  "PBES"))

CLDFun$.group=gsub(" ", "", CLDFun$.group)
ggplot(CLDFun,
       aes(x     = Bacteria,
           y     = lsmean,
           label = .group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_errorbar(aes(ymin  =  lower.CL,
                    ymax  =  upper.CL),
                width =  0.2,
                size  =  0.7) +
  geom_text(nudge_x = c(0, 0, 0),
            nudge_y = c(10, 10, 10),
            color   = "black") +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))































