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

#data Bacteria
ant=read_xlsx("./Raw_data//antagonims_Gmpep3.xlsx")
ant$Treatment=as.factor(ant$Treatment)
ant$Bacteria=as.factor(ant$Bacteria)
str(ant)

# Data visualization
(data_ant <- ant %>% 
  group_by(Bacteria) %>% 
  summarize(y= mean(`Ratio (mm)`),
            sd_bact = sd(`Ratio (mm)`),
            se_bact =  sd_bact/sqrt(length(`Ratio (mm)`)))
  )
  
boxplot(`Ratio (mm)` ~ Bacteria, ant) + mtext(paste("outliers: ", paste(out_ind, collapse = ",")))
ggplot(ant, aes(y = `Ratio (mm)`, x= Treatment)) + geom_boxplot(aes(fill=Bacteria)) 

#identify outliers in boxplot
outliers=boxplot(ant$`Ratio (mm)`, plot = FALSE)$out
out_ind <- which(ant$`Ratio (mm)` %in% c(outliers))
out_ind

#summary
summary(ant$`Ratio (mm)`)
summary(ant)

#one sample test 
t.test(ant$`Ratio (mm)`, mu=83.81, conf.level = 0.95) #t.test alternative hypothesis is not equal to mean

#Plot of means and confidence intervals
Sumary_Bact = groupwiseMean(`Ratio (mm)` ~ Bacteria,
                    data   = ant,
                    conf   = 0.95,
                    digits = 3,
                    traditional = FALSE,
                    percentile  = TRUE)

ggplot(Sumary_Bact,                ### The data frame to use.
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
model = lm(`Ratio (mm)` ~ Bacteria,
           data = ant)
summary(model)
#Post-hoc analysis:  mean separation tests
marginal = lsmeans(model,
                   ~ Bacteria)
pairs(marginal,
      adjust="tukey")

CLDBac = cld(marginal,
          alpha   = 0.05,
          Letters = letters,    ### Use lower-case letters for .group
          adjust  = "Sidak")    ### Tukey-adjusted comparisons


#Plot of means, confidence intervals, and mean-separation letters
CLDBac$Treatment = factor(CLDBac$Bacteria,
                        levels=c("",
                                 "PBES"))

CLDBac$.group=gsub(" ", "", CLDBac$.group)
(CLD_Bact_plot <- ggplot(CLDBac,
       aes(x     = Bacteria,
           y     = lsmean,
           label = .group)) +
  geom_point(shape  = 15,
             size   = 4) +
  geom_text(nudge_x = c(0, 0, 0),
            nudge_y = c(25, 25, 20),
            color   = "black") +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text.y =  element_text(face = "bold"),
        axis.text.x  = element_text(face = "bold", colour = c("Red", "Red", "Green", "Green", "Green")),
        plot.caption = element_text(hjust = 0)))

CLD_Bact_plot + 
  geom_errorbar(aes(ymin  =  lower.CL,
                    ymax  =  upper.CL),
                width =  0.2,
                size  =  0.7)
