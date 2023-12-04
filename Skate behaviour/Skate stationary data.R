##Script for cleaning data from video clips of tank trials at MBA
##tank trials took place between 13th May 2021 and 23rd August 2021
##Consisted of testing 27 skate (13 blonde rays, 12 spotted and 2 small-eyed)
#The tank was 2.5m by 170cm tank, with a blue fabric background and sand/sediment bottom
##subjected fish to light (around 50 watts**, a single LED made by SafetyNet Technologies)for 5 minutes, with 5 mins of no light immediately before
##6 light colours were used - blue, red, green, white, amber, royal blue 
##repeated for continuous and flashing light (4hz)
##one light was at either end of the tank - and I alternated between which side was switched on to rule out a tank-side bias 
##therefore, the light that was not  being switched on at one end of the tank was the control light
##videos analysed to see how fish responded to light, and whether certain colours influenced type of response)
#Behaviour types: 
#stationary_(light or control) = fish stationary for at least 15 seconds
#stationary_(light or control)_stop = fish starts to move again
##each behaviour was also characterised as either _on or _off 

library(dplyr)
library(ggplot2)
library(ggpubr)
#setwd
skate_stationary<-read.csv("skate_stationary.csv")
skate_stationary2<-skate_stationary[,-(1)]   
##total number of skates that were stationary, so we aren't just including "end" code
stat_totes<-skate_stationary2%>% group_by(trial_number, light_colour, light_type) %>%
  filter((n() <= 2 & time[1]<=16))
stat_total<- split(stat_totes, paste0(stat_totes$trial_number,sep="_", stat_totes$light_type, sep="_", stat_totes$light_colour))
#176 trials discounted-stationary whole trial

##for each trial, remove row if behaviour contains only stationary_control or stationary_light
##this is because the skate stayed stationary for the entire trial, which I am therefore removing
##to do this, filter out groups if rows are 2 in length(which means stationary +end)
##AND less than 16 seconds, which will delete all skate that have been stationary for entire trial
stat_filter<-skate_stationary2%>% group_by(trial_number, light_colour, light_type) %>%
  filter(!(n() <= 2 & time[1]<=16))
##remove trials with just end
stat_filter2<-stat_filter%>% group_by(trial_number, light_colour, light_type) %>%
  filter(!(n() <= 1))
###list to check number of different trials that we will analyse
trials<- split(stat_filter2, paste0(stat_filter2$trial_number,sep="_", stat_filter2$light_type, sep="_", stat_filter2$light_colour))
##110 diferent trials (not including skate that were stationary entire trial)
###TOTAL skate trials were 324 (27 skate, 2x light types, x6 colour modes)

###Stationary totals for control and exp sides########
skate_stationary_totals<-read.csv("stationary_total_control_exp.csv")

###Questions-
##Does light affect behaviour? - compare behaviour between 'on' and 'off' codes. 
##Does light colour affect behaviour? - compare behaviour across colours- for 'on' behaviour codes - more time stationary?
##Is there a difference in  behaviour between flashing and continuous light modes? -compare behaviour in across 2 treatments. 
##Do fish become habituated to light- 1) after initial exposure (during individual trial)? - 
#2) after whole trial period? (after exposure to all 6 colour modes)- trial number - does behaviour change depending on trial order


####data exploration#####
hist(skate_stationary_totals$total_new)
##kind of normally distributed 
hist(log(skate_stationary_totals$total_new))
##left skewed, looked better before logging

##stationary totals different for on and off??###
green_time_total<-subset(skate_stationary_totals, skate_stationary_totals$light_colour=="green")
green_table<-green_time_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars("total_new"), funs(mean, sd))

green_time_plot<-ggplot(green_time_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Green light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
green_time_plot

blue_time_total<-subset(skate_stationary_totals, skate_stationary_totals$light_colour=="blue")

blue_table<-blue_time_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars("total_new"), funs(mean, sd))

blue_time_plot<-ggplot(blue_time_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Blue light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
blue_time_plot

royalblue_time_total<-subset(skate_stationary_totals, skate_stationary_totals$light_colour=="royalblue")

royalblue_table<-royalblue_time_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars("total_new"), funs(mean, sd))

royalblue_time_plot<-ggplot(royalblue_time_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Royal blue light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
royalblue_time_plot

red_time_total<-subset(skate_stationary_totals, skate_stationary_totals$light_colour=="red")

red_table<-red_time_total %>% 
  group_by(light_mode, light_type) %>%
  summarize_at(vars("total_new"), funs(mean, sd))

red_time_plot<-ggplot(red_time_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Red light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
red_time_plot

amber_time_total<-subset(skate_stationary_totals, skate_stationary_totals$light_colour=="amber")

amber_table<-amber_time_total %>% 
  group_by(light_mode, light_type) %>%
  summarize_at(vars("total_new"), funs(mean, sd))

amber_time_plot<-ggplot(amber_time_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Amber light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
amber_time_plot

white_time_total<-subset(skate_stationary_totals, skate_stationary_totals$light_colour=="white")

white_table<-white_time_total %>% 
  group_by(light_mode, light_type) %>%
  summarize_at(vars("total_new"), funs(mean, sd))

white_time_plot<-ggplot(white_time_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "White light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
white_time_plot

###looks like more stationary with lights on. amber and red not as clear. high variance white

##experimental or control??
amber_ec<-ggplot(amber_time_total, aes(x=light_mode, y=total_new, shape=light_type)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing'))+
  labs(x = "Amber light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
amber_ec

green_ec<-ggplot(green_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Green light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
green_ec

blue_ec<-ggplot(blue_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Blue light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
blue_ec

white_ec<-ggplot(white_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Whitelight", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
white_ec

red_ec<-ggplot(red_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Red light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
red_ec

rblue_ec<-ggplot(royalblue_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Royal blue light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
rblue_ec

###not much of a pattern, large variation

##flashing or continuous differences?
amber_cf<-ggplot(amber_time_total, aes(x=light_mode, y=total_new, shape=light_type)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing'))+
  labs(x = "Amber light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
amber_cf

green_cf<-ggplot(green_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing'))+
  labs(x = "Green light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
green_cf

blue_cf<-ggplot(blue_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing'))+
  labs(x = "Blue light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
blue_cf

white_cf<-ggplot(white_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing'))+
  labs(x = "Whitelight", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
white_cf

red_cf<-ggplot(red_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing'))+
  labs(x = "Red light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
red_cf

rblue_cf<-ggplot(royalblue_time_total, aes(x=light_mode, y=total_new, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing'))+
  labs(x = "Royal blue light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
rblue_cf

###not much of a pattern

############stats analysis#########
skate_stationary_totals$LightFact <- factor(skate_stationary_totals$LightFact, levels = c("off", "amber", "blue", "green", "red","royalblue","white"))
skate_stationary_totals$TypeFact <- factor(skate_stationary_totals$TypeFact, levels = c("off", "flashing", "continuous"))
skate_stationary_totals$light_order<-as.factor(skate_stationary_totals$light_order)
skate_stationary_totals$type_order<-as.factor(skate_stationary_totals$type_order)
skate_stationary_totals$trial_number<-as.factor(skate_stationary_totals$trial_number)
skate_stationary_totals$light_side<-as.factor(skate_stationary_totals$light_side)
skate_stationary_totals$species<-as.factor(skate_stationary_totals$species)

require(lmerTest)

linear1 <-lmer(total_new ~ 
                +LightFact
                +TypeFact
                +species
                +mls
                +control_light
                +light_side
                +light_order
                +type_order
                +(1|trial_number), data = skate_stationary_totals) 
summary(linear1)

###royal blue, green, blue significant. more stationary after 5th
##same result for wing length as well.
ggarrange(royalblue_time_plot, blue_time_plot, green_time_plot, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
