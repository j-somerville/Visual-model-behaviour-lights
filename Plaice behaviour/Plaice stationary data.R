##Script for cleaning data from video clips of tank trials at MBA
##tank trials took place between 13th May 2021 and 23rd August 2021
##Consisted of testing 14 plaice
#The tank was 2.5m by 170cm tank, with a blue fabric background and sand/sediment bottom
##subjected fish to light (around 50 watts**, a single LED made by SafetyNet Technologies)for 5 minutes, with 5 mins of no light immediately before
##6 light colours were used - blue, red, green, white, amber, royal blue 
##repeated for continuous and flashing light (8hz)
##one light was at either end of the tank - and I alternated between which side was switched on to rule out a tank-side bias 
##therefore, the light that was not  being switched on at one end of the tank was the control light
##videos analysed to see how fish responded to light, and whether certain colours influenced type of response)
#Behaviour types: 
#Behaviour types: 
#stationary_(light or control) = fish stationary for at least 15 seconds
#stationary_(light or control)_stop = fish starts to move again
##each behaviour was also characterised as either _on or _off 

##out of 14 plaice, only 5 moved (and subsequently only 5 have been analysed, as the other 9 did not move the entire trial)
library(dplyr)
library(ggplot2)
library(ggpubr)
#setwd
plaice_stationary<-read.csv("plaice_stationary.csv")
plaice_stationary2<-plaice_stationary[,-(1)]   

##trials where plaice were stationary entire time
stat_totes<-plaice_stationary2%>% group_by(trial_number, light_colour, light_type) %>%
  filter(time[2]>= 590 & time[1]<=16)
stat_total<- split(stat_totes, paste0(stat_totes$trial_number,sep="_", stat_totes$light_type, sep="_", stat_totes$light_colour))
#10 trials discouted-stationary whole trial
##plus the 9 trials that I didnt include in data sheet (where plaice didnt move)
#9*6
#54*2
#108 trials altogether, plus 10 trials
108+10
##118
##get rid of trials that were stationary the whole time (each trial less than 2 and first value in time column less than 16 seconds are deleted)
stat_filter<-plaice_stationary2%>% group_by(trial_number, light_colour, light_type) %>%
  filter(!(n() <= 2 & time[1]<=16))
###list to see diff group numbers
stat_filter2<-stat_filter%>% group_by(trial_number, light_colour, light_type) %>%
  filter(!(n() <= 1))
trials<- split(stat_filter2, paste0(stat_filter2$trial_number,sep="_", stat_filter2$light_type, sep="_", stat_filter2$light_colour))
##30 different trials that had stationary episodes, but weren't stationary for the whole trial

###Stationary totals for control and exp sides########
plaice_stationary_totals<-read.csv("stationary_total_control_exp.csv")

###Questions-
##Does light affect behaviour? - compare behaviour between 'on' and 'off' codes. 
##Does light colour affect behaviour? - compare behaviour across colours- for 'on' behaviour codes - more time stationary?
##Is there a difference in  behaviour between flashing and continuous light modes? -compare behaviour in across 2 treatments. 
##Do fish become habituated to light- 1) after initial exposure (during individual trial)? - 
#2) after whole trial period? (after exposure to all 6 colour modes)- trial number - does behaviour change depending on trial order


####data exploration#####
hist(plaice_stationary_totals$sum.total2.)
hist(log(plaice_stationary_totals$sum.total2.))
##better


##stationary totals different for on and off??###
green_time_total<-subset(plaice_stationary_totals, plaice_stationary_totals$light_colour=="green")
green_table<-green_time_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars("sum.total2."), funs(mean, sd))

green_time_plot<-ggplot(green_time_total, aes(x=light_mode, y=sum.total2.)) + 
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

blue_time_total<-subset(plaice_stationary_totals, plaice_stationary_totals$light_colour=="blue")

blue_table<-blue_time_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars("sum.total2."), funs(mean, sd))

blue_time_plot<-ggplot(blue_time_total, aes(x=light_mode, y=sum.total2.)) + 
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

royalblue_time_total<-subset(plaice_stationary_totals, plaice_stationary_totals$light_colour=="royalblue")

royalblue_table<-royalblue_time_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars("sum.total2."), funs(mean, sd))

royalblue_time_plot<-ggplot(royalblue_time_total, aes(x=light_mode, y=sum.total2.)) + 
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

red_time_total<-subset(plaice_stationary_totals, plaice_stationary_totals$light_colour=="red")

red_table<-red_time_total %>% 
  group_by(light_mode, light_type) %>%
  summarize_at(vars("sum.total2."), funs(mean, sd))

red_time_plot<-ggplot(red_time_total, aes(x=light_mode, y=sum.total2.)) + 
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

amber_time_total<-subset(plaice_stationary_totals, plaice_stationary_totals$light_colour=="amber")

amber_table<-amber_time_total %>% 
  group_by(light_mode, light_type) %>%
  summarize_at(vars("sum.total2."), funs(mean, sd))

amber_time_plot<-ggplot(amber_time_total, aes(x=light_mode, y=sum.total2.)) + 
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

white_time_total<-subset(plaice_stationary_totals, plaice_stationary_totals$light_colour=="white")

white_table<-white_time_total %>% 
  group_by(light_mode, light_type) %>%
  summarize_at(vars("sum.total2."), funs(mean, sd))

white_time_plot<-ggplot(white_time_total, aes(x=light_mode, y=sum.total2.)) + 
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

###opposites in royal blue and red, others not a clear pattern

##experimental or control??
amber_ec<-ggplot(amber_time_total, aes(x=light_mode, y=sum.total2., shape=light_type)) + 
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

green_ec<-ggplot(green_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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

blue_ec<-ggplot(blue_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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

white_ec<-ggplot(white_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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

red_ec<-ggplot(red_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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

rblue_ec<-ggplot(royalblue_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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
amber_cf<-ggplot(amber_time_total, aes(x=light_mode, y=sum.total2., shape=light_type)) + 
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

green_cf<-ggplot(green_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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

blue_cf<-ggplot(blue_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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

white_cf<-ggplot(white_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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

red_cf<-ggplot(red_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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

rblue_cf<-ggplot(royalblue_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
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
###not clear pattern

############stats analysis#########
plaice_stationary_totals$LightFact <- factor(plaice_stationary_totals$LightFact, levels = c("off", "amber", "blue", "green", "red","royalblue","white"))
plaice_stationary_totals$TypeFact <- factor(plaice_stationary_totals$TypeFact, levels = c("off", "flashing", "continuous"))
plaice_stationary_totals$light_order<-as.factor(plaice_stationary_totals$light_order)
plaice_stationary_totals$type_order<-as.factor(plaice_stationary_totals$type_order)
plaice_stationary_totals$trial_number<-as.factor(plaice_stationary_totals$trial_number)
plaice_stationary_totals$light_side<-as.factor(plaice_stationary_totals$light_side)

require(lmerTest)
linear1 <- lmer(sum.total2. ~ 
                  +LightFact
                +TypeFact
                +control_light
                +light_side
                +light_order
                +type_order
                +(1|trial_number), data = plaice_stationary_totals) 
summary(linear1)

###royal blue, green, blue significant. more stationary after 5th

ggarrange(royalblue_time_plot, blue_time_plot, green_time_plot, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

