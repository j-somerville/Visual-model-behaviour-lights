###Plaice Tank Trial analysis
##Tank trials took place between 13th May 2021 and 23rd August 2021
##14 plaice were measured, where only 5 moved
#The tank was 2.5m by 170cm tank, with a blue fabric background and sand/sediment bottom
##subjected fish to light (around 50 watts**, a single LED made by SafetyNet Technologies)for 5 minutes, with 5 mins of no light immediately before
#stationary_(light or control)_stop = fish starts to move again
##each behaviour was also characterised as either _on or _off #The tank was 2.5m by 170cm tank, with a blue fabric background and sand/sediment bottom
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

#set wd
library(dplyr)
library(ggplot2)
library(ggpubr)

##out of 14 plaice, only 5 moved (and subsequently only 5 have been analysed, as the other 9 did not move the entire trial)

#setwd
###needs length in
plaice_stationary<-read.csv("plaice_stationary.csv")
###raw stationary data
plaice_stationary2<-plaice_stationary[,-(1)]   

plaice_stationary_totals<-read.csv("stationary_total_control_exp.csv")
##totals for either control or exp side of tank.
##possibility of 4 data points per plaice per colour mode. 
##at least one value for total time spent stationary in on/off light conditions
plaice_stat_entire_trial<-read.csv("plaice_stat_colours2.csv")
##Plaice trials where plaice were stationary for entirety of lights off and on conditions for certain colours
plaice_active_entiretrial<-read.csv("plaice_active_entiretrial.csv")
###plaice trials where plaice plaice were active entire time for a light colour trial

##trials where plaice were stationary entire time
#stat_totes<-plaice_stationary2%>% group_by(trial_number, light_colour, light_type) %>%
#  filter(time[2]>= 590 & time[1]<=16)
#stat_total<- split(stat_totes, paste0(stat_totes$trial_number,sep="_", stat_totes$light_type, sep="_", stat_totes$light_colour))


##get rid of trials that were stationary the whole time (each trial less than 2 and first value in time column less than 16 seconds are deleted)
stat_filter<-plaice_stationary2%>% group_by(trial_number, light_colour, light_type) %>%
  filter(!(n() <= 2 & time[1]<=16))
###list to see diff group numbers
stat_filter2<-stat_filter%>% group_by(trial_number, light_colour, light_type) %>%
  filter(!(n() <= 1))
trials<- split(stat_filter2, paste0(stat_filter2$trial_number,sep="_", stat_filter2$light_type, sep="_", stat_filter2$light_colour))
##30 different trials that had stationary episodes, but weren't stationary for the whole trial

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

####PLOTS AND TABLES######
##stationary totals different for on and off??
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


##Difference between flashing and contin??
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
##opposite effect here?

green_ec<-ggplot(green_time_total, aes(x=light_mode, y=sum.total2., shape=light_type)) + 
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
green_ec
##no difference really

blue_ec<-ggplot(blue_time_total, aes(x=light_mode, y=sum.total2., shape=light_type)) + 
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
blue_ec
##no difference

white_ec<-ggplot(white_time_total, aes(x=light_mode, y=sum.total2., shape=light_type)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing'))+
  labs(x = "White light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
white_ec
##no comparison (no stationary behaviour for lights on flashing)

red_ec<-ggplot(red_time_total, aes(x=light_mode, y=sum.total2., shape=light_type)) + 
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
red_ec
##not much of a difference

rblue_ec<-ggplot(royalblue_time_total, aes(x=light_mode, y=sum.total2., shape=light_type)) + 
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
rblue_ec

###not much of a pattern, large variation


###Experimental or control preferences (towards light or away?)
amber_cf<-ggplot(amber_time_total, aes(x=light_mode, y=sum.total2., shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
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
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
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
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
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
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
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
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
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
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Royal blue light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
rblue_cf
###not clear pattern, more clear differences in royal blue. small sample size

######ACTIVE PLAICE FOR HALF TRIAL######
##active (e.g. no time spent stationary for half trials first)
##subset where rows are n=1.
stat_active_1<-plaice_stationary_totals%>% group_by(trial_number, light_colour, light_type) %>%
  filter((n() <= 1))
##5 trials.

##change corresponding trial to active
stat_active_1$control_light<-"both"
stat_active_1_off<-subset(stat_active_1, stat_active_1$light_mode=="off")
stat_active_1_off$light_mode<-"on"
stat_active_1_off$sum.total2.<-0  
stat_active_1_off$LightFact <- stat_active_1_off$light_colour
stat_active_1_off$TypeFact <- stat_active_1_off$light_type


##now do for lights on (change that they were active in lgihts off)
stat_active_1_on<-subset(stat_active_1, stat_active_1$light_mode=="on")
stat_active_1_on$light_mode<-"off"
stat_active_1_on$sum.total2.<-0  
stat_active_1_on$LightFact <- "off"
stat_active_1_on$TypeFact <- "off"

stat_active_half<-rbind(stat_active_1_on,stat_active_1_off)

stationary_side_new_activehalf<-rbind(plaice_stationary_totals,stat_active_half)
###added 5 on 
stationary_activehalf<- split(stationary_side_new_activehalf, paste0(stationary_side_new_activehalf$trial_number,sep="_", stationary_side_new_activehalf$light_type, sep="_", stationary_side_new_activehalf$light_colour))
##no more rows containing just 1 now


stationary_side_new_activehalf$LightFact <- factor(stationary_side_new_activehalf$LightFact, levels = c("off", "amber", "blue", "green", "red","royalblue","white"))
stationary_side_new_activehalf$light_order<-as.factor(stationary_side_new_activehalf$light_order)
stationary_side_new_activehalf$type_order<-as.factor(stationary_side_new_activehalf$type_order)
stationary_side_new_activehalf$trial_number<-as.factor(stationary_side_new_activehalf$trial_number)
stationary_side_new_activehalf$control_light<- factor(stationary_side_new_activehalf$control_light, levels = c("control", "light", "both"))


##try looking with model

hist(stationary_side_new_activehalf$sum.total2.)
##kind of normally distributed 
hist(log(stationary_side_new_activehalf$sum.total2.))
##left skewed - zero inflated because havent added stationary ones yet.
##300 seconds for being stationary entire trial

require(lmerTest)
total<-lmer(sum.total2.~ 
              +LightFact
            +light_type
            +type_order
            +light_order
            +control_light
            +(1|light_side)
            +(1|trial_number),
            data=stationary_side_new_activehalf)
summary(total)
#Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: sum.total2. ~ +LightFact + light_type + type_order + light_order +  
#  control_light + (1 | light_side) + (1 | trial_number)
#Data: stationary_side_new_activehalf

#REML criterion at convergence: 689

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.4365 -0.7018 -0.1074  0.5476  2.2594 

#Random effects:
#  Groups       Name        Variance Std.Dev.
#trial_number (Intercept)    0      0.00   
#light_side   (Intercept) 1353     36.79   
#Residual                 5721     75.64   
#Number of obs: 73, groups:  trial_number, 5; light_side, 2

#Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)   
#(Intercept)         140.439     36.518    2.422   3.846  0.04492 * 
#  LightFactamber        3.136     41.651   56.003   0.075  0.94024   
#LightFactblue       -60.506     31.415   56.418  -1.926  0.05915 . 
#LightFactgreen      -19.023     32.474   56.384  -0.586  0.56036   
#LightFactred        -86.893     29.079   56.551  -2.988  0.00415 **
#  LightFactroyalblue  105.518     44.589   56.040   2.366  0.02144 * 
#  LightFactwhite      -75.421     57.449   56.346  -1.313  0.19456   
#light_typeflashing   -8.520     18.834   56.117  -0.452  0.65273   
#type_order2          32.965     21.331   56.032   1.545  0.12788   
#light_order2        -58.637     29.363   56.936  -1.997  0.05061 . 
#light_order3       -101.467     36.413   56.987  -2.787  0.00722 **
#  light_order4         31.840     33.677   55.588   0.945  0.34852   
#light_order5        -14.887     39.966   56.572  -0.372  0.71092   
#light_order6         13.470     30.326   56.369   0.444  0.65862   
#control_lightlight    7.639     19.107   56.731   0.400  0.69079   
#control_lightboth   -87.410     38.639   56.434  -2.262  0.02755 * 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation matrix not shown by default, as p = 16 > 12.
#Use print(x, correlation=TRUE)  or
#vcov(x)        if you need it

#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')

###light side variation
pl<-ggplot(stationary_side_new_activehalf, aes(x=light_side, y=sum.total2.,shape=trial_number)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.5)+
  #geom_point(position=position_jitterdodge(dodge.width=0.1), size=1.5)+
  #geom_point(position = position_jitterdodge(jitter.width = 0.1),size =1.75)+
  labs(x = "Trial number", 
       y = "Time spent stationary (s)")+
  theme_bw()+
  ylim(0,300)+
  #  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=11))+
  theme(legend.title=element_blank())+
  #theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())+
  # scale_x_discrete(labels=c('Amber', 'Blue','Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.6, end = .9)
pl
##trials 13 - right side preference

######Second analysis######
###Includes trials where plaice were active or stationary for whole colour trials (not just in lights off or on conditions)
###Trials where plaice did not move for entire 2 hrs are not included.
plaice_stat_entire_trial2list<- split(plaice_stat_entire_trial, paste0(plaice_stat_entire_trial$trial_number,sep="_", plaice_stat_entire_trial$light_type, sep="_", plaice_stat_entire_trial$light_colour))
##28 trials not active for certain colour modes

###active entire trial
trials_active<- split(plaice_active_entiretrial,paste0(plaice_active_entiretrial$trial_number,sep="_", plaice_active_entiretrial$light_type, sep="_", plaice_active_entiretrial$light_colour))
##2 trials from 2 plaice

##check they have one active total for each on and off
plaice_stat_entire_trial<-plaice_stat_entire_trial[,-(1)]   
plaice_zero_in<-rbind(plaice_stat_entire_trial,plaice_active_entiretrial)
plaice_zero_in_list<- split(plaice_zero_in, paste0(plaice_zero_in$trial_number,sep="_", plaice_zero_in$light_type, sep="_", plaice_zero_in$light_colour))
##30 paired trials (28 stationary, 2 active)

##bind with first analysis dataframe
##remove columns 13 and 14
stationary_side_new_activehalf1<-stationary_side_new_activehalf[,-13]
stationary_side_new_activehalf1<-stationary_side_new_activehalf1[,-13]
plaice_zero_infl2<-rbind(plaice_zero_in,stationary_side_new_activehalf1)
plaice_zero_infl2list<- split(plaice_zero_infl2, paste0(plaice_zero_infl2$trial_number,sep="_", plaice_zero_infl2$light_type, sep="_", plaice_zero_infl2$light_colour))
##60 trials
##133 obs

plaice_zero_infl2$LightFact <- factor(plaice_zero_infl2$LightFact, levels = c("off", "amber", "blue", "green", "red","royalblue","white"))
plaice_zero_infl2$TypeFact <- factor(plaice_zero_infl2$TypeFact, levels = c("off", "flashing", "continuous"))
plaice_zero_infl2$light_order<-as.factor(plaice_zero_infl2$light_order)
plaice_zero_infl2$type_order<-as.factor(plaice_zero_infl2$type_order)
plaice_zero_infl2$trial_number<-as.factor(plaice_zero_infl2$trial_number)
plaice_zero_infl2$light_side<-as.factor(plaice_zero_infl2$light_side)
plaice_zero_infl2$control_light <- factor(plaice_zero_infl2$control_light, levels = c("control", "both", "light"))

hist(plaice_zero_infl2$sum.total2.)
##300 inflated not normally distributed 

##GLMM

linear1 <- lmer(sum.total2. ~ 
                  +LightFact
                +light_type
                +control_light
                +(1|light_side)
                +light_order
                +type_order
                +(1|trial_number), data = plaice_zero_infl2) 
summary(linear1)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: sum.total2. ~ +LightFact + light_type + control_light + (1 |  
#    light_side) + light_order + type_order + (1 | trial_number)
#   Data: plaice_zero_infl2

#REML criterion at convergence: 1420.1

#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-1.93551 -0.54314  0.03761  0.63838  2.67379 

#Random effects:
# Groups       Name        Variance Std.Dev.
# trial_number (Intercept) 2585     50.84   
# light_side   (Intercept)    0      0.00   
# Residual                 6925     83.21   
#Number of obs: 133, groups:  trial_number, 5; light_side, 2

#Fixed effects:
#                   Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)         151.734     32.341   13.041   4.692 0.000418 ***
#LightFactamber       21.227     27.968  113.052   0.759 0.449454    
#LightFactblue       -41.033     27.766  113.040  -1.478 0.142245    
#LightFactgreen      -15.960     27.591  113.074  -0.578 0.564110    
#LightFactred        -65.098     26.078  113.051  -2.496 0.013991 *  
#LightFactroyalblue   22.403     29.066  113.037   0.771 0.442452    
#LightFactwhite       13.548     30.421  113.127   0.445 0.656905    
#light_typeflashing   -9.104     15.084  113.089  -0.604 0.547366    
#control_lightboth  -136.323     33.403  114.634  -4.081 8.32e-05 ***
#control_lightlight   -5.253     15.448  113.438  -0.340 0.734467    
#light_order2        -43.408     26.387  113.013  -1.645 0.102744    
#light_order3         44.959     26.517  113.032   1.696 0.092729 .  
#light_order4         70.045     25.746  113.206   2.721 0.007545 ** 
#light_order5         82.592     26.376  113.304   3.131 0.002214 ** 
#light_order6         48.112     25.715  113.405   1.871 0.063926 .  
#type_order2          58.693     15.594  113.120   3.764 0.000267 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation matrix not shown by default, as p = 16 > 12.
#Use print(x, correlation=TRUE)  or
#    vcov(x)        if you need it

#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')

###individual variation here in this model
plaice_zero_infl2$trial_number  = factor(plaice_zero_infl2$trial_number, levels=c("3","5","8","10","13"))

###plot variation here
pl<-ggplot(plaice_zero_infl2, aes(x=trial_number, y=sum.total2.,shape=light_mode)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.5)+
  #geom_point(position=position_jitterdodge(dodge.width=0.1), size=1.5)+
  geom_point(position = position_jitterdodge(jitter.width = 0.1),size =1.75)+
  labs(x = "Trial number", 
       y = "Time spent stationary (s)")+
  theme_bw()+
  ylim(0,300)+
  #  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=11))+
  theme(legend.title=element_blank())+
  #theme(axis.title.x=element_blank())+
  #theme(axis.title.y=element_blank())+
  # scale_x_discrete(labels=c('Amber', 'Blue','Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.6, end = .9)
pl

## between colour modes and trial 8 in particular