###Skate Tank Trial analysis
##Tank trials took place between 13th May 2021 and 23rd August 2021
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
###raw stationary data
skate_stationary2<-skate_stationary[,-(1)]   

skate_stationary_totals<-read.csv("stationary_total_control_exp.csv")
##totals for either control or exp side of tank.
##possibility of 4 data points per skate per colour mode. 
##at least one value for total time spent stationary in on/off light conditions
skate_entire_trial<-read.csv("stationary_entire_trial.csv")
##Skate trials where skates were stationary for entirety of lights off and on conditions
all_trials_skate<- read.csv("all_skate_trials.csv")
##all skate trials including ones that didnt move (inflated towards 300s)

###First analysis####
###Make sure to include skates that were active for lights off or lights on conditions
##subset where rows are n=1.
stat_active_1<-skate_stationary_totals%>% group_by(trial_number, light_colour, light_type) %>%
  filter((n() <= 1))
##20 trials.
##change corresponding trial to active, so we now have a paired trial for lights off and on conditions
stat_active_1$control_light<-"both"
stat_active_1_off<-subset(stat_active_1, stat_active_1$light_mode=="off")
stat_active_1_off$light_mode<-"on"
stat_active_1_off$total_new<-0  
stat_active_1_off$LightFact <- stat_active_1_off$light_colour
stat_active_1_off$TypeFact <- stat_active_1_off$light_type

##now do for lights on (change that they were active in lgihts off)
stat_active_1_on<-subset(stat_active_1, stat_active_1$light_mode=="on")
stat_active_1_on$light_mode<-"off"
stat_active_1_on$total_new<-0  
stat_active_1_on$LightFact <- "off"
stat_active_1_on$TypeFact <- "off"

stat_active_half<-rbind(stat_active_1_on,stat_active_1_off)

active_half<- split(stat_active_half, paste0(stat_active_half$trial_number,sep="_", stat_active_half$light_type, sep="_", stat_active_half$light_colour))
##20 paired trials now.

stationary_side_new_activehalf<-rbind(skate_stationary_totals,stat_active_half)
###added this data to stationary totals.

stationary_activehalf<- split(stationary_side_new_activehalf, paste0(stationary_side_new_activehalf$trial_number,sep="_", stationary_side_new_activehalf$light_type, sep="_", stationary_side_new_activehalf$light_colour))
##no more rows containing just 1 now, so all trials paired.

##sort data for model and plotting
stationary_side_new_activehalf$total_new<-as.numeric(stationary_side_new_activehalf$total_new)
stationary_side_new_activehalf$light_type<-as.factor(stationary_side_new_activehalf$light_type)
stationary_side_new_activehalf$species<-as.factor(stationary_side_new_activehalf$species)
stationary_side_new_activehalf$LightFact <- factor(stationary_side_new_activehalf$LightFact, levels = c("off", "amber", "blue", "green", "red","royalblue","white"))
stationary_side_new_activehalf$light_order<-as.factor(stationary_side_new_activehalf$light_order)
stationary_side_new_activehalf$type_order<-as.factor(stationary_side_new_activehalf$type_order)
stationary_side_new_activehalf$trial_number<-as.factor(stationary_side_new_activehalf$trial_number)
stationary_side_new_activehalf$control_light<- factor(stationary_side_new_activehalf$control_light, levels = c("control", "light", "both"))

stationary_side_new_activehalf<-stationary_side_new_activehalf[-1]

#######plot and tables first analysis######

##each colour mode
white_total<-subset(stationary_side_new_activehalf, stationary_side_new_activehalf$light_colour=="white")
royalblue_total<-subset(stationary_side_new_activehalf, stationary_side_new_activehalf$light_colour=="royalblue")
blue_total<-subset(stationary_side_new_activehalf, stationary_side_new_activehalf$light_colour=="blue")
green_total<-subset(stationary_side_new_activehalf, stationary_side_new_activehalf$light_colour=="green")
red_total<-subset(stationary_side_new_activehalf, stationary_side_new_activehalf$light_colour=="red")
amber_total<-subset(stationary_side_new_activehalf, stationary_side_new_activehalf$light_colour=="amber")


white_table<-white_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars((total_new)), funs(mean, sd))
rb_table<-royalblue_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars((total_new)), funs(mean, sd))
green_table<-green_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars((total_new)), funs(mean, sd))
blue_table<-blue_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars((total_new)), funs(mean, sd))
amber_table<-amber_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars((total_new)), funs(mean, sd))
red_table<-red_total %>% 
  group_by(light_mode) %>%
  summarize_at(vars((total_new)), funs(mean, sd))

white_plot<-ggplot(white_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "White light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
white_plot

rb_plot<-ggplot(royalblue_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Royal blue light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
rb_plot

green_plot<-ggplot(green_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Green light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
green_plot

blue_plot<-ggplot(blue_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Blue light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
blue_plot

amber_plot<-ggplot(amber_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Amber light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
amber_plot

red_plot<-ggplot(red_total, aes(x=light_mode, y=total_new)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Red light", 
       y = "Stationary time (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_fill_grey(start = 0.6, end = .9)
red_plot
##amber and red not much different. a lot of variation across colours.

###flash versus continuous###
red_ec<-ggplot(red_total, aes(x=light_mode, y=total_new, shape=light_type)) + 
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

blue_ec<-ggplot(blue_total, aes(x=light_mode, y=total_new, shape=light_type)) + 
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

amber_ec<-ggplot(amber_total, aes(x=light_mode, y=total_new, shape=light_type)) + 
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

white_ec<-ggplot(white_total, aes(x=light_mode, y=total_new, shape=light_type)) + 
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

rb_ec<-ggplot(royalblue_total, aes(x=light_mode, y=total_new, shape=light_type)) + 
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
rb_ec

green_ec<-ggplot(green_total, aes(x=light_mode, y=total_new, shape=light_type)) + 
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

##no visible differences

###Model####

hist(stationary_side_new_activehalf$total_new)
##kind of normally distributed 
hist(log(stationary_side_new_activehalf$total_new))
##left skewed
library(lmerTest)

##NEW ACTIVE MODEL.
linear1 <- lmer(total_new ~ 
                  +LightFact
                +light_type
                +(1|species)
                +wing_length
                +control_light
                +(1|light_side)
                +light_order
                +type_order
                +(1|trial_number), data = stationary_side_new_activehalf) 
summary(linear1)
###no diff betweenc control and light
##less "both" - less skates active full trial 
##more time stationary in white blue rblue green
#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: total_new ~ +LightFact + light_type + (1 | species) + wing_length +  
#    control_light + (1 | light_side) + light_order + type_order +      (1 | trial_number)
#   Data: stationary_side_new_activehalf

#REML criterion at convergence: 2755.9

#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-1.94814 -0.78438 -0.06962  0.79387  2.21565 

#Random effects:
# Groups       Name        Variance Std.Dev.
# trial_number (Intercept)    0.0    0.00   
# species      (Intercept)  378.5   19.45   
# light_side   (Intercept)    0.0    0.00   
# Residual                 8188.8   90.49   
#Number of obs: 244, groups:  trial_number, 22; species, 3; light_side, 2#

#Fixed effects:
#                    Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)         102.7604    31.0066   15.6517   3.314  0.00450 ** 
#LightFactamber       25.9972    20.4769  224.9518   1.270  0.20554    
#LightFactblue        70.3886    21.4252  225.3279   3.285  0.00118 ** 
#LightFactgreen       78.4990    23.9152  225.8173   3.282  0.00119 ** 
#LightFactred         20.7144    22.4720  225.2752   0.922  0.35762    
#LightFactroyalblue  116.7250    23.6754  225.3654   4.930 1.59e-06 ***
#LightFactwhite       58.6003    21.5690  224.4956   2.717  0.00710 ** 
#light_typeflashing   19.3676    12.1172  226.9800   1.598  0.11135    
#wing_length           0.2511     0.6687   54.0897   0.376  0.70873    
#control_lightlight  -12.8824    12.4344  226.1857  -1.036  0.30129    
#control_lightboth  -191.8902    23.2526  225.0905  -8.252 1.31e-14 ***
#light_order2        -12.4958    17.4668  226.2322  -0.715  0.47510    
#light_order3         -8.1953    17.9760  225.6231  -0.456  0.64890    
#light_order4         28.8225    21.7925  226.4998   1.323  0.18731    
#light_order5         57.2906    19.7379  224.9956   2.903  0.00407 ** 
#light_order6         31.1747    22.9422  225.4979   1.359  0.17556    
#type_order2           5.6699    12.2445  226.8420   0.463  0.64377    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation matrix not shown by default, as p = 17 > 12.
#Use print(x, correlation=TRUE)  or
#    vcov(x)        if you need it

#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')


ggarrange(white_plot, green_plot, rb_plot, blue_plot, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


######Second analysis#####
###Includes trials where skates were active or stationary for whole colour trials (not just in lights off or on conditions)
###But still need to remove trials where skates didnt move for entire 2 hours (skates 11,17,18,22,27)
skate_entire_trial2<-all_trials_skate%>% filter(trial_number != 11)
skate_entire_trial3<-skate_entire_trial2%>% filter(trial_number != 17)
skate_entire_trial4<-skate_entire_trial3%>% filter(trial_number != 18)
skate_entire_trial5<-skate_entire_trial4 %>% filter(trial_number != 22)
skate_entire_trial6<-skate_entire_trial5 %>% filter(trial_number != 27)
##correct
skate_reduced_list<- split(skate_entire_trial6, paste0(skate_entire_trial6$trial_number,sep="_", skate_entire_trial6$light_type, sep="_", skate_entire_trial6$light_colour))
##264 trials = total trials were 324. 60 trials removed.
skate_reduced<-skate_entire_trial6

skate_reduced$LightFact <- factor(skate_reduced$LightFact, levels = c("off", "amber", "blue", "green", "red","royalblue","white"))
skate_reduced$TypeFact <- factor(skate_reduced$TypeFact, levels = c("off", "flashing", "continuous"))
skate_reduced$control_light<- factor(skate_reduced$control_light, levels = c("control", "light", "both"))
skate_reduced$light_order<-as.factor(skate_reduced$light_order)
skate_reduced$type_order<-as.factor(skate_reduced$type_order)
skate_reduced$trial_number<-as.factor(skate_reduced$trial_number)
skate_reduced$light_side<-as.factor(skate_reduced$light_side)
skate_reduced$species<-as.factor(skate_reduced$species)


###ALL ACTIVE SKATES - UNDER MLS - SMALL
###GLMM
linear1 <- lmer(total_new ~ 
                  +LightFact
                +light_type
                +(1|species)
                +wing_length
                +control_light
                +(1|light_side)
                +light_order
                +type_order
                +(1|trial_number),REML = FALSE, 
                control = lmerControl(optimizer ="Nelder_Mead"), data = skate_reduced) 
summary(linear1)

###white loses significane. Model is a poorer fit,

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: total_new ~ +LightFact + light_type + (1 | species) + wing_length +  
#  control_light + (1 | light_side) + light_order + type_order +      (1 | trial_number)
#Data: skate_reduced
#Control: lmerControl(optimizer = "Nelder_Mead")

#AIC      BIC   logLik deviance df.resid 
#6448.1   6538.7  -3203.1   6406.1      531 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.1681 -0.5013  0.2573  0.6535  2.1965 

#Random effects:
#  Groups       Name        Variance Std.Dev.
#trial_number (Intercept)  685.2   26.18   
#species      (Intercept)    0.0    0.00   
#light_side   (Intercept)    0.0    0.00   
#Residual                 6087.8   78.02   
#Number of obs: 552, groups:  trial_number, 22; species, 3; light_side, 2#

#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)         187.7713    23.1256   39.2719   8.120 6.23e-10 ***
#  LightFactamber        7.0479    12.5149  530.5018   0.563  0.57356    
#LightFactblue        32.5610    12.5587  529.9787   2.593  0.00979 ** 
#  LightFactgreen       31.7310    12.6461  531.2107   2.509  0.01240 *  
#  LightFactred         18.1154    12.4668  529.6184   1.453  0.14679    
#LightFactroyalblue   51.8070    12.5259  529.6205   4.136 4.11e-05 ***
#  LightFactwhite       16.6516    12.4654  530.0601   1.336  0.18218    
#light_typeflashing    2.8946     6.7189  530.0133   0.431  0.66678    
#wing_length          -0.4890     0.6145   21.9066  -0.796  0.43468    
#control_lightlight    2.1890     7.3499  531.6330   0.298  0.76595    
#control_lightboth  -207.2041    11.8815  414.7214 -17.439  < 2e-16 ***
#  light_order2         14.9752    11.4000  535.5254   1.314  0.18954    
#light_order3         35.4013    11.8477  537.3031   2.988  0.00294 ** 
#  light_order4         80.7153    11.6674  534.8789   6.918 1.31e-11 ***
#  light_order5         80.6594    11.7886  536.0224   6.842 2.14e-11 ***
#  light_order6         85.3810    11.7162  535.2360   7.287 1.14e-12 ***
#  type_order2         -15.5024     6.7527  531.6960  -2.296  0.02208 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Correlation matrix not shown by default, as p = 17 > 12.
#Use print(x, correlation=TRUE)  or
#vcov(x)        if you need it

#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')


######skate variation in stationary behaviour with analysis 2####

##change to numeric as currently a factor
skate_reduced[,11] <- as.numeric(as.character(skate_reduced[,11]))

trial_1to11<-subset(skate_reduced, skate_reduced$trial_number< 12)
trial_12to26<-subset(skate_reduced, skate_reduced$trial_number>=12)
trial_1to11$trial_number <- as.factor(trial_1to11$trial_number)
trial_12to26$trial_number <- as.factor(trial_12to26$trial_number)

exp2<-ggplot(trial_1to11, aes(x=trial_number, y=total_new,shape=light_mode)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.5)+
  #geom_point(position=position_jitterdodge(dodge.width=0.1), size=1.5)+
  geom_point(position = position_jitterdodge(jitter.width = 0.1),size =1.75)+
  scale_shape_manual(values = c(1, 17),labels=c('Lights off', 'Lights on'))+
  labs(x = "Light colour modes", 
       y = "Stationary time (s)")+
  theme_bw()+
  ylim(0,300)+
  # theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=11))+
  theme(legend.title=element_blank())+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())+
  # scale_x_discrete(labels=c('Amber', 'Blue','Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.6, end = .9)
exp2

exp3<-ggplot(trial_12to26, aes(x=trial_number, y=total_new,shape=light_mode)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.5)+
  #geom_point(position=position_jitterdodge(dodge.width=0.1), size=1.5)+
  geom_point(position = position_jitterdodge(jitter.width = 0.1),size =1.75)+
  scale_shape_manual(values = c(1, 17),labels=c('Lights off', 'Lights on'))+
  labs(x = "Light colour modes", 
       y = "Stationary time (s)")+
  theme_bw()+
  ylim(0,300)+
  # theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=11))+
  theme(legend.title=element_blank())+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())+
  # scale_x_discrete(labels=c('Amber', 'Blue','Green', 'Red','Royal blue', 'White'))+
  scale_fill_grey(start = 0.6, end = .9)
exp3

require(grid) 
figure<-ggarrange(exp2, exp3,ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
annotate_figure(figure, left = textGrob("   Stationary time (s)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))

