##Script for cleaning data from video clips of tank trials at MBA
##tank trials took place between 13th May 2021 and 23rd August 2021
##Consisted of testing 38 catsharks
#The tank was 2.5m by 170cm tank, with a blue fabric background and sand/sediment bottom
##subjected fish to light around 50 watts**, a single LED made by SafetyNet Technologies)for 5 minutes, with 5 mins of no light immediately before
##6 light colours were used - blue, red, green, white, amber, royal blue 
##repeated for continuous and flashing light (4hz)
##one light was at either end of the tank - and I alternated between which side was switched on to rule out a tank-side bias 
##therefore, the light that was not  being switched on at one end of the tank was the control light
##videos analysed to see how fish responded to light, and whether certain colours influenced type of response
##Light interaction behaviour: #(light or control)_interaction = direct 'nudging' of the light
#behaviour was measured both either _on or _off conditions

library(dplyr)
library(ggplot2)
library(ggpubr)

#setwd
interaction<-read.csv("catsharks_light_interactions.csv")
trial_total<- split(interaction, paste0(interaction$trial_number,sep="_", interaction$light_type, sep="_", interaction$light_colour))
#252 different trials out of 456 total

###explore data - what type do we have?
##questions I am interested in
## first look at whether behaviour changes over time in linear model - eg during lights off and lights on treatment
##if we find time doesn't matter (e.g no effect of time on behaviour), then look at frequencies of behaviour
##Does light affect behaviour? - compare behaviour between 'on' and 'off' codes. 
##Does light colour affect behaviour? - compare behaviour across colours- for 'on' behaviour codes - more time stationary?
##Is there a difference in  behaviour between flashing and continuous light modes? -compare behaviour in across 2 treatments. 
##Do fish become habituated to light- 1) after initial exposure (during individual trial)? - 
#2) after whole trial period? (after exposure to all 6 colour modes)- trial number - does behaviour change depending on trial order

########################Data exploration##################################

##Does light affect behaviour? - on and off comparison
interaction_on<-interaction %>%
  filter(str_detect(light_mode, "on")) 
##260 different trials where light or control interactions occured when light on 

interaction_off<-interaction %>%
  filter(str_detect(light_mode, "off")) 
##142 occurences for light off-much less

###
sum(interaction$n)
#793 total interactions

interaction_table<-interaction %>% 
  group_by(light_mode, control_light, light_colour) %>%
  summarize_at(vars("n"), funs(mean, sd))
##mean difference between off and on in control and light/experimental 
##parts of the tank. less interactions in control


############PLOTS-DATA EXPLORATION##############
hist(interaction$n)
##count data, lots of '1' occurrences, right skewed data.
##poisson distribution

##plot on/off difference for each colour mode
#green
green_li<-subset(interaction, interaction$light_colour=="green")
blue_li<-subset(interaction, interaction$light_colour=="blue")
white_li<-subset(interaction, interaction$light_colour=="white")
rblue_li<-subset(interaction, interaction$light_colour=="royalblue")
red_li<-subset(interaction, interaction$light_colour=="red")
amber_li<-subset(interaction, interaction$light_colour=="amber")

greenp<-ggplot(green_li, aes(x=light_mode, y=n)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Green light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  scale_fill_grey(start = 0.6, end = .9)
greenp

bluep<-ggplot(blue_li, aes(x=light_mode, y=n)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Blue light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  scale_fill_grey(start = 0.6, end = .9)
bluep

royalbluep<-ggplot(rblue_li, aes(x=light_mode, y=n)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Royal blue light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  scale_fill_grey(start = 0.6, end = .9)
royalbluep

whitep<-ggplot(white_li, aes(x=light_mode, y=n)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "White light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  scale_fill_grey(start = 0.6, end = .9)
whitep

redp<-ggplot(red_li, aes(x=light_mode, y=n)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Red light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  scale_fill_grey(start = 0.6, end = .9)
redp


amberp<-ggplot(amber_li, aes(x=light_mode, y=n)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=2)+
  labs(x = "Amber light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  scale_fill_grey(start = 0.6, end = .9)
amberp


##does flashing or continuous effect this?
green_cf<-ggplot(green_li, aes(x=light_mode, y=n, shape=light_type)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing (4Hz)'))+
  labs(x = "Green light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
green_cf


blue_cf<-ggplot(blue_li, aes(x=light_mode, y=n, shape=light_type)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
    geom_point(position=position_jitterdodge(dodge.width=1))+
   scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing (4Hz)'))+
   labs(x = "Blue light", 
        y = "Light interaction count")+
   theme_bw()+
    theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
blue_cf

rb_cf<-ggplot(rblue_li, aes(x=light_mode, y=n, shape=light_type)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing (4Hz)'))+
  labs(x = "Royal blue light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
rb_cf

white_cf<-ggplot(white_li, aes(x=light_mode, y=n, shape=light_type)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing (4Hz)'))+
  labs(x = "White light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
white_cf

red_cf<-ggplot(red_li, aes(x=light_mode, y=n, shape=light_type)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing (4Hz)'))+
  labs(x = "Red light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
red_cf

amber_cf<-ggplot(amber_li, aes(x=light_mode, y=n, shape=light_type)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Continuous', 'Flashing (4Hz)'))+
  labs(x = "Amber light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
amber_cf

##seems flashing may have a slight effect - less interactions for white blue royal blue and green
##but some outliers

##experimental or control??
amber_ec<-ggplot(amber_li, aes(x=light_mode, y=n, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Amber light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
amber_ec

blue_ec<-ggplot(blue_li, aes(x=light_mode, y=n, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Blue light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
blue_ec

rblue_ec<-ggplot(rblue_li, aes(x=light_mode, y=n, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Royal blue light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
rblue_ec

green_ec<-ggplot(green_li, aes(x=light_mode, y=n, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Green light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
green_ec

white_ec<-ggplot(white_li, aes(x=light_mode, y=n, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "White light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
white_ec

red_ec<-ggplot(red_li, aes(x=light_mode, y=n, shape=control_light)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=1), width=0.9)+
  geom_point(position=position_jitterdodge(dodge.width=1))+
  scale_shape_manual(values = c(1, 17),labels=c('Control', 'Experimental'))+
  labs(x = "Red light", 
       y = "Light interaction count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size=11))+
  scale_fill_grey(start = 0.6, end = .9)
red_ec
###more interactions with control light - attraction?

interaction_table2<-interaction %>% 
  group_by(light_mode, control_light, light_colour) %>%
  summarize_at(vars("n"), funs(sum))

#######STATISTICAL ANALYSIS##########
interaction$LightFact <- factor(interaction$LightFact, levels = c("off", "amber", "blue", "green", "red","royalblue","white"))
interaction$TypeFact <- factor(interaction$TypeFact, levels = c("off", "flashing", "continuous"))
interaction$light_order<-as.factor(interaction$light_order)
interaction$type_order<-as.factor(interaction$type_order)
interaction$trial_number<-as.factor(interaction$trial_number)
interaction$light_side<-as.factor(interaction$light_side)

require(lmerTest)

test_li<-glmer(n~ LightFact
              +TypeFact
              +light_order
              +type_order
              +light_side
              +control_light
              +(1|trial_number),
              family="poisson",
              data=interaction)
summary(test_li)
##royalblue,blue,amer,green,white. not red
##experimental side- more interactions with that light when switched on

########paper plot#############
exp_interaction<-subset(interaction, interaction$control_light=="light")
exp_interaction_on<-subset(exp_interaction, exp_interaction$light_mode=="on")

amber<-subset(exp_interaction_on,exp_interaction_on$light_colour=="amber")
sum(amber$n)

exp_interaction<-ggplot(exp_interaction_on, aes(x=light_colour, y=n)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(width=0.275), width=0.9)+
  geom_point(position=position_jitter(width=0.275, height=0), size=1.75)+
  labs(x = "Light colour modes", 
       y = "Light interactions")+
  theme_bw()+
 # theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=15))+
  theme(legend.title=element_blank())+
  scale_x_discrete(labels=c('Amber', 'Blue','Green', 'Red','Royal blue', 'White'))+
  annotate("text", x = c(1,2,3,4,5,6), y=12.5, label = c("44, n=17","97, n=22","103, n=27","35, n=15","122, n=28","106, n=24"),size=4)+
  expand_limits(y=1.05*max(exp_interaction_on$n))+
  scale_fill_grey(start = 0.6, end = .9)
exp_interaction

ggsave(filename = "new_interaction.png", plot = exp_interaction, width = 12, height = 10, dpi = 300)


