##Quantum catch model for skate, catsharks and plaice
#setwd

#set WD

############Data files####################
##LED COLOURS - irradiance measured 50cm from LED in dark room 
###7 different colour modes.
LED<-read.csv("LED_normalised_new.csv")
##species - generated from inputting peak sensitivities into Pigment Model excel doc.
##which is based on Govardovskiĭ et al 2000 pigment model
species2<-read.csv("catshark_plaice_ray_sensitivity.csv")
##ocular media files - based on Thorpe et al 1993 pigment classification
###or extracted from the literature from measurements (see Species vision document)
##see Vision datasheet for details on lens information
ray_lens<-read.csv("raja_lens_processed.csv")
plaice_lens<-read.csv("plaice_lens_processed.csv")
catshark_lens<-read.csv("raja_lens_processed.csv")
##same T50 value as ray

##blue polyester fabric background for MBA tank experiments, measured at 50cm in replicated dark conditions
blue_background_dark<-read.csv("blue_background_dark.csv")
##blue background light, measured in lighter conditions with white light
blue_background_light<-read.csv("blue_background_light.csv")

library(dplyr)
library(scales)
library(ggpubr)
library(ggplot2)

##-LED data is normalised for each colour mode (see 'normalising LED spectra' r script)
##species relative sensitivity is the photoreceptor curve for each species (derived from pigment template Govardovskii et al 2000)
##-All relative sensitivities are normalised between 0 and 1.
##-Categories = photoreceptor types (shorter wavelength: sw, medium: mw, longer: lw, rod)
##species = cod, anchovy etc
##dl=dark,light, unknown (the ambient light conditions that photoreceptor sensitivity was measured in, see Species vision)
##-Aim is to multiply each LED colour mode spectra by each species' sensitivity, 
##then get overall sum of photoreceptor stimulation for different light colour modes
##and for each photoreceptor a species has
##Do the same for background  - multiply photoreceptor sensitivity by background
##then get a ratio of LED to background as final model output
##-Ocular media files (how light transmits through eye structures before retina) for different species
##-These files have been processed from graph extraction software (see 'Ocular media extraction instructions' r script)


##########Species dataframe sorting####################
species_split <- split(species2, paste0(species2$photoreceptor,sep="_", species2$species, sep="_", species2$dl))
##split by species, photoreceptor type and dl
##when using bigger dataset, should be maximum of 4 different photoreceptor type
##so maximum of 4 lists per species.
species_split[["sw_plaice_dark"]]
##check out a particular list

a<-lapply(species_split, `[`, 4)

##create a dataframe that reduces lists to just the sensitivity of species,removes every column apart from 4th
#use 'a' to get seperate columns for each species
library("tidyverse")
species_rename<-map2(a, names(a),~rename_all(.x, function(z) paste(.y, z, sep = ".")))
species_df<-(as.data.frame(species_rename))
##add wavelength values in 1nm increments
species_df$wavelength=(300:700)

##next step is to multiply ocular media by photoreceptor sensitivity
##as each ocular media sensitivity is a different file for each species,
##there is separate lines of code for this, per species

#thornbackray
species_df$rod_thornbackray_unknown_OM_sensitivity<-species_df$rod_thornback.ray_unknown.sensitivity*ray_lens$sensitivity
##plaice sw
species_df$sw_plaice_dark_OM_sensitivity<-species_df$sw_plaice_dark.sensitivity*plaice_lens$sensitivity
##plaice mw
species_df$mw_plaice_dark_OM_sensitivity<-species_df$mw_plaice_dark.sensitivity*plaice_lens$sensitivity
##plaice lw
species_df$lw_plaice_dark_OM_sensitivity<-species_df$lw_plaice_dark.sensitivity*plaice_lens$sensitivity
##plaice rod
species_df$rod_plaice_dark_OM_sensitivity<-species_df$rod_plaice_dark.sensitivity*plaice_lens$sensitivity
##catshark rod
species_df$rod_catshark_dark_OM_sensitivity<-species_df$rod_catshark_dark.sensitivity*catshark_lens$sensitivity

##Create new dataframe for ocular media values
ocularmedia<-select(species_df, contains("OM_"))
##now to normalise all columns
##can apply a function to dataframe
normalise <- function(x, na.rm = TRUE) {
  ranx <- range(x, na.rm = na.rm)
  (x - ranx[1]) / diff(ranx)
}

ocularmedia1<-lapply(ocularmedia, FUN=normalise)
##makes into a list

#now to multiply every column in the list by LED values to get quantum catch
##values
white<-sapply(ocularmedia1,FUN= function(x) x*LED$white.norm)
colnames(white) <- paste("white", colnames(white), sep = "_")
##adds prefix of 'white' to all column names. repeat for all colour modes
blue<-sapply(ocularmedia1, FUN= function(x) x*LED$blue.norm)
colnames(blue) <- paste("blue", colnames(blue), sep = "_")
green<-sapply(ocularmedia1,FUN= function(x) x*LED$green.norm)
colnames(green) <- paste("green", colnames(green), sep = "_")
cyan<-sapply(ocularmedia1,FUN= function(x) x*LED$cyan.norm)
colnames(cyan) <- paste("cyan", colnames(cyan), sep = "_")
red<-sapply(ocularmedia1,FUN= function(x) x*LED$red.norm)
colnames(red) <- paste("red", colnames(red), sep = "_")
royalblue<-sapply(ocularmedia1,FUN= function(x) x*LED$royalblue.norm)
colnames(royalblue) <- paste("royalblue", colnames(royalblue), sep = "_")
amber<-sapply(ocularmedia1,FUN= function(x) x*LED$amber.norm)
colnames(amber) <- paste("amber", colnames(amber), sep = "_")

allcolours<-cbind(blue,white,cyan,green,red,royalblue,amber)
allcolours<-as.data.frame(allcolours)
#write.csv(allcolours, "allcoloursquantumcatch1nm.csv")

#########Quantum catch colour modes###########
##sum all colour modes to get quantum catch for each photoreceptor type of each species, for each colour mode
qcvalues<-colSums(allcolours)
qcvalues<-as.data.frame(qcvalues)
##adds new column with row name filled, so now dataframe has 2 columns
qcvalues<-setNames(cbind(rownames(qcvalues), qcvalues, row.names = NULL), 
                   c("species", "QC.light"))
##these are the 'raw' Quatum catch values, assuming no background light
##next step would be to find background irradiance total 
##sort original qc dataframe by colour
##separate by each "_"
library("stringr")
qcvalues2<-as.data.frame(str_split_fixed(qcvalues$species, "_", 6))
qcvalues3<-cbind(qcvalues2, qcvalues$QC.light)
##get rid of unnecessary OM and sensitivity column
qcvalues3<-subset(qcvalues3, select = -c(V5,V6))
colnames(qcvalues3)[1]=("colour")
colnames(qcvalues3)[2]=("photoreceptor")
colnames(qcvalues3)[3]=("species")
colnames(qcvalues3)[4]=("light_conditions_photoreceptors")
colnames(qcvalues3)[5]=("quantum_catch_OM")

######Blue fabric MBA background quantum catch#######

amber<-subset(qcvalues3, qcvalues3$colour=="amber")
##divide by quantum catch background!
royalblue<-subset(qcvalues3, qcvalues3$colour=="royalblue")
blue<-subset(qcvalues3, qcvalues3$colour=="blue")
cyan<-subset(qcvalues3, qcvalues3$colour=="cyan")
green<-subset(qcvalues3, qcvalues3$colour=="green")
white<-subset(qcvalues3, qcvalues3$colour=="white")
red<-subset(qcvalues3, qcvalues3$colour=="red")

##similar conditions to actual MBA tank
blue_fabric_dark_qc<-mapply(ocularmedia1,FUN= function(x) x*blue_background_dark$norm_radiance)
blue_fabric_dark_qc<-as.data.frame(blue_fabric_dark_qc)
blue_fabric_dark_qc2<-colSums(blue_fabric_dark_qc)
blue_fabric_dark_qc2<-as.data.frame(blue_fabric_dark_qc2)
##add new column with row name filled, so now dataframe has 2 columns
blue_fabric_dark_qc2<-setNames(cbind(rownames(blue_fabric_dark_qc2), blue_fabric_dark_qc2, row.names = NULL), 
                               c("species", "blue_fabric_dark_qc"))

##divide by quantum catch background. Similar conditions to actual MBA tank
amber$quantumcatch_blue_dark_bground<-amber$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
royalblue$quantumcatch_blue_dark_bground<-royalblue$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
blue$quantumcatch_blue_dark_bground<-blue$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
cyan$quantumcatch_blue_dark_bground<-cyan$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
green$quantumcatch_blue_dark_bground<-green$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
white$quantumcatch_blue_dark_bground<-white$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
red$quantumcatch_blue_dark_bground<-red$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
qc_final_jerlovIb<-rbind(royalblue,blue,cyan,green,amber,red,white)

###Blue fabric background light - more white light
blue_fabric_light_qc<-mapply(ocularmedia1,FUN= function(x) x*blue_background_light$norm_radiance)
blue_fabric_light_qc<-as.data.frame(blue_fabric_light_qc)
blue_fabric_light_qc2<-colSums(blue_fabric_light_qc)
blue_fabric_light_qc2<-as.data.frame(blue_fabric_light_qc2)
##add new column with row name filled, so now dataframe has 2 columns
blue_fabric_light_qc2<-setNames(cbind(rownames(blue_fabric_light_qc2), blue_fabric_light_qc2, row.names = NULL), 
                               c("species", "blue_fabric_light_qc"))

##divide by quantum catch background
amber$quantumcatch_blue_light_bground<-amber$quantum_catch_OM/blue_fabric_light_qc2$blue_fabric_light_qc
royalblue$quantumcatch_blue_light_bground<-royalblue$quantum_catch_OM/blue_fabric_light_qc2$blue_fabric_light_qc
blue$quantumcatch_blue_light_bground<-blue$quantum_catch_OM/blue_fabric_light_qc2$blue_fabric_light_qc
cyan$quantumcatch_blue_light_bground<-cyan$quantum_catch_OM/blue_fabric_light_qc2$blue_fabric_light_qc
green$quantumcatch_blue_light_bground<-green$quantum_catch_OM/blue_fabric_light_qc2$blue_fabric_light_qc
white$quantumcatch_blue_light_bground<-white$quantum_catch_OM/blue_fabric_light_qc2$blue_fabric_light_qc
red$quantumcatch_blue_light_bground<-red$quantum_catch_OM/blue_fabric_light_qc2$blue_fabric_light_qc
qc_final_jerlovIb<-rbind(royalblue,blue,cyan,green,amber,red,white)




##for each species, sum  each column and divide by length of each species group

library(dplyr)

qc_final_jerlov_light<- qc_final_jerlovIb %>%
  group_by(species, colour) %>%
  mutate(qc_total_light_bground=sum(quantumcatch_blue_light_bground)/length(species))

qc_final_jerlov_new <- qc_final_jerlov_light %>%
  group_by(species, colour) %>%
  mutate(qc_total_dark_bground=sum(quantumcatch_blue_dark_bground)/length(species))

qc_final_jerlov_new2<-qc_final_jerlov_new%>% distinct(colour, .keep_all = TRUE)



########PLAICE, SKATE AND CATSHARKS OCULAR MEDIA#######
ocularmedia_df<-as.data.frame(ocularmedia1)
ocularmedia_df$wavelength=(300:700)

library(ggplot2)

##theme for background of plots
Theme <- theme_bw()+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11),
        plot.title = element_text(size=17, hjust = 0.5),
        legend.position = "none", 
        strip.background = element_rect(fill="white", colour="white", size=1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

#plaice
PlaiceOM<-ggplot(data=(ocularmedia_df),aes(x = wavelength, y = swplaicedark.sensitivity))+
  geom_point(data=ocularmedia_df, aes(x = wavelength, y = sw_plaice_dark_OM_sensitivity), colour="red", size=0.5 )+
  geom_point(data=species_df, aes(x = wavelength, y = sw_plaice_dark.sensitivity), colour="black", size=0.5)+
  geom_point(data=ocularmedia_df, aes(x = wavelength, y = mw_plaice_dark_OM_sensitivity), colour="red", size=0.5 )+
  geom_point(data=species_df, aes(x = wavelength, y = mw_plaice_dark.sensitivity), colour="black", size=0.5)+
  geom_point(data=ocularmedia_df, aes(x = wavelength, y = lw_plaice_dark_OM_sensitivity), colour="red", size=0.5 )+
  geom_point(data=species_df, aes(x = wavelength, y = lw_plaice_dark.sensitivity), colour="black", size=0.5 )+
  geom_point(data=ocularmedia_df, aes(x = wavelength, y = rod_plaice_dark_OM_sensitivity), colour="red", size=0.5 )+
  geom_point(data=species_df, aes(x = wavelength, y = rod_plaice_dark.sensitivity), colour="black", size=0.5)+
  geom_vline(xintercept = 436, colour = 'darkturquoise', size=0.75)+ 
  geom_vline(xintercept = 456, colour = 'grey', size=0.75)+ 
  geom_vline(xintercept = 447, colour = 'blue', size =0.75)+
  geom_vline(xintercept = 471, colour = 'deepskyblue', size=0.75)+
  geom_vline(xintercept = 592, colour = 'darkorange', size=0.75)+
  geom_vline(xintercept = 518, colour = 'seagreen3', size=0.75)+
  geom_vline(xintercept = 627, colour = 'firebrick3', size =0.75)+
  labs(x = "Wavelength (nm)", 
       y = "Plaice relative sensitivity")+
  scale_x_continuous(breaks = seq (300,700,50), limits = c(300,700)) +
  Theme
PlaiceOM


##ocular media sensitivity for catsharks
catsharkOM<-ggplot()+
  geom_point(data=ocularmedia_df, aes(x = wavelength, y = rod_catshark_dark_OM_sensitivity), colour="red", size=0.5)+
  geom_point(data=species_df, aes(x = wavelength, y = rod_catshark_dark.sensitivity), colour="black", size=0.5 )+
  #geom_point(aes(x = wavelength, y =lw_plaice_dark_OM_sensitivity), colour="black", size=0.5 )+
  #geom_point(aes(x = wavelength, y = rod_plaice_dark_OM_sensitivity), colour="red", size=0.5 )+
  geom_vline(xintercept = 436, colour = 'darkturquoise', size=0.75)+ 
  geom_vline(xintercept = 456, colour = 'grey', size=0.75)+ 
  geom_vline(xintercept = 447, colour = 'blue', size =0.75)+
  geom_vline(xintercept = 471, colour = 'deepskyblue', size=0.75)+
  geom_vline(xintercept = 592, colour = 'darkorange', size=0.75)+
  geom_vline(xintercept = 518, colour = 'seagreen3', size=0.75)+
  geom_vline(xintercept = 627, colour = 'firebrick3', size =0.75)+
  labs(x = "Wavelength (nm)", 
       y = "Catshark relative sensitivity")+
  scale_x_continuous(breaks = seq (300,700,50), limits = c(300,700)) +
  Theme
catsharkOM

##skate
skateOM<-ggplot()+
  geom_point(data=ocularmedia_df, aes(x = wavelength, y = rod_thornbackray_unknown_OM_sensitivity), colour="red", size=0.5)+
  geom_point(data=species_df, aes(x = wavelength, y = rod_thornback.ray_unknown.sensitivity), colour="black", size=0.5 )+
  geom_vline(xintercept = 436, colour = 'darkturquoise', size=0.75)+ 
  geom_vline(xintercept = 456, colour = 'grey', size=0.75)+ 
  geom_vline(xintercept = 447, colour = 'blue', size =0.75)+
  geom_vline(xintercept = 471, colour = 'deepskyblue', size=0.75)+
  geom_vline(xintercept = 592, colour = 'darkorange', size=0.75)+
  geom_vline(xintercept = 518, colour = 'seagreen3', size=0.75)+
  geom_vline(xintercept = 627, colour = 'firebrick3', size =0.75)+
  labs(x = "Wavelength (nm)", 
       y = "Skate relative sensitivity")+
  scale_x_continuous(breaks = seq (300,700,50), limits = c(300,700)) +
  Theme
skateOM

ggarrange(PlaiceOM,catsharkOM,skateOM, ncol=1, nrow=3, common.legend = TRUE,
          legend = "bottom")




##led colours
led_normalised<-ggplot(data=(LED),aes(x = wavelength, y = white.norm))+
  geom_line(aes(x = wavelength, y = white.norm), colour="grey", size=0.5)+
  geom_line(aes(x = wavelength, y = blue.norm), colour="deepskyblue", size=0.5)+
  geom_line(aes(x = wavelength, y =green.norm), colour="seagreen3", size=0.5 )+
  geom_line(aes(x = wavelength, y =royalblue.norm), colour="blue", size=0.5 )+
  geom_line(aes(x = wavelength, y =red.norm), colour="firebrick3", size=0.5 )+
  geom_line(aes(x = wavelength, y =amber.norm), colour="darkorange", size=0.5 )+
  labs(x = "Wavelength (nm)")+ 
  labs(y = expression ("Normalised watts per"~m^2*nm))+
  scale_x_continuous(breaks = seq (300,700,50), limits = c(300,700)) +
  Theme
led_normalised



####### QUANTUM CATCH OUTPUTS#########
##remove row containing cyan as this wasnt tested
qc_final_jerlov_new3<-qc_final_jerlov_new2[- grep("cyan", qc_final_jerlov_new2$colour),]

##subset to get plaice
plaice<-subset(qc_final_jerlov_new3, qc_final_jerlov_new3$species=="plaice")
catshark<-subset(qc_final_jerlov_new3, qc_final_jerlov_new3$species=="catshark")
skate<-subset(qc_final_jerlov_new3, qc_final_jerlov_new3$species=="thornbackray")


###replicated (dark) conditions
cols <- c("turquoise4")

#catshark
colourplot_catshark<-arrange(catshark) %>%
  mutate(colour = factor(colour, levels=c("white", "green", "blue", "royalblue", "amber", "red"))) %>%
  ggplot(aes(fill=photoreceptor, y=qc_total_dark_bground, x=colour)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Light colour mode", 
       y = "Catshark visual stimulation")+
  theme_bw()+
  ylim(0,2.5)+
  theme(legend.position = "none")+ 
  theme(axis.title.x=element_blank())+
 # theme(axis.title.y=element_blank())+
  theme(text = element_text(size=11))+
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=14))+
  scale_x_discrete(labels=c('White','Green','Blue','Royal blue','Amber','Red'))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(name="rod", values = cols)
colourplot_catshark


##skate

cols <- c("turquoise3")

colourplot_skate<-arrange(skate) %>%
  mutate(colour = factor(colour, levels=c("white", "blue", "green", "royalblue", "amber", "red"))) %>%
  ggplot(aes(fill=photoreceptor, y=qc_total_dark_bground, x=colour)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Light colour mode", 
       y = "Skate visual stimulation")+
  theme_bw()+
  ylim(0,2.5)+
  theme(legend.position = "none")+ 
  theme(axis.title.x=element_blank())+
#  theme(axis.title.y=element_blank())+
  theme(text = element_text(size=11))+
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=14))+
  scale_x_discrete(labels=c('White','Blue','Green','Royal blue','Amber','Red'))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_fill_manual(name="rod", values = cols)
colourplot_skate


##plaice
cols <- c("darkslategrey")
colourplot_plaice<-arrange(plaice) %>%
  mutate(colour = factor(colour, levels=c("white", "blue", "green", "royalblue", "amber", "red"))) %>%
  ggplot(aes(fill=photoreceptor, y=qc_total_dark_bground, x=colour)) +
  geom_bar(position="stack", stat="identity")+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Light colour mode", 
       y = "Plaice visual stimulation")+
  theme_bw()+
  ylim(0,2.5)+
  theme(legend.position = "none")+ 
  theme(axis.title.x=element_blank())+
  theme(text = element_text(size=11))+
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=14))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_discrete(labels=c('White','Blue','Green','Royal blue','Amber','Red'))+
  scale_fill_manual(name="rod", values = cols)
colourplot_plaice

ggarrange(colourplot_plaice,colourplot_catshark,colourplot_skate, ncol=3, nrow=1)


##light conditions
#catshark
cols <- c("turquoise4")

colourplot_catshark<-arrange(catshark) %>%
  mutate(colour = factor(colour, levels=c("white", "green", "blue", "royalblue", "amber", "red"))) %>%
  ggplot(aes(fill=photoreceptor, y=qc_total_light_bground, x=colour)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Light colour mode", 
       y = "Catshark visual stimulation in tank")+
  theme_bw()+
  ylim(0,2.5)+
  theme(legend.position = "none")+ 
  theme(axis.title.x=element_blank())+
  # theme(axis.title.y=element_blank())+
  theme(text = element_text(size=15))+
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=14))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_discrete(labels=c('White','Green','Blue','Royal blue','Amber','Red'))+
  scale_fill_manual(name="rod", values = cols)
colourplot_catshark


##skate

cols <- c("turquoise3")

colourplot_skate<-arrange(skate) %>%
  mutate(colour = factor(colour, levels=c("white", "blue", "green", "royalblue", "amber", "red"))) %>%
  ggplot(aes(fill=photoreceptor, y=qc_total_light_bground, x=colour)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Light colour mode", 
       y = "Skate visual stimulation in tank")+
  theme_bw()+
  ylim(0,2.5)+
  theme(legend.position = "none")+ 
  theme(axis.title.x=element_blank())+
  #  theme(axis.title.y=element_blank())+
  theme(text = element_text(size=15))+
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=14))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_discrete(labels=c('White','Blue','Green','Royal blue','Amber','Red'))+
  scale_fill_manual(name="rod", values = cols)
colourplot_skate


##plaice
cols <- c("darkslategrey")
colourplot_plaice<-arrange(plaice) %>%
  mutate(colour = factor(colour, levels=c("white", "blue", "green", "royalblue", "amber", "red"))) %>%
  ggplot(aes(fill=photoreceptor, y=qc_total_light_bground, x=colour)) +
  geom_bar(position="stack", stat="identity")+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Light colour mode", 
       y = "Plaice visual stimulation in tank")+
  theme_bw()+
  ylim(0,2.5)+
  theme(legend.position = "none")+ 
  theme(axis.title.x=element_blank())+
  scale_x_discrete(labels = label_wrap(11)) +  theme(text = element_text(size=14))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  scale_x_discrete(labels=c('White','Blue','Green','Royal blue','Amber','Red'))+
  scale_fill_manual(name="rod", values = cols)
colourplot_plaice

ggarrange(colourplot_plaice,colourplot_catshark,colourplot_skate, ncol=3, nrow=1)

##########ocular media plots Thorpe et al#######
#plaice, skate, catshark ocular media
#plaice- type d. Catshark and skate - type a
##PLOT each on graph

ocmedia<-ggplot(data=(plaice_lens),aes(x = wavelength, y = sensitivity))+
  geom_point(data=(plaice_lens),aes(x = wavelength, y = sensitivity), colour="blue", size=0.5)+
  geom_point(data=(catshark_lens), aes(x = wavelength, y = sensitivity), colour="black", size=0.5 )+
  labs(x = "Wavelength (nm)", 
       y = "Transmission (%)")+
  scale_x_continuous(limits = c(300,500))+
  Theme

ocmedia

##fabric backgrounds plots

##light conditions
light<-ggplot(data=(blue_background_light),aes(x = wavelength, y = norm_radiance))+
  geom_line(data=(blue_background_light),aes(x = wavelength, y = norm_radiance), colour="blue", size=0.5)+
  labs(x = "Wavelength (nm)", 
       y = "Radiance [W/(sr*sqm*nm)]")+
  scale_x_continuous(limits = c(300,700))+
  Theme
light

##replicated tank conditions
dark<-ggplot(data=(blue_background_dark),aes(x = wavelength, y = norm_radiance))+
  geom_line(data=(blue_background_dark),aes(x = wavelength, y = norm_radiance), colour="darkblue", size=0.5)+
  labs(x = "Wavelength (nm)", 
         y = "Radiance [W/(sr*sqm*nm)]")+
    scale_x_continuous(limits = c(300,700))+
    Theme+  ggtitle("Replicated tank conditions")
dark
