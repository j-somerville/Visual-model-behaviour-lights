##Quantum catch model for skate, catsharks and plaice
#setwd

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

##blue polyester fabric background for MBA tank experiments, measured at 50cm in dark and light conditions
blue_background_dark<-read.csv("blue_background_dark.csv")
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

##divide by quantum catch background!
amber$quantumcatch_blue_dark_bground<-amber$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
royalblue$quantumcatch_blue_dark_bground<-royalblue$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
blue$quantumcatch_blue_dark_bground<-blue$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
cyan$quantumcatch_blue_dark_bground<-cyan$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
green$quantumcatch_blue_dark_bground<-green$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
white$quantumcatch_blue_dark_bground<-white$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
red$quantumcatch_blue_dark_bground<-red$quantum_catch_OM/blue_fabric_dark_qc2$blue_fabric_dark_qc
qc_final_jerlovIb<-rbind(royalblue,blue,cyan,green,amber,red,white)


########PLAICE, SKATE AND CATSHARKS OCULAR MEDIA#######


library(ggplot2)

##theme for background of plots
Theme <- theme_bw()+
  theme(axis.text=element_text(size=13.5),
        axis.title=element_text(size=13.5),
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

catshark_red<-subset(red, red$species=="catshark")
catshark_amber<-subset(amber, amber$species=="catshark")
catshark_blue<-subset(blue, blue$species=="catshark")
catshark_royalblue<-subset(royalblue, royalblue$species=="catshark")
#plaice_cyan<-subset(cyan, cyan$species=="plaice")
catshark_green<-subset(green, green$species=="catshark")
catshark_white<-subset(white, white$species=="catshark")
catshark_qc<-rbind(catshark_royalblue,catshark_blue,catshark_green,catshark_amber,catshark_red,catshark_white)

cols <- c("turquoise3")

colourplot_catshark<-ggplot(catshark_qc, aes(fill=photoreceptor,y=quantumcatch_blue_dark_bground, x=colour)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Light colour mode", 
       y = "Catshark visual stimulation in tank")+
  theme_bw()+
  ylim(0,9)+
  theme(legend.position = "none")+       
  theme(text = element_text(size=10))+
  scale_x_discrete(labels = label_wrap(10)) +  theme(text = element_text(size=10))+
  scale_x_discrete(labels=c('Amber','Blue','Green','Red','Royal blue','White'))+
  scale_fill_manual(name="rod", values = cols)
colourplot_catshark


##skate
skate_red<-subset(red, red$species=="thornbackray")
skate_amber<-subset(amber, amber$species=="thornbackray")
skate_blue<-subset(blue, blue$species=="thornbackray")
skate_royalblue<-subset(royalblue, royalblue$species=="thornbackray")
#plaice_cyan<-subset(cyan, cyan$species=="plaice")
skate_green<-subset(green, green$species=="thornbackray")
skate_white<-subset(white, white$species=="thornbackray")
skate_qc<-rbind(skate_royalblue,skate_blue,skate_green,skate_amber,skate_red,skate_white)

cols <- c("turquoise3")

colourplot_skate<-ggplot(skate_qc, aes(fill=photoreceptor, y=quantumcatch_blue_dark_bground, x=colour)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Light colour mode", 
       y = "Skate visual stimualtion in tank")+
  ylim(0,9)+
  theme_bw()+
  theme(legend.position = "none")+       
  theme(text = element_text(size=10))+
  scale_x_discrete(labels = label_wrap(10)) +  theme(text = element_text(size=10))+
  scale_x_discrete(labels=c('Amber','Blue','Green','Red','Royal blue','White'))+
  scale_fill_manual(values = cols)
#  scale_fill_grey(start = 0.7, end=0.5)
colourplot_skate

##plaice
plaice_red<-subset(red, red$species=="plaice")
plaice_amber<-subset(amber, amber$species=="plaice")
plaice_blue<-subset(blue, blue$species=="plaice")
plaice_royalblue<-subset(royalblue, royalblue$species=="plaice")
#plaice_cyan<-subset(cyan, cyan$species=="plaice")
plaice_green<-subset(green, green$species=="plaice")
plaice_white<-subset(white, white$species=="plaice")
plaice_qc<-rbind(plaice_royalblue,plaice_blue,plaice_green,plaice_amber,plaice_red,plaice_white)

cols <- c("darkslategrey", "turquoise4", "turquoise3", "cadetblue1")

colourplot_plaice<-ggplot(plaice_qc, aes(fill=photoreceptor, y=quantumcatch_blue_dark_bground, x=colour)) +
  geom_bar(position="stack", stat="identity")+
  labs(x = "Light colour mode", 
       y = "Plaice visual stimualtion in tank",
       fill="Photoreceptor type")+
  ylim(0,9)+
  theme_bw()+
  theme(text = element_text(size=10))+
  scale_x_discrete(labels = label_wrap(10)) +  theme(text = element_text(size=10))+
  scale_x_discrete(labels=c('Amber','Blue','Green','Red','Royal blue','White'))+
  scale_fill_manual(values=c("darkslategrey", "turquoise4", "turquoise3", "cadetblue1"), 
                    name="Photoreceptor type",
                    breaks=c("lw", "mw", "rod","sw"),
                    labels=c("LW", "MW", "Rod","SW"))
colourplot_plaice

#scale_fill_grey(start = 0.3, end=0.9, labels=c('LW', 'MW', 'Rod', 'SW'))


ggarrange(colourplot_plaice,colourplot_catshark+theme(legend.position="none"),colourplot_skate+theme(legend.position="none"), ncol=3, nrow=1, common.legend = TRUE,
          legend = "bottom")

##########ocular media plots Thorpe et al#######
#plaice, skate, catshark ocular media
#plaice- type d. Catshark and skate - type a
##PLOT each on graph

chap2ocmedia<-ggplot(data=(plaice_lens),aes(x = wavelength, y = sensitivity))+
  geom_point(data=(plaice_lens),aes(x = wavelength, y = sensitivity), colour="blue", size=0.5)+
  geom_point(data=(catshark_lens), aes(x = wavelength, y = sensitivity), colour="black", size=0.5 )+
  labs(x = "Wavelength (nm)", 
       y = "Transmission (%)")+
  scale_x_continuous(limits = c(300,500))+
  Theme

chap2ocmedia

