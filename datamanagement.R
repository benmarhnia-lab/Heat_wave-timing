library(xlsx)
library(Hmisc)
library(lubridate)
library(effects)
library(ggplot2)
library(prettyR)
library(segmented)
library(ggeffects)
library(devtools)
library(zoo)
library(knitr)
library(kableExtra)
library(tidyverse)
library("writexl")
library(hrbrthemes)
library(tidyverse)
library(ggridges)
library(viridis)
library(ggpubr)
library(R.utils)
library(meta)
library(jtools)
library(huxtable)


##################### CREATE DATA #############################################

#I create the database by merging data from CEPEM project and data from CEPIDC: 

# Load CepiDC data (only summer data)
load("C:/Users/aalari/Dropbox/Noemie/Analysis with CepiDC MF data/villes_s.RData")
# retain only cities not included in the SpF database
cpdc<-villes_s[c("avignon","saint_etienne","toulon")]
# exclude the year 2016 in order to be coherent with the SpF database

for (i in 1:length(cpdc)){
  cpdc[[i]]<-cpdc[[i]][which(cpdc[[i]]$Dates < "2016-01-01"),]
  cpdc[[i]]$annee<-as.factor(as.character(cpdc[[i]]$annee))  ## Drop Unused levels
 cpdc[[i]] <- cpdc[[i]]%>% rename (cv_tot=cv_cpdc, respi_tot=respi_cpdc)
  }

# Load SpF data (only summer data)
load("C:/Users/aalari/Dropbox/Noemie/Analysis with SpF Data/villes_s.RData")
villes_s<-c(villes_s,cpdc)
villes_s<-villes_s[order(names(villes_s))]


############################ HEAT WAVE VARIABLE ###############################

#We defined heat waves episodes in 21 different cities in French during the period going from 2000 to 2015. 

## Drop unused levels for month variable 
for (i in 1:length(villes_s)){
  villes_s[[i]]$mois<-as.factor(as.character(villes_s[[i]]$mois))
}

N_HeatWave<-matrix(NA,nrow = 21,ncol=6)
colnames(N_HeatWave)<-c("city","May","June","July","August","September")
N_HeatWave[,1]<-names(villes_s)

for (i in (1:length(villes_s))){
  N_HeatWave[i,2:6]<-as.numeric(t(villes_s [[i]]%>% group_by(mois)%>% summarise(
    N_HeatWave = sum(heat_wave)))[2,])
}
N_HeatWave%>% kable(caption = "Number of Heat Waves per city and per Month, 2000-2015")%>% kable_styling(bootstrap_options = c("hover", "condensed","bordered"),full_width = F)%>%
  column_spec(1, bold = T)%>%column_spec(2, color = "black",background = "lemonchiffon")%>%
  column_spec(6, color = "black",background = "lemonchiffon")

for (i in 1:length(villes_s)){
  villes_s[[i]]$heat_wave<-as.factor(as.character(villes_s[[i]]$heat_wave))
}


#There are no heat wave episodes during the months of may and september
# and in some cities also during the month of june (Bassin minier, Clermont-Ferrand, Lille, Marseille, Nice, Paris, Rouen). 
# ->I will exclude the month of may and september for a more appropiate definition of summer in FRANCE

############################## ORDER OF HEAT WAVE ################################

# Create a variable for order of heat wave


lapply(villes_s,function(x){print(x$Dates[which(x$heat_wave=="1")])})


# After a check of heat wave occurences, this is the list of cities and years when heat waves occured more than once per summer:
#   
#   - avignon: 2003 2006 2015
# - dijon: 2003 2015
# - grenoble: 2015
# - le havre: 2001 2003 2006
# - lyon: 2003 2015
# - marseille: 2003 2015
# - montpellier: 2003 2006 (2015)
# - nancy: 2015
# - nantes: 2003 2005
# - nice: 2006 2015
# - paris: 2006? 2015
# - rennes: 2001 2003 2006 
# - rouen: 2013
# - strasbourg: 2015
# - saint-etienne: 2003 2005 2015
# - toulon: 2006 2015
# - toulouse: 2003 
# 
# we have to be carefull as we saw that different extreme-heat episodes are separated by only one day: they should be then considered as part of the same heat wave.


## Define new variable "order of heat wave"
# I first put together heat wave separated by only one day:

for (i in 1:length(villes_s)){
  villes_s[[i]]$orderhw<-NA
  villes_s[[i]]$heat_wave<-as.numeric(as.character(villes_s[[i]]$heat_wave))
}

## Put together heat wave episodes separated only by one days 

for (i in 1:length(villes_s)){
  for (r in 2:nrow(villes_s[[i]])){
    villes_s[[i]]$heatwave[1]<-0  
    villes_s[[i]]$heatwave[r]<-ifelse(villes_s[[i]]$heat_wave[r-1]==1 & villes_s[[i]]$heat_wave[r+1]==1,1,villes_s[[i]]$heat_wave[r])
  }
}

lapply(villes_s,function(x){table(x$heatwave,x$heat_wave)})


#Here we can see that only a few days that were not considered heat wave according to the first definition have been now included in a heat wave episode in order to put together to heat waves separated only by one day. 

# With this new heat wave variable, I can now create the variable of order of heat waves:

year<-levels(villes_s[[1]]$annee)

#define first day for each heat wave
for (i in 1:length(villes_s)){
  for (r in 2:nrow(villes_s[[i]])){
    villes_s[[i]]$firstd[1]<-0
    villes_s[[i]]$firstd[r]<-if (villes_s[[i]]$heatwave[r]==1 & villes_s[[i]]$heatwave[r-1]==0) {1} else {0}
  }
}

## Creating variable for day of the season, going from 1 at the 1st of may of every year and until 153 for 30th of september (except of bisextiles years, 2000, 2004, 2008, 2012)

for (i in 1:length(villes_s)){
  villes_s[[i]]$day<-ifelse(villes_s[[i]]$annee %in% c("2000","2004","2008","2012"),as.numeric(strftime(villes_s[[i]]$Dates,format="%j"))-121,as.numeric(strftime(villes_s[[i]]$Dates, format = "%j"))-120)   
}

for (i in 1:length(villes_s)){ # pour exécuter la boucle pour chaque villes
  for (y in 1:16){ # pour exécuter la boucle pour chaque année
    for (r in 1:153){ # pour exécuter la boucle pour chaque jour 
      villes_s[[i]]$orderhw[which (villes_s[[i]]$annee == year[y] & villes_s[[i]]$day==r)]<-sum(villes_s[[i]]$firstd[which (villes_s[[i]]$annee == year[y])][1:r])
    }
  }
  # pour éviter que la somme des épisodes de HW se repete même les jours hors HW
  villes_s[[i]]$orderhw[which(villes_s[[i]]$heatwave==0)]<-0
}

lapply(villes_s,function(x){table(x$orderhw,x$annee)})


## I exclude the months of may and september
lapply(villes_s,function(x){levels(x$mois)})


for (i in 1:length(villes_s)){
  villes_s[[i]]<-villes_s[[i]][which(villes_s[[i]]$mois %in%  c("6","7","8")),]
  villes_s[[i]]$time<-1:nrow(villes_s[[i]])
}

lapply(villes_s,function(x){table(x$mois)})

## Drop unused levels for month variable 
for (i in 1:length(villes_s)){
  villes_s[[i]]$mois<-as.factor(as.character(villes_s[[i]]$mois))
}

### I create two alternative definition of order of heat wave:

# A: - one with the ordinal number of heat wave in the season (ordinal_hw)
# B: - the other with value 1 if the heat wave is the first in the season, 2 otherwise (order_heatw)

## A: ordinal_hw
for (i in 1:length(villes_s)){
  villes_s[[i]]$ordinal_hw<-as.factor(as.character(villes_s[[i]]$orderhw))
}

## B:order_heatw
for (i in 1:length(villes_s)){
  villes_s[[i]]$orderhw[which(villes_s[[i]]$orderhw>1)]<-2
}

for (i in 1:length(villes_s)){
  villes_s[[i]]$orderhw<-as.factor(as.character(villes_s[[i]]$orderhw))
}

lapply(villes_s,function(x){levels(x$orderhw)})


## save data
save(villes_s,file="C:/Users/aalari/Dropbox/Noemie/FInal Analysis - SpF + CepiDC/villes_s.RData")



