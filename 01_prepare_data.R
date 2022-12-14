## prepare BRUV data for GAMM analysis ###

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)

# clear workspace ----
rm(list = ls())


# set working directories ----
w.dir <- ("~/Repositories/Ashmore_BRUVS")

# Set data directory - to read the data from
dt.dir <- (paste(w.dir, "Data/Tidy", sep='/'))
dr.dir <- (paste(w.dir, "Data/Raw", sep='/'))
# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.

study<-"Ashmore_BRUVS"


# Load data ----

# Import unchecked data from staging folder----
setwd(dr.dir)
dir()

# Import metadata ---
metadata<-read.csv(paste(study,"2004_metadata.csv",sep="_"))%>%
  dplyr::rename(Location= LOCATION)%>%
  dplyr::rename(Date= DATE)%>%
  dplyr::select(OPCODE,OP_CODE, Date, Year, Location, Lat, Lon, Coral, Habitat, Complexity)%>%
  glimpse()

# Import 2004 points file---

data <-read_csv(paste(study,"2004_Points.csv",sep="_"))%>%
  dplyr::rename(OP_CODE = Bruvs)%>%
  dplyr::rename(Scientific = Spec)%>%
  dplyr::rename(maxn = SumOfMAXNUM )%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::rename(Family = FAMILY )%>%
  dplyr::rename(Genus= GENUS)%>%
  dplyr::rename(Species= SPECIES)%>%
  dplyr::rename(Location= LOCATION)%>%
  dplyr::mutate(OP_CODE= str_replace(OP_CODE, ", ", "_"))%>%
 #dplyr::mutate(species=tolower(species))%>%
 #dplyr::select(campaignid,sample,family,genus,species,maxn)%>%
 replace_na(list(family="Unknown",genus="Unknown",species="spp"))%>% # remove any NAs in taxa name
 #dplyr::mutate(genus=ifelse((family%in%c("Carangidae")&species%in%c("sp10")),"Pseudocaranx",genus)) %>%
 #dplyr::mutate(species=ifelse((species%in%c("sp10")),"spp",species)) %>%
  dplyr::select(OP_CODE, Scientific, maxn, Location, Family, Genus, Species, TIMEFIRSTSEEN, TIMEMAXNUMSEEN, TIMEFIRSTFEED)%>%
 dplyr::glimpse()


# Join these datasets together

dat_2004 <- left_join(metadata, data, by="OP_CODE")%>%
  glimpse()


## Import 2016 time first seen data
setwd(dr.dir)

timefed_2016 <- read.csv(paste(study,"2016_timefed.csv",sep="_"))%>%
  dplyr::rename(OP_CODE= OP_CODE.AIMS)%>%
  dplyr::rename(TimeFirstFed= Time.first.fed..mins.)%>%
  dplyr::mutate(Scientific=paste(Family,Genus,Species,sep=" "))%>%
  dplyr:: mutate(Year=2016) %>%
  dplyr::select(OP_CODE,NAME.ANALYSIS.ZOE, Year, TimeFirstFed, Family, Genus, Species, Scientific) %>%
glimpse()

## Import 2016 time first seen data


