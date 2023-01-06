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
  dplyr::select(OP_CODE, Scientific, maxn, Family, Genus, Species, TIMEFIRSTSEEN, TIMEMAXNUMSEEN, TIMEFIRSTFEED)%>%
 dplyr::glimpse()


# Join these datasets together

dat_2004 <- left_join(metadata, data, by="OP_CODE")%>%
  glimpse()

#Calculate has fed

dat_2004_fed <- dat_2004 %>%
  dplyr::rename(TimeFirstFed = TIMEFIRSTFEED)%>%
  mutate(HasFed = ifelse(TimeFirstFed > 0, "1", "0"))%>%
  dplyr::mutate(HasFed=as.numeric(HasFed))%>%
  dplyr::mutate(HasFed = replace_na(HasFed, 0)) %>%
  glimpse()

#Calculate delay to feed

dat_2004_delay <- dat_2004_fed %>%
  dplyr::rename(TimeFirstSeen = TIMEFIRSTSEEN)%>%
  dplyr::mutate(TimeFirstFed=as.numeric(TimeFirstFed))%>%
  dplyr::mutate(TimeFirstFed=as.numeric(TimeFirstFed))%>%
  mutate(DelayToFeed = TimeFirstFed-TimeFirstSeen)%>%
  dplyr::select(OP_CODE, Year, TimeFirstFed, Family, Genus, Species, Scientific, TimeFirstSeen,
                HasFed, DelayToFeed)%>%
  glimpse()

## Import 2016 time first seen data
setwd(dr.dir)

timefed_2016 <- read.csv(paste(study,"2016_timefed.csv",sep="_"))%>%
  dplyr::rename(OP_CODE= OP_CODE.AIMS)%>%
  dplyr::rename(TimeFirstFed= Time.first.fed..mins.)%>%
  dplyr::mutate(Scientific=paste(Family,Genus,Species,sep=" "))%>%
  dplyr:: mutate(Year=2016) %>%
  mutate_all(na_if,"")%>%
  dplyr::select(OP_CODE, Year, TimeFirstFed, Family, Genus, Species, Scientific) %>%
  na.omit()%>%
glimpse()

## Import 2016 time first seen data


timefirstseen_2016 <- read.csv(paste(study,"2016_timeseen.csv",sep="_"))%>%
  dplyr::rename(OP_CODE= OpCode)%>%
  dplyr::rename(TimeFirstSeen= Time.first.seen..mins.)%>%
  dplyr::mutate(Scientific=paste(Family,Genus,Species,sep=" "))%>%
  dplyr:: mutate(Year=2016) %>%
  mutate_all(na_if,"")%>%
  dplyr::select(OP_CODE, Year,TimeFirstSeen, Family, Genus, Species, Scientific) %>%
  na.omit()%>%
  glimpse()


#Combine these dataframes by OP_CODE and Scientific

dat_2016 <- full_join(timefirstseen_2016, timefed_2016, by=c("OP_CODE","Scientific", "Family", "Genus", "Species", "Year"))%>%
  glimpse()

#Calculate has fed

dat_2016_fed <- dat_2016 %>%
  mutate(HasFed = ifelse(TimeFirstFed > 0, "1", "0"))%>%
  dplyr::mutate(HasFed=as.numeric(HasFed))%>%
  dplyr::mutate(HasFed = replace_na(HasFed, 0)) %>%
  glimpse()

#Calculate delay to feed

dat_2016_delay <- dat_2016_fed %>%
  dplyr::mutate(TimeFirstFed=as.numeric(TimeFirstFed))%>%
  dplyr::mutate(TimeFirstFed=as.numeric(TimeFirstFed))%>%
  mutate(DelayToFeed = TimeFirstFed-TimeFirstSeen)%>%
  dplyr::select(OP_CODE, Year, TimeFirstFed, Family, Genus, Species, Scientific, TimeFirstSeen,
                HasFed, DelayToFeed)%>%
  glimpse()

## Combine 2004 and 2016 datasets 


dat <- full_join(dat_2016_delay, dat_2004_delay)%>%
  glimpse()

# Read in Opcode key
dir()
opcodes <- read.csv("Ashmore_BRUV_naming_key.csv")%>%
  glimpse()

# read in data from Zoe with metadata, MaxN and size class info

metadat <- read.csv("Ashmorebehaviour_zoe_analysis.csv")%>%
  dplyr::select(Date, Video, Year, Depth, Coral, Habitat, Complexity, 
              Treatment, Family, Genus_species, Genus, Species., Size_class., 
              MaxN_species_.standardised_hour.)%>%
  dplyr::rename(NAME.ANALYSIS.ZOE=Video)%>%
  dplyr::mutate(Year=as.numeric(Year))%>%
  glimpse()


# Join datasets by OP_CODE.AIMS

metadata_2004_2016 <- left_join(opcodes,metadat, by="NAME.ANALYSIS.ZOE")%>%
  dplyr::rename(Species=Species.)%>%
  dplyr::rename(OP_CODE=OP_CODE.AIMS)%>%
  dplyr::rename(Size_class=Size_class.)%>%
  dplyr::mutate(Scientific=paste(Family,Genus,Species,sep=" "))%>%
  dplyr::select("OP_CODE", "Year", "Depth", "Coral", "Habitat", "Complexity", "Treatment")%>%
  glimpse()



## Now add metadata_2004_2016 to dat by OP_CODE and Scientific
## Filter for families of interest

meta_behaviour_dat <- left_join(dat, metadata_2004_2016, by=c("OP_CODE", "Year"))%>%
  glimpse()

# make new df with families zoe analysed

species_analysed <- read.csv("Ashmorebehaviour_zoe_analysis.csv")%>%
  dplyr::select(Family, Genus_species, Genus, Species.)%>%
  dplyr::rename(Species=Species.)%>%
  dplyr::mutate(Scientific=paste(Family,Genus,Species, sep=" "))%>%
  dplyr::filter(! Family == c("No fish"))%>%
  unique()%>%
  glimpse()

summary(as.factor(species_analysed$Scientific))

# To do: 
# Add in size classes
# Use fish base max length and size categories from Speed et al. (2020) 
# Protection from illegal fishing and shark recovery restructures mesopredatory fish communities on a coral reef

#install.packages("rfishbase")
library("rfishbase")

# Make a species list with families from meta_behaviour_dat df

head(meta_behaviour_dat)

fish_fams <- meta_behaviour_dat %>%
  dplyr::select(Family)%>%
  unique()%>%
  glimpse()


fish <- species_list(Family = fish_fams$Family)
fish

lengthdat <- species(fish, fields=c("Species", "Length"))%>%
  glimpse()

lengthdat$Length <- round(lengthdat$Length, 0)
summary(lengthdat$Length)

# Use ifelse to assign size classes: “small” (≤50 cm TL), “medium” (50–100 cm TL), and “large” (>100 cm TL)

size_classes <- lengthdat %>%
  mutate(Size_class = ifelse(Length %in% 0:49, "Small",
                                    ifelse(Length %in% 50:99, "Medium",
                                           ifelse(Length %in% 100:5000, "Large", "Huge")))) %>%
  dplyr::rename(Gensp=Species)%>%
  glimpse()

## Add these size classes to the data

head(size_classes)

# need to make new genus species list to match fishbase data

meta_behaviour_data <- meta_behaviour_dat %>%
  dplyr::mutate(Gensp=paste(Genus,Species, sep=" "))%>%
  glimpse()
  
head(meta_behaviour_data)

meta_behaviour_size_dat <- left_join(meta_behaviour_data, size_classes, by=c("Gensp"))%>%
  glimpse()


# To do: add in column whether shark present, MaxN shark, and whether shark fed

#Carcharhinus amblyrhynchos and Triaenodon obesus were the most abundant sharks appearing in videos, alongside some less frequent observations of reef sharks including Nebrius ferrugineus, Sphyrna lewini and Stegostoma fasciatum.
#Galeocerdo cuvier
#Carangidae, Lutjanidae, Serranidae and Lethrinidae

meso_meta_behaviour_size_dat <- meta_behaviour_size_dat %>%
  filter(Family %in% c("Carcharhinidae", "Sphyrnidae", "Ginglymostomatidae", "Carangidae",
                       "Lutjanidae","Serranidae", "Lethrinidae"))%>%
  dplyr::mutate(OP_CODE=as.factor(OP_CODE))%>%
  glimpse()

summary(meso_meta_behaviour_size_dat$OP_CODE)

## Add in Shark present column

shark_meso_meta_behaviour_size_dat <- meso_meta_behaviour_size_dat %>%
  dplyr::mutate(OP_CODE=as.factor(OP_CODE))%>%
  dplyr::group_by(OP_CODE)%>%
  mutate(Shark_present = any(Family %in% c("Carcharhinidae", "Sphyrnidae", "Ginglymostomatidae"), na.rm = FALSE)) %>%
  ungroup()%>%
  unique()%>%
  glimpse()

