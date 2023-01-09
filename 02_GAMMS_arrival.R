## Script 2 ##
## run GAMMs for time to arrival ##

# Current bugs: 
# These are only videos says sharks are present so error in cleaning script. The others must have been lost in joins and filtering.. 

# set working directories ----

rm(list=ls())

w.dir <- ("~/Repositories/Ashmore_BRUVS")

# Set data directory - to read the data from
dt.dir <- (paste(w.dir, "Data/Tidy", sep='/'))
dr.dir <- (paste(w.dir, "Data/Raw", sep='/'))

# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
w.dir <- "C:/Users/elester/OneDrive - Australian Institute of Marine Science/Documents/Repositories/NWAUS_fish"

# Set model output directory
m.dir <- paste(w.dir,"Model_Outputs/Arrival", sep="/")

setwd(w.dir)
dir()

# librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
#library(gplots)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(ggplot2)
library(RCurl) #needed to download data from GitHub
library(patchwork)
library(png)


# install package----
#devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

# Bring in and format the data----

name<-"Ashmore_BRUVS_Arrival"
dir()

dat <-read.csv(paste(dt.dir, paste("Ashmore_04_16_tidy.csv", sep='.'), sep = '/'))%>%
  dplyr::mutate(OP_CODE=as.factor(OP_CODE))%>%
  dplyr::rename(Opcode = OP_CODE)%>%
  dplyr::mutate(Year=as.factor(Year))%>%
  dplyr::mutate(Scientific=as.factor(Scientific))%>%
  dplyr::mutate(Habitat=as.factor(Habitat))%>%
  dplyr::mutate(Complexity=as.factor(Complexity))%>%
  dplyr::mutate(Gensp=as.factor(Gensp))%>%
  dplyr::mutate(Size_class=as.factor(Size_class))%>%
  dplyr::mutate(Shark_present=as.factor(Shark_present))%>%
  filter(!Size_class=="NA")%>%
  dplyr::rename(response = TimeFirstSeen)%>%
  dplyr::mutate(Family=as.factor(Family))%>%
  filter(Family %in% c("Carangidae", "Lethrinidae", "Lutjanidae", "Serranidae"))%>%
  dplyr::select(Opcode, Year, Family:response, Depth, Coral, Habitat, Complexity, Gensp, Length, Size_class, Shark_present, MaxN, SumMaxN, Small_MaxN, Medium_MaxN, Large_MaxN)%>%
  unique()%>%
  droplevels()%>%
  filter(!Depth=="NA")%>%
  glimpse()

summary(dat$Size_class)

summary(dat$Family)

# Set predictor variables---
pred.vars=c("Depth","Coral","MaxN", "SumMaxN", "Small_MaxN", "Medium_MaxN", "Large_MaxN") 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
par(mfrow=c(1,1))
round(cor(dat[,pred.vars]),2)
summary(dat$response)
plot(dat$response)


# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i)) 
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
} 

# Review of individual predictors - we have to make sure they have an even distribution---
#If the data are squewed to low numbers try sqrt>log or if squewed to high numbers try ^2 of ^3
# log transform MaxNs, coral cover data is patchy, depth looks pretty good!


dat$log.SumMaxN<- log(dat$SumMaxN+1)
dat$log.Small_MaxN<- log(dat$Small_MaxN+1)
dat$log.Medium_MaxN<- log(dat$Medium_MaxN+1)
dat$log.Large_MaxN<- log(dat$Large_MaxN+1)


# # Re-set the predictors for modeling----
pred.vars=c("Depth","log.SumMaxN", "log.Small_MaxN", "log.Medium_MaxN", "log.Large_MaxN") 


#### Plot all against response
library(viridis)
colnames(dat)
par(mfrow=c(1,1))

ggdepth <- ggplot(dat, aes(x=Depth, y=response, fill=Size_class, colour=Size_class))+ facet_wrap(~Size_class)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggdepth+ theme(axis.text.x=element_text(size=15),
              axis.text.y=element_text(size=15),
              axis.title=element_text(size=20),
              strip.text =element_text(size=12),
              legend.position="none")

ggShark_present <- ggplot(dat, aes(x=Shark_present, y=response,fill=Size_class, colour=Size_class))+ facet_wrap(~Size_class)+
  geom_violin(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggShark_present+ theme(axis.text.x=element_text(size=15),
                axis.text.y=element_text(size=15),
                axis.title=element_text(size=20),
                strip.text =element_text(size=12),
                legend.position="none")

ggYear <- ggplot(dat, aes(x=Year, y=response,fill=Size_class, colour=Size_class))+ facet_wrap(~Size_class)+
  geom_violin(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggYear+ theme(axis.text.x=element_text(size=15),
                       axis.text.y=element_text(size=15),
                       axis.title=element_text(size=20),
                       strip.text =element_text(size=12),
                       legend.position="none")

ggCoral<- ggplot(dat, aes(x=Coral, y=response, fill=Size_class, colour=Size_class))+ facet_wrap(~Size_class)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggCoral + theme(axis.text.x=element_text(size=15),
                   axis.text.y=element_text(size=15),
                   axis.title=element_text(size=20),
                   strip.text =element_text(size=12),
                   legend.position="none")

gglog.SumMaxN <- ggplot(dat, aes(x=log.SumMaxN, y=response,fill=Size_class, colour=Size_class))+ facet_wrap(~Size_class)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
gglog.SumMaxN+ theme(axis.text.x=element_text(size=15),
                    axis.text.y=element_text(size=15),
                    axis.title=element_text(size=20),
                    strip.text =element_text(size=12),
                    legend.position="none")


gglog.Small_MaxN <- ggplot(dat, aes(x=log.Small_MaxN, y=response,fill=Size_class, colour=Size_class))+ facet_wrap(~Size_class)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
gglog.Small_MaxN + theme(axis.text.x=element_text(size=15),
                     axis.text.y=element_text(size=15),
                     axis.title=element_text(size=20),
                     strip.text =element_text(size=12),
                     legend.position="none")


gglog.Medium_MaxN <- ggplot(dat, aes(x=log.Small_MaxN, y=response,fill=Size_class, colour=Size_class))+ facet_wrap(~Size_class)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
gglog.Medium_MaxN + theme(axis.text.x=element_text(size=15),
                         axis.text.y=element_text(size=15),
                         axis.title=element_text(size=20),
                         strip.text =element_text(size=12),
                         legend.position="none")


gglog.Large_MaxN <- ggplot(dat, aes(x=log.Small_MaxN, y=response,fill=Size_class, colour=Size_class))+ facet_wrap(~Size_class)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
gglog.Large_MaxN + theme(axis.text.x=element_text(size=15),
                          axis.text.y=element_text(size=15),
                          axis.title=element_text(size=20),
                          strip.text =element_text(size=12),
                          legend.position="none")


# # Re-set the predictors for modeling---- (make sure you have enough data )
pred.vars=c("Depth","log.SumMaxN", "log.Small_MaxN", "log.Medium_MaxN", "log.Large_MaxN") 


glimpse(dat)

# Check to make sure Response vector has not more than 80% zeros----

unique.vars=unique(as.character(dat$Size_class))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Size_class==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     
colnames(dat)


# Run the full subset model selection----

head(dat)

resp.vars=unique.vars.use
use.dat=dat
factor.vars=c( "Year", "Shark_present", "Complexity" )# 
out.all=list()
var.imp=list()

glimpse(dat)
summary(dat)

setwd(m.dir)

par(mfrow=c(1,1))
plot(dat$response)

# Loop through the FSS function for each Taxa----

for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Size_class==resp.vars[i]),]
  
  Model1=gam(response~s(Depth,k=3,bs='cr'),
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               factor.factor.interactions = T,
                              # list(factor.vars=c"Shark_present", "Year")),#?
                               factor.smooth.interactions = F,
                              # list(
                              #   fact.vars=c("shark.presence"),
                              #   cont.vars=c("sd.relief","reef.cover")), #?
                               k=3,
                               cov.cutoff=0.3)
                              #null.terms="s(Opcode,bs='re')")
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=3),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models 
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}


# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))
all.mod.fits

