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
m.dir <- paste(w.dir,"Model_Out", sep="/")

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
  rename(response = TimeFirstSeen)%>%
  dplyr::select(Opcode, Year, Family:response, Depth, Coral, Habitat, Complexity, Gensp, Length, Size_class, Shark_present, MaxN, Small_MaxN, Medium_MaxN, Large_MaxN)%>%
  unique()%>%
  dplyr::group_by(Opcode)%>%
  dplyr::mutate(SumMaxN = sum(MaxN))%>%
  ungroup()%>%
  filter(!Depth=="NA")%>%
  glimpse()

summary(dat$Size_class)


# Set predictor variables---
pred.vars=c("Depth","Coral","MaxN", "SumMaxN", "Small_MaxN", "Medium_MaxN", "Large_MaxN") 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---

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
pred.vars=c("Depth","Coral","log.SumMaxN", "log.Small_MaxN", "log.Medium_MaxN", "log.Large_MaxN") 


#### Plot all against response
library(viridis)
colnames(dat)


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

summary(dat$Shark_present)
ggstatus <- ggplot(dat, aes(x=status, y=response,fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_violin(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggstatus+ theme(axis.text.x=element_text(size=15),
                axis.text.y=element_text(size=15),
                axis.title=element_text(size=20),
                strip.text =element_text(size=12),
                legend.position="none")



ggsd.relief <- ggplot(dat, aes(x=sd.relief, y=response, fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggsd.relief+ theme(axis.text.x=element_text(size=15),
                   axis.text.y=element_text(size=15),
                   axis.title=element_text(size=20),
                   strip.text =element_text(size=12),
                   legend.position="none")

ggshark.maxn <- ggplot(dat, aes(x=sharkmaxn, y=response,fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggshark.maxn+ theme(axis.text.x=element_text(size=15),
                    axis.text.y=element_text(size=15),
                    axis.title=element_text(size=20),
                    strip.text =element_text(size=12),
                    legend.position="none")

colnames(dat)
ggshark.trophic <- ggplot(dat, aes(x=shark.presence, y=response, fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_violin(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggshark.trophic+ theme(axis.text.x=element_text(size=15),
                       axis.text.y=element_text(size=15),
                       axis.title=element_text(size=20),
                       strip.text =element_text(size=12),
                       legend.position="none")

ggshark.trophic <- ggplot(dat, aes(x=shark.presence, y=response, fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_violin(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggshark.trophic+ theme(axis.text.x=element_text(size=15),
                       axis.text.y=element_text(size=15),
                       axis.title=element_text(size=20),
                       strip.text =element_text(size=12),
                       legend.position="none")


ggshark.fed<- ggplot(dat, aes(x=sharkhasfed, y=response, fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggshark.fed+ theme(axis.text.x=element_text(size=15),
                   axis.text.y=element_text(size=15),
                   axis.title=element_text(size=20),
                   strip.text =element_text(size=12),
                   legend.position="none")


gglog.depth <- ggplot(dat, aes(x=depth, y=response,fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
gglog.depth+ theme(axis.text.x=element_text(size=15),
                   axis.text.y=element_text(size=15),
                   axis.title=element_text(size=20),
                   strip.text =element_text(size=12),
                   legend.position="none")


gglog.dst2ramp <- ggplot(dat, aes(x=sqrt.dst2ramp, y=response,fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
gglog.dst2ramp+ theme(axis.text.x=element_text(size=15),
                      axis.text.y=element_text(size=15),
                      axis.title=element_text(size=20),
                      strip.text =element_text(size=12),
                      legend.position="none")


gglarge <- ggplot(dat, aes(x=log.large, y=response, fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
gglarge+ theme(axis.text.x=element_text(size=15),
               axis.text.y=element_text(size=15),
               axis.title=element_text(size=20),
               strip.text =element_text(size=12),
               legend.position="none")


ggsmall<- ggplot(dat, aes(x=log.small, y=response, fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggsmall+ theme(axis.text.x=element_text(size=15),
               axis.text.y=element_text(size=15),
               axis.title=element_text(size=20),
               strip.text =element_text(size=12),
               legend.position="none")


ggall<- ggplot(dat, aes(x=log.all, y=response, fill=size_class_new, colour=size_class_new))+ facet_wrap(~size_class_new)+
  geom_point(size=3, alpha=0.7)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_classic()
ggall+ theme(axis.text.x=element_text(size=15),
             axis.text.y=element_text(size=15),
             axis.title=element_text(size=20),
             strip.text =element_text(size=12),
             legend.position="none")


# # Re-set the predictors for modeling---- (make sure you have enough data )
pred.vars=c("sd.relief", "sqrt.dst2ramp",  "reef.cover","log.large", "log.small") 


glimpse(dat)

# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(dat$size_class_new))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$size_class==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     
colnames(dat)


# Run the full subset model selection----
#setwd("G:/My Drive/PhD_Emily/Chapters/Chapter 2 Scott Reef and Rowleys/Analysis_Lester_Chapter2_2019_model_out") #Set wd for example outputs - will differ on your computer
resp.vars=unique.vars.use
use.dat=dat
factor.vars=c( "status", "shark.presence")# 
out.all=list()
var.imp=list()

glimpse(dat)
summary(dat)

setwd(m.dir)

# Loop through the FSS function for each Taxa----

for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$size_class_new==resp.vars[i]),]
  
  Model1=gam(response~s(sd.relief,k=3,bs='cr') + s(year,bs='re')+ s(mean.lat,bs='re'),
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=factor.vars,
                               factor.factor.interactions = F, #?
                               factor.smooth.interactions = T,
                               list(
                                 fact.vars=c("shark.presence"),
                                 cont.vars=c("sd.relief","reef.cover")), #?
                               k=3,
                               cov.cutoff=0.3,
                               null.terms="s(year,bs='re') + s(mean.lat,bs='re') + depth")
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


##############################################################################
############ Part 2 - custom plot of importance scores---- ###################
##############################################################################

dir()
# Load the importance score dataset produced above
# dat.taxa <-read.csv(text=getURL("https://raw.githubusercontent.com/beckyfisher/FSSgam/master/case_study2_model_out/clams_all.var.imp.csv"))%>% #from github
dat.all <-read.csv("TOA.R1_all.var.imp.csv")%>% #from local copy
  dplyr::rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()


# Plotting defaults----
library(ggplot2)
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=8),
    legend.title = element_text(size=10, face="bold"),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 20,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=20),
    axis.title.y=element_text(vjust=0.6, angle=90, size=20),
    axis.text.x=element_text(size=15,angle = 45, hjust=1,vjust=1),
    axis.text.y=element_text(size=15,angle = 0, hjust=1,vjust=0.5),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    # axis.ticks.y =element_blank(),
    plot.margin = margin(2,4,2,5, "cm"),#trbl
    strip.background = element_blank())


# colour ramps-
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

# Labels-
legend_title<-"Importance"
dat.all
# Annotations-  ##come back to this when you want to add in X's to important variables in the moedl
dat.label<-dat.all%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor=="log.large"&resp.var=="large","X",ifelse(predictor=="sd.relief"&resp.var=="large","X",ifelse(predictor=="sqrt.dst2ramp"&resp.var=="large","X",label))))%>%
  #mutate(label=ifelse(predictor=="sd.relief"&resp.var=="medium","X",ifelse(predictor=="log.medium"&resp.var=="medium","X",ifelse(predictor=="sqrt.dst2ramp"&resp.var=="medium","X",label))))%>%
  mutate(label=ifelse(predictor=="log.small"&resp.var=="small","X",ifelse(predictor=="sd.relief"&resp.var=="small","X",label)))%>%
  # mutate(label=ifelse(predictor=="reef.cover"&resp.var=="ray","X",ifelse(predictor=="log.large"&resp.var=="ray","X",ifelse(predictor=="shark.presence"&resp.var=="ray","X",label))))%>%
  glimpse()

##order factors
dat.label
# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(0, 1.001))+
  scale_x_discrete(limits=c("sd.relief",
                            #"depth.cat",
                            "reef.cover",
                            "sqrt.dst2ramp",
                            "status",
                            "shark.presence",
                            #"log.all",
                            "log.large", 
                            # "log.medium", 
                            "log.small"),
                   labels=c("Sd relief",
                            #"Depth",
                            "Reef cover",
                            "Sqrt distance to ramp",
                            "Status",
                            "Shark presence",
                            # "Log all competitors",
                            "Log large competitors",
                            #"Log medium competitors",
                            "Log small competitors"))+
  scale_y_discrete(limits = c("small",
                              "large"),
                   
                   labels=c("Small",
                            "Large"))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1 + 
  geom_text(aes(label=label),size=8)

gg.importance.scores

setwd(p.dir)
ggsave("TOAimp__R1.tiff", plot=gg.importance.scores, height=5.2, width=8,device="tiff", dpi=300)

##############################################################################
############ Part 3 - plots of the most parsimonious models----  #############
##############################################################################


### now  make a nice plot of the most interesting models-----
library(gridExtra)
library(grid)
library(ggplot2)

# Theme----
Theme2 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=25),
    legend.title = element_blank(),
    plot.title=element_text(size=17),
    text=element_text(size=25),
    strip.text.y = element_text(size = 27,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=16),
    axis.title.y=element_text(vjust=0.6, angle=90, size=18),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    axis.line.x=element_line(colour="black", size=1,linetype='solid'),
    axis.line.y=element_line(colour="black", size=1,linetype='solid'),
    #plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),  #trbl
    strip.background = element_blank())

# Bring in and format the raw data----

glimpse(dat)
summary(dat$gen.sp)

# Manually make the most parsimonious GAM models for each taxa ----
#setwd("~/GitHub/FSSgam/case_study2_model_out")

########################################################################################
# top model large ----
########################################################################################
datlarge <- dat%>%
  filter(size_class_new=="large")%>%
  droplevels()%>%
  glimpse()



gamm=gam(response~ s(log.large, k=3, bs="cr") + sd.relief + sqrt.dst2ramp + s(year,bs="re") + s(mean.lat, bs="re") + log.depth, family=tw(),data=datlarge)
gam.check(gamm)
summary(gamm)
mod<-gamm
plot(gamm)

#model predictions for log.large ----


testdata1 <- expand.grid(log.large = seq(min(datlarge$log.large),max(datlarge$log.large),length.out = 20),
                         sd.relief = mean(mod$model$sd.relief),
                         sqrt.dst2ramp = mean(mod$model$sqrt.dst2ramp),
                         mean.lat= (mod$model$mean.lat),
                         year=(mod$model$year),
                         log.depth = mean(mod$model$log.depth))%>%
  distinct()%>%
  glimpse()



head(testdata1)
fits1 <- predict.gam(mod, newdata=testdata1, type='response', se.fit=T)
# head(fits,2)


predicts.log.large = testdata1%>%data.frame(fits1)%>%
  group_by(log.large)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.log.large

## Read in fish pic ----
dir()
size = unit(10, "cm")
largepic <- readPNG("large.png")%>%
  rasterGrob(interpolate=TRUE)


## plot large----

ggmod.log.large <-  ggplot(aes(x=log.large ,y=response), data=predicts.log.large)+
  ylab("
       ")+
  xlab('Log no. large competitors')+
  # geom_point(data=datlarge, aes(x=log.large, y=response), alpha=0.1, colour="#293462")+
  geom_line(data=predicts.log.large,aes(x=log.large,y=response),colour="#293462",alpha=0.8,size=1,show.legend=TRUE)+
  geom_ribbon(aes(ymin=response-se.fit, ymax=response + se.fit), alpha=0.4, fill="#293462", linetype='blank')+
  theme_classic()+
  ylim(0,40)+
  ggtitle("Large")+
  Theme2 
# annotation_custom(largepic, xmin=1.44, xmax=3.44, ymin=41, ymax=53)
# annotate("text", x = -Inf, y=Inf, label = "B",vjust = 1, hjust = -.1,size=6)

ggmod.log.large + guides(fill=FALSE) + theme(legend.position = "none") 
+ geom_rug(data=datlarge, aes(x=log.large, y=response), sides="b", alpha=0.8, size=1, colour="#293462")

a<- ggmod.log.large + guides(fill=FALSE) + theme(legend.position = "none") + geom_rug(data=datlarge, aes(x=log.large, y=response), sides="b", alpha=0.8, size=1, colour="#293462")




#model predictions for sd relief


testdata2 <- expand.grid(sd.relief = seq(min(datlarge$sd.relief),max(datlarge$sd.relief),length.out = 20),
                         log.large = mean(mod$model$log.large),
                         sqrt.dst2ramp = mean(mod$model$sqrt.dst2ramp),
                         log.depth = mean(mod$model$log.depth),
                         mean.lat= (mod$model$mean.lat),
                         year=(mod$model$year))%>%
  distinct()%>%
  glimpse()

fits2 <- predict.gam(mod, newdata=testdata2, type='response', se.fit=T)
# head(fits,2)


predicts.sd.relief = testdata2%>%data.frame(fits2)%>%
  group_by(sd.relief)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.sd.relief

## Colour


ggmod.sd.relief <-  ggplot(aes(x=sd.relief, y=response), data=predicts.sd.relief)+
  ylab("
       ")+
  xlab('Sd relief ')+
  # geom_point(data=datlarge, aes(x=sd.relief, y=response), alpha=0.1, colour="#293462")+
  geom_line(data=predicts.sd.relief ,aes(x=sd.relief ,y=response),colour="#293462",alpha=0.8,size=1,show.legend=TRUE)+
  geom_ribbon(aes(ymin=response-se.fit, ymax=response + se.fit), alpha=0.4, fill="#293462", linetype='blank')+
  theme_classic()+
  ylim(0,40)+
  #ggtitle("Large")+
  scale_x_continuous(breaks=seq(0, 12, 2))+
  Theme2
#annotation_custom(largepic, xmin=0.3, xmax=2.44, ymin=38, ymax=53)+
#annotate("text", x = -Inf, y=Inf, label = "A Large",vjust = 1, hjust = -.2,size=6)

ggmod.sd.relief + guides(fill=FALSE) + theme(legend.position = "none") 
#+ geom_rug(data=datlarge, aes(x=sd.relief , y=response), sides="b", alpha=0.8, size=1, colour="#293462")

b <- ggmod.sd.relief + guides(fill=FALSE) + theme(legend.position = "none") + geom_rug(data=datlarge, aes(x=sd.relief , y=response), sides="b", alpha=0.8, size=1, colour="#293462")



#model predictions for sqrt dst2ramp

testdata3 <- expand.grid(sqrt.dst2ramp = seq(min(datlarge$sqrt.dst2ramp),max(datlarge$sqrt.dst2ramp),length.out = 20),
                         log.large = mean(mod$model$log.large),
                         sd.relief = mean(mod$model$sd.relief),
                         mean.lat= (mod$model$mean.lat),
                         log.depth = mean(mod$model$log.depth),
                         year=(mod$model$year))%>%
  distinct()%>%
  glimpse()

fits3 <- predict.gam(mod, newdata=testdata3, type='response', se.fit=T)
# head(fits,2)


predicts.sqrt.dst2ramp = testdata3%>%data.frame(fits3)%>%
  group_by(sqrt.dst2ramp)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.sqrt.dst2ramp

## Colour


ggmod.sqrt.dst2ramp <-  ggplot(aes(x=sqrt.dst2ramp, y=response), data=predicts.sqrt.dst2ramp)+
  ylab("
       ")+
  xlab('Sqrt distance to ramp')+
  # geom_point(data=datlarge, aes(x=sd.relief, y=response), alpha=0.1, colour="#293462")+
  geom_line(data=predicts.sqrt.dst2ramp ,aes(x=sqrt.dst2ramp,y=response),colour="#293462",alpha=0.8,size=1,show.legend=TRUE)+
  geom_ribbon(aes(ymin=response-se.fit, ymax=response + se.fit), alpha=0.4, fill="#293462", linetype='blank')+
  theme_classic()+
  ylim(0,40)+
  #scale_x_continuous(breaks=seq(0, 12, 2))+
  Theme2 
#  annotation_custom(largepic, xmin=0.3, xmax=2.44, ymin=38, ymax=53)+
# annotate("text", x = -Inf, y=Inf, label = "C",vjust = 1, hjust = -.2,size=6)

ggmod.sqrt.dst2ramp + guides(fill=FALSE) + theme(legend.position = "none") 
#+ geom_rug(data=datlarge, aes(x=sqrt.dst2ramp , y=response), sides="b", alpha=0.8, size=1, colour="#293462")

c<- ggmod.sqrt.dst2ramp + guides(fill=FALSE) + theme(legend.position = "none") + geom_rug(data=datlarge, aes(x=sqrt.dst2ramp , y=response), sides="b", alpha=0.8, size=1, colour="#293462")

large.plot <- a+b+c + plot_annotation(tag_levels = "A")
large.plot


########################################################################################
# top model small
########################################################################################
datsmall <- dat%>%
  filter(size_class_new=="small")%>%
  droplevels()%>%
  glimpse()

gamm4=gam(response~ s(log.small, k=3, bs="re") + sd.relief + log.depth + s(year,bs="re") + s(mean.lat, bs="re"), family=tw(),data=datsmall)
gam.check(gamm4)
summary(gamm4)
mod4<-gamm4
plot(gamm4)

# predictions for log small

testdata7 <- expand.grid(log.small=seq(min(datsmall$log.small),max(datsmall$log.small),length.out = 20),
                         log.depth = mean(mod4$model$log.depth),
                         sd.relief = mean(mod4$model$sd.relief),
                         mean.lat= (mod4$model$mean.lat),
                         year=(mod4$model$year))%>%
  distinct()%>%
  glimpse()



head(testdata7)
fits7 <- predict.gam(mod4, newdata=testdata7, type='response', se.fit=T)
# head(fits,2)


predicts.log.small.s= testdata7%>%data.frame(fits7)%>%
  group_by(log.small)%>% #only change here
  # group_by(sqrt.X500um,Status)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.log.small.s


## Read in fish pic
dir()
size = unit(10, "cm")
smallpic <- readPNG("small.png")%>%
  rasterGrob(interpolate=TRUE)


## Colour

ggmod.log.small.s <-  ggplot(aes(x=log.small, y=response), data=predicts.log.small.s)+
  ylab("
       ")+
  xlab('Log no. small competitors')+
  geom_line(data=predicts.log.small.s, aes(x=log.small,y=response),colour="#00818a",alpha=0.8,size=1,show.legend=TRUE)+
  geom_ribbon(aes(ymin=response-se.fit, ymax=response + se.fit), alpha=0.4, fill="#00818a", linetype='blank')+
  ylim(0,20)+
  theme_classic()+
  ggtitle("Small")+
  # ylim(0,25)+
  Theme2 
# annotation_custom(smallpic, xmin=0.4, xmax=2.3, ymin=23, ymax=26)+
# annotate("text", x = -Inf, y=Inf, label = "(e) Small",vjust = 1, hjust = -.1,size=6)


ggmod.log.small.s + guides(fill=FALSE) + theme(legend.position = "none") 
#+ geom_rug(data=datsmall, aes(x=log.small, y=response), sides="b", alpha=0.8,  size=1, colour="#00818a")

d <- ggmod.log.small.s + guides(fill=FALSE) + theme(legend.position = "none") + geom_rug(data=datsmall, aes(x=log.small, y=response), sides="b", alpha=0.8,  size=1, colour="#00818a")



# predictions for sd relief

testdata8 <- expand.grid(sd.relief=seq(min(datsmall$sd.relief),max(datsmall$sd.relief),length.out = 20),
                         log.small = mean(mod4$model$log.small),
                         log.depth = mean(mod4$model$log.depth),
                         mean.lat= (mod4$model$mean.lat),
                         year=(mod4$model$year))%>%
  distinct()%>%
  glimpse()

fits8 <- predict.gam(mod4, newdata=testdata8, type='response', se.fit=T)


predicts.sd.relief.s= testdata8%>%data.frame(fits8)%>%
  group_by(sd.relief)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
predicts.sd.relief.s

## Colour

ggmod.sd.relief.s<-  ggplot(aes(x=sd.relief, y=response), data=predicts.sd.relief.s)+
  ylab("
       ")+
  xlab('Sd relief')+
  geom_line(data=predicts.sd.relief.s,aes(x=sd.relief,y=response),colour="#00818a",alpha=0.8,size=1,show.legend=TRUE)+
  geom_ribbon(aes(ymin=response-se.fit, ymax=response + se.fit), alpha=0.4, fill="#00818a", linetype='blank')+
  theme_classic()+
  ylim(0,20)+
  Theme2 
#annotate("text", x = -Inf, y=Inf, label = "(f)",vjust = 1, hjust = -.2,size=6)

ggmod.sd.relief.s + guides(fill=FALSE) + theme(legend.position = "none")
+ geom_rug(data=datsmall, aes(x=log.small, y=response), sides="b", alpha=0.8,  size=1, colour="#00818a") 


e <- ggmod.sd.relief.s + guides(fill=FALSE) + theme(legend.position = "none") + geom_rug(data=datsmall, aes(x=sd.relief, y=response), sides="b", alpha=0.8,  size=1, colour="#00818a") 

e

########################################################################################
# Putting plots together
########################################################################################

toa_plots <- a+b+c+ #large
  d+e+plot_spacer()+ #small
  plot_layout(nrow=2) + plot_annotation(tag_levels="A") & 
  theme(plot.tag = element_text(size = 15))

toa_plots               



ggsave("TOA_preds_R1.tiff", plot=toa_plots, height=10, width=15, device="tiff", dpi=300)