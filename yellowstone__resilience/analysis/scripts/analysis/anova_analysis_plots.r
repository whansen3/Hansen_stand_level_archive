###iLand stand level analysis of resilience mechanisms#####
###Hansen 2017
##regeneration analysis script 2/2
# This script is used to plot data associated with the Anova analysis script.

setwd("H:/model working folder/stand level/the model/yellowstone__resilience/analysis")
filelist=list.files(pattern = ".*.txt")
data <- lapply(filelist, function(x)read.table(x,header=T))
summary(data)
r = do.call("rbind", data) 
summary(r)
str(r)
r$period=factor(r$period, levels = c("hist","2030","2070"))
r$elevation=factor(r$elevation, levels = c("low","med","high"))
r$distance=factor(r$distance, levels = c("short","mid","long"))
r$rcp=factor(r$rcp, levels = c("0","4.5","8"))
r$age_burned=factor(r$model.stand.age.x.post, levels = c("10","20","50","100"))
r$content.short=(((((r$content.short*1000)/950)/1000)*100))
r$content.long=(((((r$content.long*1000)/950)/1000)*100)) # to turn mm of soil water content into percent.
########
#Beginning analysis
########
library(ggplot2)
library(plyr)
r$loss=ifelse(r$density.post<=50,1,0)

r.agg1<- ddply(r, c("distance", "species","elevation","rcp","period","model.soil.y","age_burned"), summarise,
               loss    = sum(loss),
			   density =mean(density.post),
               temp.short = mean(temp.short),
			   temp.long=mean(temp.long),
			   prec.short = mean(prec.short),
			   prec.long=mean(prec.long),
			   potential.short = mean(potential.short),
			   potential.long=mean(potential.long),
			   content.short = mean(content.short),
			   content.long=mean(content.long),
			   frost.short=mean(frost.short),
			   frost.long= mean(frost.long))
str(r.agg1)
summary(r.agg1)
r.agg1$fail.perc=r.agg1$loss/20*100

summary(r.agg1)
hist(r.agg1$fail.perc)

r.agg1=subset(r.agg1, rcp==0|rcp==8)
r.agg1df=subset(r.agg1, species=="DF")
r.agg1lp=subset(r.agg1, species=="LP")
r.agg1lps=subset(r.agg1, species=="LPS")
###
 str(r.agg1)
 library(plyr)
 
 ####
 ###Figure 1 in ms
 ###

 species= ddply(r.agg1, c("species"), summarise,
               N    = length(fail.perc),
               mean = mean(fail.perc),
               sd   = sd(fail.perc),
               se   = sd / sqrt(N))
	species
####
###
##
	
b=ggplot(species, aes(x=species, y=mean)) +
 theme_bw()+
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_discrete(name="Distance from burned edge")+
  geom_errorbar(aes(ymin=(mean)-2*se, ymax=(mean)+2*se),width=.2,size=1.5, position=position_dodge(.9))+
  labs(x="Species", y ="Regeneration failure frequency (%)")  
 b	
library(gridExtra)
grid.arrange(a,b,ncol=2)

#####
####Figure 3
####DF
distance<- ddply(r.agg1df, c("distance","age_burned"), summarise,
               N    = length(fail.perc),
               mean.f = mean(fail.perc),
			   mean.d = mean(density+1),
               sd.f   = sd(fail.perc),
			    max.d=max(density),
			   sd.d   = sd(density),
               se.f   = sd.f / sqrt(N),
			   se.d   = sd.d / sqrt(N))				   
   distance
 a=ggplot(distance, aes(x=distance, y=mean.f, fill=age_burned)) +
 theme_bw()+
   theme(legend.position="none")+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=(mean.f)-2*se.f, ymax=(mean.f)+2*se.f),width=.2,size=1.5, position=position_dodge(.9))+
  labs(title="(A) Douglas-fir",x="Distance from seed source" ,y ="Regeneration failure frequency (%)")  
 a
 

 
 #####
##
####LP-NS
########
#####
####
distance<- ddply(r.agg1lp, c("distance","age_burned"), summarise,
               N    = length(fail.perc),
               mean.f = mean(fail.perc),
			   mean.d = mean(density+1),
               sd.f   = sd(fail.perc),
			   sd.d   = sd(density),
               se.f   = sd.f / sqrt(N),
			   se.d   = sd.d / sqrt(N))				   
   distance
 b=ggplot(distance, aes(x=distance, y=mean.f, fill=age_burned)) +
 theme_bw()+
   expand_limits(y = c(0, 100))+
   theme(legend.position="none")+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=(mean.f)-2*se.f, ymax=(mean.f)+2*se.f),width=.2,size=1.5, position=position_dodge(.9))+
  labs(title="(B) Non-serotinous lodgepole pine",x="Distance from seed source" ,y ="Regeneration failure frequency (%)")  
 b
  
 #####
##
####LP-S
########
########
#####
####

distance<- ddply(r.agg1lps, c("distance", "age_burned"), summarise,
               N    = length(fail.perc),
               mean.f = mean(fail.perc),
			   mean.d = mean(density+1),
               sd.f   = sd(fail.perc),
			   sd.d   = sd(density),
               se.f   = sd.f / sqrt(N),
			   se.d   = sd.d / sqrt(N))				   
   distance

c=ggplot(distance, aes(x=distance, y=mean.f,fill=age_burned)) +
 theme_bw()+
  theme(legend.position="none")+
 expand_limits(y = c(0, 100))+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=(mean.f)-2*se.f, ymax=(mean.f)+2*se.f),width=.2,size=1.5, position=position_dodge(.9))+
  labs(title="(C) Serotinous lodgepole pine",x="Distance from seed source", y ="Regeneration failure frequency (%)")  
 c


 
###Figure 3
grid.arrange(a,b,c,nrow=1)
 
 
 
 
 ####
 #####
 #####
 #####
########
###
##
#3. Evaluating Density change. 
###
####
summary(r)
str(r)
library(plyr)
r$rcp=revalue(r$rcp, c("0"="0","4.5"="4_5","8"="8"))
r=rename(r, c("temp.short"="temp_short","temp.long"="temp_long","prec.short"="prec_short","prec.long"="prec_long","potential.short"="potential_short","potential.long"="potential_long"
,"content.short"="content_short","content.long"="content_long","frost.short"="frost_short", "frost.long"="frost_long" ))

 r1.wide= reshape(r, direction = "wide", idvar = c("replicate","distance","species","rcp","elevation","model.soil.y","model.stand.age.x.post"), timevar = c("period")) 
  r2.wide= reshape(r1.wide, direction = "wide", idvar = c("replicate","distance","species","elevation","model.soil.y","model.stand.age.x.post"), timevar = c("rcp"))
summary(r2.wide)
r2.wide=subset(r2.wide,density.post.hist.0>50)
str(r2.wide)
r2.wide$density_change.hist_0=(r2.wide$density.post.hist.0-r2.wide$density.post.hist.0)
 r2.wide$density_change.2030_4_5=(r2.wide$density.post.2030.4_5-r2.wide$density.post.hist.0)
  r2.wide$density_change.2030_8=(r2.wide$density.post.2030.8-r2.wide$density.post.hist.0)
 r2.wide$density_change.2070_4_5=(r2.wide$density.post.2070.4_5-r2.wide$density.post.hist.0)
  r2.wide$density_change.2070_8=(r2.wide$density.post.2070.8-r2.wide$density.post.hist.0)
  
  
 r2.wide2=rename(r2.wide,c("density.post.hist.0"="density_post.hist_0","temp_short.hist.0"="temp_short.hist_0","temp_short.2030.4_5"="temp_short.2030_4_5","temp_short.2030.8"="temp_short.2030_8","temp_long.hist.0"="temp_long.hist_0","temp_long.2030.4_5"="temp_long.2030_4_5","temp_long.2030.8"="temp_long.2030_8",
 "temp_short.2070.4_5"="temp_short.2070_4_5","temp_short.2070.8"="temp_short.2070_8","temp_long.2070.4_5"="temp_long.2070_4_5","temp_long.2070.8"="temp_long.2070_8",
 "prec_short.hist.0"="prec_short.hist_0","prec_short.2030.4_5"="prec_short.2030_4_5","prec_short.2030.8"="prec_short.2030_8","prec_long.hist.0"="prec_long.hist_0","prec_long.2030.4_5"="prec_long.2030_4_5","prec_long.2030.8"="prec_long.2030_8",
 "prec_short.2070.4_5"="prec_short.2070_4_5","prec_short.2070.8"="prec_short.2070_8","prec_long.2070.4_5"="prec_long.2070_4_5","prec_long.2070.8"="prec_long.2070_8",
 "potential_short.hist.0"="potential_short.hist_0","potential_short.2030.4_5"="potential_short.2030_4_5","potential_short.2030.8"="potential_short.2030_8","potential_long.2030.4_5"="potential_long.2030_4_5","potential_long.2030.8"="potential_long.2030_8",
 "potential_short.2070.4_5"="potential_short.2070_4_5","potential_short.2070.8"="potential_short.2070_8","potential_long.hist.0"="potential_long.hist_0","potential_long.2070.4_5"="potential_long.2070_4_5","potential_long.2070.8"="potential_long.2070_8",
 "content_short.hist.0"="content_short.hist_0","content_short.2030.4_5"="content_short.2030_4_5","content_short.2030.8"="content_short.2030_8","content_long.2030.4_5"="content_long.2030_4_5","content_long.2030.8"="content_long.2030_8",
 "content_short.2070.4_5"="content_short.2070_4_5","content_short.2070.8"="content_short.2070_8","content_long.hist.0"="content_long.hist_0","content_long.2070.4_5"="content_long.2070_4_5","content_long.2070.8"="content_long.2070_8",
 "frost_short.hist.0"="frost_short.hist_0","frost_short.2030.4_5"="frost_short.2030_4_5","frost_short.2030.8"="frost_short.2030_8","frost_long.hist.0"="frost_long.hist_0","frost_long.2030.4_5"="frost_long.2030_4_5","frost_long.2030.8"="frost_long.2030_8",
 "frost_short.2070.4_5"="frost_short.2070_4_5","frost_short.2070.8"="frost_short.2070_8","frost_long.2070.4_5"="frost_long.2070_4_5","frost_long.2070.8"="frost_long.2070_8"))



 r2.long=reshape(r2.wide2,direction = "long",  varying=c("density_change.hist_0","density_change.2030_4_5","density_change.2030_8","density_change.2070_4_5","density_change.2070_8","temp_short.hist_0","temp_short.2030_4_5","temp_short.2030_8",
 "temp_short.2070_4_5","temp_short.2070_8","temp_long.hist_0","temp_long.2030_4_5","temp_long.2030_8","temp_long.2070_4_5","temp_long.2070_8","prec_short.2030_4_5","prec_short.hist_0",
 "prec_short.2030_8","prec_short.2070_4_5","prec_short.2070_8","prec_long.hist_0","prec_long.2030_4_5","prec_long.2030_8","prec_long.2070_4_5","prec_long.2070_8","potential_short.hist_0","potential_short.2030_4_5","potential_short.2030_8",
 "potential_short.2070_4_5","potential_short.2070_8","potential_long.hist_0","potential_long.2030_4_5","potential_long.2030_8","potential_long.2070_4_5","potential_long.2070_8","content_short.hist_0","content_short.2030_4_5","content_short.2030_8",
 "content_short.2070_4_5","content_short.2070_8","content_long.hist_0","content_long.2030_4_5","content_long.2030_8","content_long.2070_4_5","content_long.2070_8","frost_short.hist_0","frost_short.2030_4_5","frost_short.2030_8",
 "frost_short.2070_4_5","frost_short.2070_8","frost_long.hist_0","frost_long.2030_4_5","frost_long.2030_8","frost_long.2070_4_5","frost_long.2070_8"),
 idvar = c("replicate","distance","species","elevation","model.soil.y","model.stand.age.x.post"),sep=".")
  summary(r2.long)

  r2.long$time=as.factor(r2.long$time) 
r2.long$time=revalue(r2.long$time, c("2030_4_5"="2030_4.5","2070_4_5"="2070_4.5"))
library(tidyr)
 r3.long= separate(r2.long,time, c('period','rcp'), sep = '_')  
 summary(r3.long) 


 
 
 
 r.agg3dens=ddply(r3.long, c("distance", "species","elevation","rcp","period","model.soil.y","model.stand.age.x.post"), summarise,
               density_change    = mean(density_change),
			   density_historic =mean(density_post.hist_0),
			   density_2030=mean(density.post.2030.8),
			  density_2070=mean(density.post.2070.8),
               temp_short = mean(temp_short),
			   temp_long=mean(temp_long),
			   prec_short = mean(prec_short),
			   prec_long=mean(prec_long),
			   potential_short = mean(potential_short),
			   potential_long=mean(potential_long),
			   content_short = mean(content_short),
			   content_long=mean(content_long),
			   frost_short=mean(frost_short),
			   frost_long=mean(frost_long))
summary(r.agg3dens)

period=ddply(r.agg3dens, c( "period","species"), summarise,
              N    = length(density_historic),
               mean = mean(density_historic),
               sd   = sd(density_historic),
               se   = sd / sqrt(N),
			   
			   N    = length(density_2030),
               mean30 = mean(density_2030),
               sd30   = sd(density_2030),
               se30   = sd30 / sqrt(N),
			   
			   N    = length(density_2070),
               mean70 = mean(density_2070),
               sd70   = sd(density_2070),
               se70   = sd70 / sqrt(N))
period

 
 distance=ddply(r.agg3dens, c("distance","species"), summarise,
              N    = length(density_change),
               mean = mean(density_change),
               sd   = sd(density_change),
               se   = sd / sqrt(N))
distance

 substrate=ddply(r.agg3dens, c("model.soil.y","species"), summarise,
              N    = length(density_change),
               mean = mean(density_change),
               sd   = sd(density_change),
               se   = sd / sqrt(N))
substrate

 elevation=ddply(r.agg3dens, c("elevation","species"), summarise,
              N    = length(density_change),
               mean = mean(density_change),
               sd   = sd(density_change),
               se   = sd / sqrt(N))
elevation

 fri=ddply(r.agg3dens, c("model.stand.age.x.post","species"), summarise,
              N    = length(density_change),
               mean = mean(density_change),
               sd   = sd(density_change),
               se   = sd / sqrt(N))
fri
r.agg3dens$period=as.factor(r.agg3dens$period)
str(r.agg3dens)
r.agg3dens$model.stand.age.x.post=as.factor(r.agg3dens$model.stand.age.x.post)
r.agg3dens=subset(r.agg3dens,rcp==8)### Change here to reset to only future or both historical and future if you want to do climate comparisons.
r.agg3densDF=subset(r.agg3dens, species=="DF")
summary(r.agg3densDF)
r.agg3densLP=subset(r.agg3dens, species=="LP")
r.agg3densLPS=subset(r.agg3dens, species=="LPS")
####

library(plot3D)
args(slice3D)
###
#Figure 4
####
data.loess <- loess(density_change ~ prec_long * temp_long, data = r.agg3densDF)
xgrid <-  seq(min(r.agg3densDF$prec_long), max(r.agg3densDF$prec_long),.1)
ygrid <-  seq(min(r.agg3densDF$temp_long), max(r.agg3densDF$temp_long), .1)
data.fit <-  expand.grid(prec_long = xgrid, temp_long = ygrid)
mtrx3d <-  predict(data.loess, newdata = data.fit)
mtrx3d
persp3D(xgrid,ygrid,mtrx3d,ylim=c(12,18),xlim=c(350,1450), col= ramp.col(c("grey20", "white")),
theta=-120, phi=15, axes=TRUE,scale=2, nticks=5, label="TRUE",
ticktype="detailed",xlab="Annual precipitation (mm)", ylab="Temperature (deg C)", zlab="Density change (#)", bty = "bl2",
main="DF")


	