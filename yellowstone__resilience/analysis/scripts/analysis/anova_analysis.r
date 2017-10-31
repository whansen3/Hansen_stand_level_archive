###iLand stand level analysis of resilience mechanisms#####
###Hansen 2017
##regeneration analysis script
#analysis script 1/2
rm(list = ls())# clears memory
setwd("H:/model working folder/stand level/the model/yellowstone__resilience/analysis")
#setwd("C:/work/stand level/the model/yellowstone__resilience/analysis")

filelist=list.files(pattern = ".*.txt")
data <- lapply(filelist, function(x)read.table(x,header=T))
summary(data)
r = do.call("rbind", data) 
summary(r)
str(r)
r$period=factor(r$period, levels = c("hist","2030","2070"))
r$elevation=factor(r$elevation, levels = c("low","med","high"))
r$rcp=factor(r$rcp, levels = c("0","4.5","8"))
r$age_burned=factor(r$model.stand.age.x.post, levels = c("10","20","50","100"))
r$content.short=(((((r$content.short*1000)/950)/1000)*100))
r$content.long=(((((r$content.long*1000)/950)/1000)*100)) # to turn mm of soil water content into percent.
########
#Beginning analysis
########
library(plyr)
r$loss=ifelse(r$density.post<=50,1,0)

r.agg1<- ddply(r, c("distance", "species","elevation","rcp","period","model.soil.y","age_burned"), summarise,
               loss    = sum(loss),
			   density  = mean(density.post),
               temp.short = mean(temp.short),
			   temp.long=mean(temp.long),
			   prec.short = mean(prec.short),
			   prec.long=mean(prec.long),
			   potential.short = mean(potential.short),
			   potential.long=mean(potential.long),
			   content.short = mean(content.short),
			   content.long=mean(content.long),
			   frost.short=mean(frost.short),
			   frost.long=mean(frost.long))
str(r.agg1)
summary(r.agg1)
r.agg1$fail.perc=r.agg1$loss/20*100

summary(r.agg1)
hist(r.agg1$fail.perc)
r.agg1=subset(r.agg1, rcp==0|rcp==8)
r.agg1df=subset(r.agg1, species=="DF")
r.agg1lp=subset(r.agg1, species=="LP")
r.agg1lps=subset(r.agg1, species=="LPS")
str(r.agg1)

###
##1.species densities
######
species1= ddply(r.agg1, c("species","period"), summarise,
                N    = length(density),
                min =min(density),
                max =max(density),
                mean = mean(density),
                median= median(density),
                sd   = sd(density),
                se   = sd / sqrt(N))

###
##2. exhaustive model selection to look at the frequency of regeneration failure. 
### 
##DF-treatment-level effects

library(lme4)
library(car)
library(multcomp)
library(pscl)
library(MASS)
library(MuMIn)
library(rsq)
library(lsmeans)
r.agg1df=subset(r.agg1,species=="DF")
###
#frequency regen failure.
#####
species1= ddply(r.agg1df, c("period"), summarise,
                N    = length(fail.perc),
                min =min(fail.perc),
                max =max(fail.perc),
                mean = mean(fail.perc),
                median= median(fail.perc),
                sd   = sd(fail.perc),
                se   = sd / sqrt(N))
species1
str(r.agg1df) 
summary(r.agg1df)
r.agg1df$fail.log=logit(r.agg1df$fail.perc,adjust=0.01,percents=TRUE)
summary(r.agg1df)
options(na.action = "na.fail") 
freq.df<-lm(fail.log~distance+elevation+period+model.soil.y+age_burned+distance:elevation+distance:period+distance:model.soil.y+distance:age_burned+
elevation:period+elevation:model.soil.y+elevation:age_burned+period:model.soil.y+period:age_burned+model.soil.y:age_burned,data=r.agg1df)
vif(freq.df)
#plot(freq.df)
summary(freq.df)
logLik(freq.df)
freq.dfd <-dredge(freq.df,m.lim = c(NA, 4))
freq.dfd
get.models(freq.dfd, subset = delta<2)
options(contrasts = c("contr.sum", "contr.poly"))# This command needs to be run in order to get the correct results from the test 

freq.df1=lm(fail.log~distance+period+distance:period,data=r.agg1df)
summary(freq.df1)
#plot(freq.df1)
Anova(freq.df1,type="III")


freq.df2<-lm(fail.log~distance+elevation+period+distance*period,data=r.agg1df)
summary(freq.df2)
#plot(freq.df2)
Anova(freq.df2,type="III")

freq.df3<-lm(fail.log~distance+model.soil.y+period+distance*period,data=r.agg1df)
summary(freq.df3)
#plot(freq.df3)
Anova(freq.df3,type="III")

##Tukey distance
leastsquare = lsmeans(freq.df1, 
                      "distance",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey climate period
leastsquare = lsmeans(freq.df1, 
                      "period",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey interaction
lsm = lsmeans(freq.df1, 
              pairwise~distance:period,        #replace temp:species with the interaction you want to test.
              adjust="tukey")
cld(lsm, 
    alpha=.05,
    Letters=letters)

###LPNS 


r.agg1lp=subset(r.agg1,species=="LP")
species1= ddply(r.agg1lp, c("elevation"), summarise,
                N    = length(fail.perc),
                min =min(fail.perc),
                max =max(fail.perc),
                mean = mean(fail.perc),
                median= median(fail.perc),
                sd   = sd(fail.perc),
                se   = sd / sqrt(N))
species1
str(r.agg1lp)
r.agg1lp$fail.log=logit(r.agg1lp$fail.perc,adjust=0.01,percents=TRUE)
summary(r.agg1lp)
options(na.action = "na.fail") 
freq.lp<-lm(fail.log~distance+elevation+period+model.soil.y+age_burned+distance:elevation+distance:period+distance:model.soil.y+distance:age_burned+
elevation:period+elevation:model.soil.y+elevation:age_burned+period:model.soil.y+period:age_burned+model.soil.y:age_burned,data=r.agg1lp)
vif(freq.lp)
plot(freq.lp)

summary(freq.lp)
logLik(freq.lp)
freq.lpd <-dredge(freq.lp,m.lim = c(NA, 4))
freq.lpd
get.models(freq.lpd, subset = delta<2)

freq.lp1<-lm(fail.log~distance+elevation+model.soil.y+distance:elevation,data=r.agg1lp)
summary(freq.lp1)
Anova(freq.lp1,type="III")

##Tukey distance
leastsquare = lsmeans(freq.lp1, 
                      "distance",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey elevation
leastsquare = lsmeans(freq.lp1, 
                      "elevation",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey interaction
lsm = lsmeans(freq.lp1, 
              pairwise~distance:elevation,        #replace temp:species with the interaction you want to test.
              adjust="tukey")
cld(lsm, 
    alpha=.05,
    Letters=letters)




###LPS
r.agg1lps=subset(r.agg1,species=="LPS")
species1= ddply(r.agg1lps, c("elevation"), summarise,
                N    = length(fail.perc),
                min =min(fail.perc),
                max =max(fail.perc),
                mean = mean(fail.perc),
                median= median(fail.perc),
                sd   = sd(fail.perc),
                se   = sd / sqrt(N))
species1

plot(r.agg1lps$fail.perc~r.agg1lps$distance) 
r.agg1lps$fail.log=logit(r.agg1lps$fail.perc,adjust=0.01,percents=TRUE)
summary(r.agg1lps)
options(na.action = "na.fail") 
freq.lps<-lm(fail.log~distance+elevation+period+model.soil.y+age_burned+distance:elevation+distance:period+distance:model.soil.y+distance:age_burned+
elevation:period+elevation:model.soil.y+elevation:age_burned+period:model.soil.y+period:age_burned+model.soil.y:age_burned,data=r.agg1lps)
vif(freq.lps)
#plot(freq.lps)

summary(freq.lps)
logLik(freq.lps)
freq.lpsd <-dredge(freq.lps,m.lim = c(NA, 4))
freq.lpsd
get.models(freq.lpsd, subset = delta<2)

freq.lps1<-lm(fail.log~age_burned+distance+elevation+distance:age_burned,data=r.agg1lps)
vif(freq.lps1)
#plot(freq.lps1)
summary(freq.lps1)
Anova(freq.lps1,type="III")

##Tukey distance
leastsquare = lsmeans(freq.lps1, 
                      "distance",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey age burned
leastsquare = lsmeans(freq.lps1, 
                      "age_burned",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey interaction
lsm = lsmeans(freq.lps1, 
              pairwise~distance:age_burned,        #replace temp:species with the interaction you want to test.
              adjust="tukey")
cld(lsm, 
    alpha=.05,
    Letters=letters)







###
###4. Exhausive model selection with linear mixed effects models to look at the frequency of regeneration failure
# as a function of climate specific variables. non-climate related variables are included as random variables.
###DF

library(lme4) 
# Testing random effects
cor(r.agg1df$frost.short,r.agg1df$temp.long)
m<-lmer(scale(fail.log)~scale(prec.long)*scale(temp.long)*scale(frost.short)+(1|distance)+(1|age_burned), REML=FALSE, data=r.agg1df)
m.2<-lmer(scale(fail.log)~scale(prec.long)*scale(temp.long)*scale(frost.short)+(1|age_burned), REML=FALSE, data=r.agg1df)
m.3<-lmer(scale(fail.log)~scale(prec.long)*scale(temp.long)*scale(frost.short)+(1|distance), REML=FALSE, data=r.agg1df)
anova(m.2,m) ####  of random effect distance  
anova(m.3,m) ####  of random effect age_burned drop age_burned. 
m<-lmer(scale(fail.log)~scale(prec.long)*scale(temp.long)*scale(frost.short)+(1|distance), REML=FALSE, data=r.agg1df)
summary(m)
freq.dfd <-dredge(m,m.lim = c(NA, 4))
freq.dfd
get.models(freq.dfd, subset = delta<2)
summary(model.avg(freq.dfd, subset=delta<2, revised.var = TRUE)) 




###LPNS 
str(r.agg1lp)

summary(r.agg1lp)
m<-lmer(scale(fail.log)~scale(potential.short)*scale(prec.long)*scale(frost.short)+(1|distance)+(1|age_burned), REML=FALSE, data=r.agg1lp)
m.2<-lmer(scale(fail.log)~scale(potential.short)*scale(prec.long)*scale(frost.short)+(1|age_burned), REML=FALSE, data=r.agg1lp)
m.3<-lmer(scale(fail.log)~scale(potential.short)*scale(prec.long)*scale(frost.short)+(1|distance), REML=FALSE, data=r.agg1lp)
anova(m.2,m) ####  of random effect distance  
anova(m.3,m) ####  of random effect age_burned drop age_burned. 
m<-lmer(scale(fail.log)~scale(potential.short)*scale(prec.long)*scale(frost.short)+(1|distance), REML=FALSE, data=r.agg1lp)
summary(m)
freq.lpd <-dredge(m,m.lim = c(NA, 4))
freq.lpd
get.models(freq.lpd, subset = delta<2)
summary(model.avg(freq.lpd, revised.var = TRUE, subset=delta<2)) 





##LPS 
str(r.agg1lps)

summary(r.agg1lps)
m<-lmer(scale(fail.log)~scale(potential.short)*scale(prec.long)*scale(frost.short)+(1|distance)+(1|age_burned), REML=FALSE, data=r.agg1lps)
m.2<-lmer(scale(fail.log)~scale(potential.short)*scale(prec.long)*scale(frost.short)+(1|age_burned), REML=FALSE, data=r.agg1lps)
m.3<-lmer(scale(fail.log)~scale(potential.short)*scale(prec.long)*scale(frost.short)+(1|distance), REML=FALSE, data=r.agg1lps)
anova(m.2,m) ####  of random effect distance  
anova(m.3,m) ####  of random effect age_burned keep age_burned. 
m<-lmer(scale(fail.log)~scale(potential.short)*scale(prec.long)*scale(frost.short)+(1|distance)+(1|age_burned), REML=FALSE, data=r.agg1lps)
summary(m)
freq.lpsd <-dredge(m,m.lim = c(NA, 4))
freq.lpsd
get.models(freq.lpsd, subset = delta<2)
summary(model.avg(freq.lpsd, revised.var = TRUE, subset=delta<2)) 



###
##
#3. Evaluating Density change. 
###
####
summary(r)
str(r)
library(plyr)
r$w.height.post[is.na(r$w.height.post)]=0
r$rcp=revalue(r$rcp, c("0"="0","4.5"="4_5","8"="8"))
r=rename(r, c("temp.short"="temp_short","temp.long"="temp_long","prec.short"="prec_short","prec.long"="prec_long","potential.short"="potential_short","potential.long"="potential_long"
,"content.short"="content_short","content.long"="content_long","frost.short"="frost_short", "frost.long"="frost_long" ))

 r1.wide= reshape(r, direction = "wide", idvar = c("replicate","distance","species","rcp","elevation","model.soil.y","model.stand.age.x.post"), timevar = c("period")) 
  r2.wide= reshape(r1.wide, direction = "wide", idvar = c("replicate","distance","species","elevation","model.soil.y","model.stand.age.x.post"), timevar = c("rcp"))
summary(r2.wide)
r2.wide=subset(r2.wide,density.post.hist.0>50)
summary(r2.wide)
str(r2.wide)
 r2.wide$density_change.2030_4_5=(r2.wide$density.post.2030.4_5-r2.wide$density.post.hist.0)
  r2.wide$density_change.2030_8=(r2.wide$density.post.2030.8-r2.wide$density.post.hist.0)
 r2.wide$density_change.2070_4_5=(r2.wide$density.post.2070.4_5-r2.wide$density.post.hist.0)
  r2.wide$density_change.2070_8=(r2.wide$density.post.2070.8-r2.wide$density.post.hist.0)

  r2.wide$height_change.2030_4_5=(r2.wide$w.height.post.2030.4_5-r2.wide$w.height.post.hist.0)
  r2.wide$height_change.2030_8=(r2.wide$w.height.post.2030.8-r2.wide$w.height.post.hist.0)
 r2.wide$height_change.2070_4_5=(r2.wide$w.height.post.2070.4_5-r2.wide$w.height.post.hist.0)
  r2.wide$height_change.2070_8=(r2.wide$w.height.post.2070.8-r2.wide$w.height.post.hist.0)  
  
 r2.wide2=rename(r2.wide,c("temp_short.2030.4_5"="temp_short.2030_4_5","temp_short.2030.8"="temp_short.2030_8","temp_long.2030.4_5"="temp_long.2030_4_5","temp_long.2030.8"="temp_long.2030_8",
 "temp_short.2070.4_5"="temp_short.2070_4_5","temp_short.2070.8"="temp_short.2070_8","temp_long.2070.4_5"="temp_long.2070_4_5","temp_long.2070.8"="temp_long.2070_8",
 "prec_short.2030.4_5"="prec_short.2030_4_5","prec_short.2030.8"="prec_short.2030_8","prec_long.2030.4_5"="prec_long.2030_4_5","prec_long.2030.8"="prec_long.2030_8",
 "prec_short.2070.4_5"="prec_short.2070_4_5","prec_short.2070.8"="prec_short.2070_8","prec_long.2070.4_5"="prec_long.2070_4_5","prec_long.2070.8"="prec_long.2070_8",
 "potential_short.2030.4_5"="potential_short.2030_4_5","potential_short.2030.8"="potential_short.2030_8","potential_long.2030.4_5"="potential_long.2030_4_5","potential_long.2030.8"="potential_long.2030_8",
 "potential_short.2070.4_5"="potential_short.2070_4_5","potential_short.2070.8"="potential_short.2070_8","potential_long.2070.4_5"="potential_long.2070_4_5","potential_long.2070.8"="potential_long.2070_8",
 "content_short.2030.4_5"="content_short.2030_4_5","content_short.2030.8"="content_short.2030_8","content_long.2030.4_5"="content_long.2030_4_5","content_long.2030.8"="content_long.2030_8",
 "content_short.2070.4_5"="content_short.2070_4_5","content_short.2070.8"="content_short.2070_8","content_long.2070.4_5"="content_long.2070_4_5","content_long.2070.8"="content_long.2070_8",
 "frost_short.2030.4_5"="frost_short.2030_4_5","frost_short.2030.8"="frost_short.2030_8","frost_long.2030.4_5"="frost_long.2030_4_5","frost_long.2030.8"="frost_long.2030_8",
 "frost_short.2070.4_5"="frost_short.2070_4_5","frost_short.2070.8"="frost_short.2070_8","frost_long.2070.4_5"="frost_long.2070_4_5","frost_long.2070.8"="frost_long.2070_8"))



 r2.long=reshape(r2.wide2,direction = "long",  varying=c("density_change.2030_4_5","density_change.2030_8","density_change.2070_4_5","density_change.2070_8",
 "height_change.2030_4_5","height_change.2030_8","height_change.2070_4_5","height_change.2070_8","temp_short.2030_4_5","temp_short.2030_8",
 "temp_short.2070_4_5","temp_short.2070_8","temp_long.2030_4_5","temp_long.2030_8","temp_long.2070_4_5","temp_long.2070_8","prec_short.2030_4_5",
 "prec_short.2030_8","prec_short.2070_4_5","prec_short.2070_8","prec_long.2030_4_5","prec_long.2030_8","prec_long.2070_4_5","prec_long.2070_8","potential_short.2030_4_5","potential_short.2030_8",
 "potential_short.2070_4_5","potential_short.2070_8","potential_long.2030_4_5","potential_long.2030_8","potential_long.2070_4_5","potential_long.2070_8","content_short.2030_4_5","content_short.2030_8",
 "content_short.2070_4_5","content_short.2070_8","content_long.2030_4_5","content_long.2030_8","content_long.2070_4_5","content_long.2070_8","frost_short.2030_4_5","frost_short.2030_8",
 "frost_short.2070_4_5","frost_short.2070_8","frost_long.2030_4_5","frost_long.2030_8","frost_long.2070_4_5","frost_long.2070_8"),
 idvar = c("replicate","distance","species","elevation","model.soil.y","model.stand.age.x.post"),sep=".")
  summary(r2.long)

  r2.long$time=as.factor(r2.long$time) 
r2.long$time=revalue(r2.long$time, c("2030_4_5"="2030_4.5","2070_4_5"="2070_4.5"))
library(tidyr)
 r3.long= separate(r2.long,time, c('period','rcp'), sep = '_')  
 summary(r3.long) 

 r.agg3dens=ddply(r3.long, c("distance", "species","elevation","rcp","period","model.soil.y","model.stand.age.x.post"), summarise,
               density_change    = mean(density_change),
			   height_change  =mean(height_change),
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
str(r.agg3dens)
r.agg3dens$model.stand.age.x.post=as.factor(r.agg3dens$model.stand.age.x.post)
r.agg3dens=subset(r.agg3dens,rcp==8)
###
###Exhaustive model selection to look at the density differences versus treatment-level variables.
##Variables included are climate variables, distance to seed source, FRI
### So DF was giving me nothing with these key variables. 
###
###DF density_change
######

library(car)
library(MuMIn)
options(na.action = "na.fail") 
r.agg3densdf=subset(r.agg3dens,species=="DF")
r.agg3densdf$distance=factor(r.agg3densdf$distance)
str(r.agg3densdf)
summary(r.agg3densdf)

hist(r.agg3densdf$density_change)
###Means
species1= ddply(r.agg3densdf, c("distance","period"), summarise,
                N    = length(density_change),
                min =min(density_change),
                max =max(density_change),
                mean = mean(density_change),
                median= median(density_change),
                sd   = sd(density_change),
                se   = sd / sqrt(N))
species1
summary(r.agg3densdf)
options(na.action = "na.fail") 
freq.df<-lm(density_change~distance+elevation+period+model.soil.y+model.stand.age.x.post+distance:elevation+distance:period+distance:model.soil.y+distance:model.stand.age.x.post+
elevation:period+elevation:model.soil.y+elevation:model.stand.age.x.post+period:model.soil.y+period:model.stand.age.x.post+model.soil.y:model.stand.age.x.post,data=r.agg3densdf)###Need to update the interactions here when I get the frost day figured out.  

vif(freq.df)
#plot(freq.df)


freq.dfd <-dredge(freq.df,m.lim = c(NA, 4))
freq.dfd
get.models(freq.dfd, subset = delta<2)

freq.df1<-lm(density_change~distance+model.soil.y+period+distance:period,data=r.agg3densdf)
summary(freq.df1)
Anova(freq.df1,type="III")
##Tukey distance
leastsquare = lsmeans(freq.df1, 
                      "distance",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey age burned
leastsquare = lsmeans(freq.df1, 
                      "period",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey interaction
lsm = lsmeans(freq.df1, 
              pairwise~distance:period,        #replace temp:species with the interaction you want to test.
              adjust="tukey")
cld(lsm, 
    alpha=.05,
    Letters=letters)



######
###LP-NS-density change
#######
options(na.action = "na.fail") 
r.agg3denslp=subset(r.agg3dens,species=="LP")
species1= ddply(r.agg3denslp, c("period"), summarise,
                N    = length(density_change),
                min =min(density_change),
                max =max(density_change),
                mean = mean(density_change),
                median= median(density_change),
                sd   = sd(density_change),
                se   = sd / sqrt(N))
species1
options(na.action = "na.fail") 
freq.lp<-lm(density_change~distance+elevation+period+model.soil.y+model.stand.age.x.post+distance:elevation+distance:period+distance:model.soil.y+distance:model.stand.age.x.post+
elevation:period+elevation:model.soil.y+elevation:model.stand.age.x.post+period:model.soil.y+period:model.stand.age.x.post+model.soil.y:model.stand.age.x.post,data=r.agg3denslp)###Need to update the interactions here when I get the frost day figured out.  
library(car)
vif(freq.lp)
#plot(freq.lp)

freq.dlp <-dredge(freq.lp,m.lim = c(NA, 4))
freq.dlp
get.models(freq.dlp, subset = delta<2)

freq.lp1<-lm(density_change~distance+elevation+period+distance:elevation,data=r.agg3denslp)
summary(freq.lp1)
Anova(freq.lp1,type="III")




######
###LP-S-density change
#######
options(na.action = "na.fail") 
r.agg3densLPS=subset(r.agg3dens,species=="LPS")
species1= ddply(r.agg3densLPS, c("period"), summarise,
                N    = length(density_change),
                min =min(density_change),
                max =max(density_change),
                mean = mean(density_change),
                median= median(density_change),
                sd   = sd(density_change),
                se   = sd / sqrt(N))
species1
Math.cbrt <- function(x) {
    sign(x) * abs(x)^(1/9)
}
r.agg3densLPS$l.density_change=Math.cbrt(r.agg3densLPS$density_change)
options(na.action = "na.fail") 
freq.LPS<-lm(l.density_change~distance+elevation+period+model.soil.y+model.stand.age.x.post+distance:elevation+distance:period+distance:model.soil.y+distance:model.stand.age.x.post+
elevation:period+elevation:model.soil.y+elevation:model.stand.age.x.post+period:model.soil.y+period:model.stand.age.x.post+model.soil.y:model.stand.age.x.post,data=r.agg3densLPS)###Need to update the interactions here when I get the frost day figured out.  
vif(freq.LPS)
#plot(freq.LPS)

freq.dLPS <-dredge(freq.LPS,m.lim = c(NA, 4))
freq.dLPS
get.models(freq.dLPS, subset = delta<2)

freq.LPS1<-lm(l.density_change~distance+elevation+model.stand.age.x.post+elevation:model.stand.age.x.post,data=r.agg3densLPS)
summary(freq.LPS1)
Anova(freq.LPS1,type="III")

##Tukey elevation
leastsquare = lsmeans(freq.LPS1, 
                      "elevation",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
##Tukey distance
leastsquare = lsmeans(freq.LPS1, 
                      "distance",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey age burned
leastsquare = lsmeans(freq.LPS1, 
                      "model.stand.age.x.post",
                      adjust="tukey")

cld(leastsquare, 
    alpha=.05,
    Letters=letters)
###Tukey interaction
lsm = lsmeans(freq.LPS1, 
              pairwise~distance:period,        #replace temp:species with the interaction you want to test.
              adjust="tukey")
cld(lsm, 
    alpha=.05,
    Letters=letters)







###
###Exhaustive model selection to look at the density difference as a function of climate using mixed effects models to control for non climate related treatment effects.
###
###DF -density change
######
library(lme4)
m<-lmer(scale(density_change)~scale(prec_long)*scale(temp_long)*scale(frost_short)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3densdf)
m.2<-lmer(scale(density_change)~scale(prec_long)*scale(temp_long)*scale(frost_short)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3densdf)
m.3<-lmer(scale(density_change)~scale(prec_long)*scale(temp_long)*scale(frost_short)+(1|distance), REML=FALSE, data=r.agg3densdf)
anova(m.2,m) ### This tests the distance random effect
anova(m.3,m) ### This tests significance of fri random effect.
m<-lmer(scale(density_change)~scale(prec_long)*scale(temp_long)*scale(frost_short)+(1|distance), REML=FALSE, data=r.agg3densdf)
summary(m)
freq.dfd <-dredge(m,m.lim = c(NA, 4))
freq.dfd
get.models(freq.dfd, subset = delta<2)
summary(model.avg(freq.dfd, revised.var = TRUE, subset=delta<2)) 

m1.full<-lmer(scale(density_change)~scale(prec_long)+scale(temp_long)+scale(prec_long):scale(temp_long)+(1|distance), REML=FALSE, data=r.agg3densdf)
m1.red1<-lmer(scale(density_change)~scale(prec_long)+scale(prec_long):scale(temp_long)+(1|distance), REML=FALSE, data=r.agg3densdf)
m1.red2<-lmer(scale(density_change)~scale(temp_long)+scale(prec_long):scale(temp_long)+(1|distance), REML=FALSE, data=r.agg3densdf)
m1.red3<-lmer(scale(density_change)~scale(prec_long)+scale(temp_long)+(1|distance), REML=FALSE, data=r.agg3densdf)
summary(m1.full)
anova(m1.red1,m1.full) ### This tests significance of temp
anova(m1.red2,m1.full) #### This tests significance of precip 
anova(m1.red3,m1.full) #### This tests significance of interaction term


######
###LP-NS-density change
#######
m<-lmer(scale(density_change)~scale(potential_short)*scale(prec_long)*scale(frost_short)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3denslp)
m.2<-lmer(scale(density_change)~scale(potential_short)*scale(prec_long)*scale(frost_short)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3denslp)
m.3<-lmer(scale(density_change)~scale(potential_short)*scale(prec_long)*scale(frost_short)+(1|distance), REML=FALSE, data=r.agg3denslp)
anova(m.2,m) ### This tests the distance random effect
anova(m.3,m) ### This tests significance of fri random effect
m<-lmer(scale(density_change)~scale(potential_short)*scale(prec_long)*scale(frost_short)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3denslp)
summary(m)
freq.dfd <-dredge(m,m.lim = c(NA, 4))
freq.dfd
get.models(freq.dfd, subset = delta<2)
summary(model.avg(freq.dfd,subset= delta<2, revised.var = TRUE)) 
m1.full<-lmer(scale(density_change)~scale(potential_short)+scale(prec_long)+scale(frost_short)+scale(frost_short):scale(prec_long)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3denslp)
m1.red1<-lmer(scale(density_change)~scale(prec_long)+scale(frost_short)+scale(frost_short):scale(prec_long)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3denslp)
m1.red2<-lmer(scale(density_change)~scale(potential_short)+scale(frost_short)+scale(frost_short):scale(prec_long)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3denslp)
m1.red3<-lmer(scale(density_change)~scale(potential_short)+scale(prec_long)+scale(frost_short):scale(prec_long)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3denslp)
m1.red4<-lmer(scale(density_change)~scale(potential_short)+scale(prec_long)+scale(frost_short)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3denslp)

summary(m1.full)
anova(m1.red1,m1.full) ### This tests significance of water potential
anova(m1.red2,m1.full) #### This tests significance of precip 
anova(m1.red3,m1.full) #### This tests significance of frost
anova(m1.red4,m1.full) #### This tests significance of the interaction term



#####
###LP-S-density change
#######

Math.cbrt <- function(x) {
    sign(x) * abs(x)^(1/9)
}
r.agg3densLPS$l.density_change=Math.cbrt(r.agg3densLPS$density_change)
m<-lmer(scale(l.density_change)~scale(potential_short)*scale(prec_long)*scale(frost_short)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3densLPS)
m.2<-lmer(scale(l.density_change)~scale(potential_short)*scale(prec_long)*scale(frost_short)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3densLPS)
m.3<-lmer(scale(l.density_change)~scale(potential_short)*scale(prec_long)*scale(frost_short)+(1|distance), REML=FALSE, data=r.agg3densLPS)
anova(m,m.2) ### This tests the distance interaction
anova(m.3,m) ### This tests significance of fri interaction

m<-lmer(scale(l.density_change)~scale(potential_short)*scale(prec_long)*scale(frost_short)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3densLPS)
summary(m)
freq.dfd <-dredge(m,m.lim = c(NA, 4))
freq.dfd
get.models(freq.dfd, subset = delta<2)
summary(model.avg(freq.dfd, delta<2, revised.var = TRUE)) 

m1.full=lmer(scale(l.density_change)~scale(prec_long)+scale(temp_long)+scale(frost_short)+scale(frost_short):scale(temp_long)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3densLPS)
summary(m1.full)

m2.full=lmer(scale(l.density_change)~scale(temp_long)+scale(frost_short)+scale(frost_short):scale(temp_long)+(1|distance)+(1|model.stand.age.x.post), REML=FALSE, data=r.agg3densLPS)
summary(m2.full)


