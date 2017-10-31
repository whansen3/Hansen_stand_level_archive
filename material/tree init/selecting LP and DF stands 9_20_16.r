#####
#
## Finding densities
#NOTES:
#So this approach was fairly helpful for densities, but I don't think its particularly useful to take the actual tree characteristics for plots of corresponding densities because of the issue with not DF ages around 50 or 100 years.
###So the alternative approach is to choose our stands and then grow the trees to 100 years and harvest that data for tree sizes.  
#####
####
### read in data

stands=read.csv("YNP 2012 Extensive Data Summary_2.csv",header=T,sep=";",dec=".",na.strings="." )
trees=read.csv("Tree Measurements YNP2012_4Aug2015.csv",header=T,sep=",",dec=".",na.strings=".")
summary(stands)
stands$SiteAbbrev[stands$Site.Full.Name=="84 blowdown"]
stands$SiteAbbrev=as.character(stands$SiteAbbrev)
stands$SiteAbbrev=tolower(stands$SiteAbbrev)
stands$SiteAbbrev=as.factor(stands$SiteAbbrev)

trees$Site.Abbrev[trees$Full.Site.Name=="84 Blowdown"]
trees$Site.Abbrev=as.character(trees$Site.Abbrev)
trees$Site.Abbrev=tolower(trees$Site.Abbrev)
trees$Site.Abbrev[trees$Full.Site.Name=="84 Blowdown"]="x84blowdn"
trees$Site.Abbrev=as.factor(trees$Site.Abbrev)

###Investigate the densities for stands that are and are not serotinous for 20 year old LP
summary(stands)
hist(stands$PICO_dens)
summary(trees)
trees$Tree.Height..m.=ifelse(trees$Tree.Height..m.==15,5,trees$Tree.Height..m.)

summary(test)  ###
####Reading the 1999 data in 
stands99=read.csv("YNP 1999 Extensive Data Summary.csv",header=T,sep=";",dec=".",na.strings="." )
summary(stands99)
stands99$SiteAbbrev[stands99$Site.Full.Name=="84 blowdown"]
stands99$SiteAbbrev=as.character(stands99$SiteAbbrev)
stands99$SiteAbbrev=tolower(stands99$SiteAbbrev)
stands99$SiteAbbrev=as.factor(stands99$SiteAbbrev)
merged=merge(stands,stands99, by="SiteAbbrev")
summary(merged)
#####################
###Looking at the 1999 data to find stands that are not serotinous to get density.  
test=subset(merged,Elevation.x<2377)
test2=subset(merged, Elevation.x>2377&PICO_dens.x<1000)  ###looking at median density for stands that do not have serotinous cones
summary(test)
summary(test2)
test2 #Idetnified southeast arm from the remaining plots as a stand which is pure lodgepole that is close to 600 stems per ha. Set the density value to 600.  
test3=subset(test,PICO_dens.x<47000&PICO_dens.x>43,0000) #By looking at densities in 1999 I found the mean was 760000 trees per ha which the closest one to that is Gneiss creek south at 72000 per ha.  So Bam. used it. 
test3
###Need to pull this site from the weird 1999 data. 
###
#Investigating serotinous stands
test3=subset(merged, serotinous2>=1)  ###looking at median density for stands that do not have serotinous cones

summary(test3)# Here the median density is 8116 stems per ha. the median ANPP is 1.104 The best stand here is Firehole meadows North 
test3$minus=test3$PICO_dens.y-8116.5

######################################
####Now Douglas_fir 10 
donato=read.csv("FVS_df_tree_data2.csv", header=T,sep=",",dec=".",na.strings=".")
summary(donato)
######looking at plots that had only Douglas-fir to start with: This didn't work very well, the median density was only 200 stems per ha.
donato$df=ifelse(donato$Species=="DF", 0,1)
donato.agg=aggregate( df~ Stand_ID, data = donato, sum)
donato.agg2=subset(donato.agg, df==0)
donato.agg2
donato.df=subset(donato, Stand_ID=="cache_creek_4"|Stand_ID=="cache_creek_5"|Stand_ID=="elk_creek_3"|Stand_ID=="hoo_doo_5"|Stand_ID=="jones_creek_5"|Stand_ID=="jones_creek_7"|Stand_ID=="jones_creek_8")
summary(donato.df)
donatodf.agg=aggregate( Tree_Count~ Stand_ID, data = donato.df, sum)
summary(donatodf.agg)
d
###alternatively, we only look at the Douglas-fir within all stands, even if other species were present in those plots. Here we get a median density of 556 stems per ha. Much more reasonable.  
donato2=subset(donato, Species=="DF")
summary(donato2)
donato2.agg=aggregate( Tree_Count~ Stand_ID, data = donato2, sum)
summary(donato2.agg)
donatoall.agg=aggregate( Tree_Count~ Stand_ID, data = donato, sum)
summary(donatoall.agg)
donato2.agg$minus=donato2.agg$Tree_Count

donato3=subset(donato2, Stand_ID=="hoo_doo_4")
summary(donato3)
###convert from standard to metric
donato3$DBH=donato3$DBH*2.54
donato3$Ht=donato3$Ht*0.3048
donato3.10=subset(donato3, Age<15)
summary(donato3.10)
str(donato3.10)
write.csv(donato3.10, "D:/PHD 2013-2018/fire modelling/iland/model/stand level/the model/material/tree init/df_10.csv")





