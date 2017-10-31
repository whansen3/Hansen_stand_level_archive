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
trees$serotinous2=ifelse(trees$Serotiny..y.n.=="y",1,0)
tree.agg=aggregate(serotinous2 ~ Site.Abbrev, data = trees, sum) ##Finding stands that have serotinous trees in them
summary(tree.agg)
test=merge(stands,tree.agg, by.x=c("SiteAbbrev"),by.y=c("Site.Abbrev"))
summary(test)  ###
test2=subset(test, serotinous2==0)  ###looking at median density for stands that do not have serotinous cones
summary(test2) ### 3500 stems per ha is the median
test2$minus=test2$PICO_dens-3500 ## Find the closest stand
trees2=subset(trees, Site.Abbrev=="gracamp") ##There is a site that is closer, However, it has twice the ANPP as the median. So I chose a stand that is at 3400 stems per ha but the correct ANPP.
write.csv(trees2, "D:/PHD 2013-2018/fire modelling/iland/model/stand level/the model/material/tree init/lp_20.csv")
###
#Investigating serotinous stands
test3=subset(test, serotinous2>=1)  ###looking at median density for stands that do not have serotinous cones
summary(test3)
test3$minus=test3$PICO_dens-12100
trees3=subset(trees, Site.Abbrev=="dogshdtr")
write.csv(trees3, "D:/PHD 2013-2018/fire modelling/iland/model/stand level/the model/material/tree init/lp_20_2.csv")
# the environment file for the ids
env=read.table("YNP_environment.txt",header=T)
summary(env)
################################
###Now finding lP stands that are 50 years old and 100 years old.
#######################
kashian=read.csv("Kashian Allometric Data.csv", header=T,sep=",",dec=".",na.strings=".")
summary(kashian)
kashian50=subset(kashian, Stand_age>40&Stand_age<75&beetle==0)
summary(kashian50)
unique(kashian50$Stand_name)
trees4=subset(kashian50, Stand_name=="W_Yellostone")
summary(trees4)
write.csv(trees4, "D:/PHD 2013-2018/fire modelling/iland/model/stand level/the model/material/tree init/lp_50.csv")

summary(kashian)
kashian100=subset(kashian, Stand_age>90&Stand_age<120&beetle==0)
summary(kashian100)
unique(kashian100$Stand_name)
trees5=subset(kashian100, Stand_name=="Crawfish_Creek")
summary(trees5)
write.csv(trees5, "D:/PHD 2013-2018/fire modelling/iland/model/stand level/the model/material/tree init/lp_100.csv")
######################################
####Now Douglas_fir 10 and 20 year old stands.
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
donato2.agg$minus=donato2.agg$Tree_Count-556

donato3=subset(donato2, Stand_ID=="hoo_doo_4")
summary(donato3)
###convert from standard to metric
donato3$DBH=donato3$DBH*2.54
donato3$Ht=donato3$Ht*0.3048
donato3.10=subset(donato3, Age<15)
summary(donato3.10)
str(donato3.10)
write.csv(donato3.10, "D:/PHD 2013-2018/fire modelling/iland/model/stand level/the model/material/tree init/df_10.csv")

donato3.20=subset(donato3)
summary(donato3.20)
str(donato3.20)
write.csv(donato3.20, "D:/PHD 2013-2018/fire modelling/iland/model/stand level/the model/material/tree init/df_20.csv")
########
##Old trees for Douglas fir (50 and 100)
########
donato.stand=read.table("PSMEbeetle_PlotLevelData.txt",sep="\t", header=TRUE)
summary(donato.stand)
trees.compare=read.table("PSMEbeetle_trees.txt", header = TRUE, sep = "\t",na.string = ".")[1:2679, ]
summary(donato.trees)
str(trees.compare)
summary(trees.compare)
head(trees.compare)

# Conversion

trees.compare$HEIGHT <- trees.compare$HEIGHT * 100  # Change to cm

# Add in HD and average age

trees.compare$HD <- trees.compare$HEIGHT / trees.compare$DBH
trees.compare$Age <- 175

# Create table including only Psme, only live standing trees, remove NA DBH values, and only necessary columns

trees.compare.psme <- subset(trees.compare, SPECIES == "PSME" & DOWN == FALSE & LIVEDEAD == "L" & DBH != "NA")
str(trees.compare.psme)  # 723 observations
summary(trees.compare.psme)

# Subset this table for only green and red stands

levels(trees.compare.psme$SITE)
trees.compare.psme.greenred <- subset(trees.compare.psme, SITE == "Buffalo_Fork" | SITE == "Lone_Pronghorn" 
						| SITE == "Lozier_Hill" | SITE == "Sig.l_Mtn" | SITE == "Slough_Ck_U"
						| SITE == "Druid_R" | SITE == "Hatchet_R" | SITE == "Norris_Mtn"
						| SITE == "Rose_Ck_R" | SITE == "Thunderer")
str(trees.compare.psme.greenred)  # 435 observations
summary(trees.compare.psme.greenred)

# Aggregate the data for green and red stands

agg.trees.compare.psme.greenred <- aggregate(cbind(DBH, HEIGHT, HD) ~ Age + SITE, 
									data = trees.compare.psme.greenred,	mean)
agg.trees.compare.psme.greenred.density <- aggregate(DENSITYSCALEFACTOR ~ Age + SITE, 
									data = trees.compare.psme.greenred,	sum)
str(agg.trees.compare.psme.greenred)
summary(agg.trees.compare.psme.greenred)
str(agg.trees.compare.psme.greenred.density)
summary(agg.trees.compare.psme.greenred.density)

# Also aggregate the data for density of all trees

trees.compare.alltrees <- subset(trees.compare, DOWN == FALSE & LIVEDEAD == "L" & DBH != "NA")
trees.compare.alltrees.greenred <- subset(trees.compare.alltrees, SITE == "Buffalo_Fork" | SITE == "Lone_Pronghorn" 
                                      | SITE == "Lozier_Hill" | SITE == "Sig.l_Mtn" | SITE == "Slough_Ck_U"
                                      | SITE == "Druid_R" | SITE == "Hatchet_R" | SITE == "Norris_Mtn"
                                      | SITE == "Rose_Ck_R" | SITE == "Thunderer")
agg.trees.compare.alltrees.greenred.density <- aggregate(DENSITYSCALEFACTOR ~ Age + SITE,
                                                         data = trees.compare.alltrees.greenred, sum)

# Add avg height, DBH to density dataframes

agg.trees.compare.alltrees.greenred.density$HEIGHT <- sapply(1:dim(agg.trees.compare.alltrees.greenred.density)[1], 
                                                       function(s){agg.trees.compare.psme.greenred$HEIGHT[agg.trees.compare.alltrees.greenred.density$SITE == agg.trees.compare.psme.greenred$SITE[s]]})
agg.trees.compare.alltrees.greenred.density$DBH <- sapply(1:dim(agg.trees.compare.alltrees.greenred.density)[1], 
                                                             function(s){agg.trees.compare.psme.greenred$DBH[agg.trees.compare.alltrees.greenred.density$SITE == agg.trees.compare.psme.greenred$SITE[s]]})
agg.trees.compare.psme.greenred.density$HEIGHT <- sapply(1:dim(agg.trees.compare.psme.greenred.density)[1], 
                                                          function(s){agg.trees.compare.psme.greenred$HEIGHT[agg.trees.compare.psme.greenred.density$SITE == agg.trees.compare.psme.greenred$SITE[s]]})
agg.trees.compare.psme.greenred.density$DBH <- sapply(1:dim(agg.trees.compare.psme.greenred.density)[1], 
                                                         function(s){agg.trees.compare.psme.greenred$DBH[agg.trees.compare.psme.greenred.density$SITE == agg.trees.compare.psme.greenred$SITE[s]]})
str(agg.trees.compare.alltrees.greenred.density)

# Add stand-level density to tree dataframe

trees.compare.psme.greenred$DENSITY_PSME <- sapply(1:dim(trees.compare.psme.greenred)[1], 
    function(s){agg.trees.compare.psme.greenred.density$DENSITYSCALEFACTOR[agg.trees.compare.psme.greenred.density$SITE == trees.compare.psme.greenred$SITE[s]]})
trees.compare.psme.greenred$DENSITY_ALLTREES <- sapply(1:dim(trees.compare.psme.greenred)[1], 
    function(s){agg.trees.compare.alltrees.greenred.density$DENSITYSCALEFACTOR[agg.trees.compare.alltrees.greenred.density$SITE == trees.compare.psme.greenred$SITE[s]]})
str(trees.compare.psme.greenred)
summary(trees.compare.psme.greenred)

##########################




