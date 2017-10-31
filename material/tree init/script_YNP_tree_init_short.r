#####
#
## YNP regeneration data
#
#
#####
### This is the script where I create the sapling initialization files for the stand level experiment.
### before this, stands were identified in the other script
### read in data
rm(list = ls())
df10=read.csv("lp_s10.csv",header=T,sep=",",dec=".",na.strings="." )
###
###reating dataframes for each of just the variables I want. 
summary(df10)
df10=cbind.data.frame(df10$height, df10$age, df10$count)
colnames(df10) <- c("height","age","count")
str(df10)

###Okay now I repeat each set three times to start lining them up with the environment file
df2=sapply(df10, rep.int, times=120)

str(df2)

df2=data.frame(df2)
summary(df2)
library(plyr)

summary(df2)
# the environment file for the ids
env=read.csv("master_list.csv",sep=",") ###this reads in the master excel file which I use to get the climate table names.
env2=read.table("environment_short.txt", head=TRUE)# Read in the environment file to get the id number that lines up with the model climate table name in the master list.
summary(env)
summary(env2)
str(env)
dfenv=subset(env,species=="LP" & distance==50)

colnames(env2)=c("id")
env2
str(dfenv)
library(plyr)
dfenv=cbind.data.frame(dfenv,env2) # combine model climate table name and id number 
dfenv
###Repeat each set of rows the correct number of times (to match the number of trees in each stand age) by creating a freq variable and then repeating rows based on the frequency table. 
dfenv$freq=25
summary(dfenv)
str(dfenv)
dfenv.ex <- dfenv[rep(row.names(dfenv), dfenv$freq), 1:8]
str(dfenv.ex)
dfenvname=data.frame(dfenv.ex[c(1,8)])
summary(dfenvname)

df_trees=data.frame(cbind(dfenvname,df2))
summary(df_trees)
df_trees$count=55
df_trees$species="PicS"
df_trees$height=df_trees$height/100
#####
#
## saplings <4m height
#
#####

sap=subset(df_trees, df_trees$height<4) #subset data to trees less than four meter
summary(sap)	
str(sap)
		sapinit=cbind.data.frame(
			stand_id=sap$id,
			species=sap$species, 
			count=sap$count,
			height_from=sap$height-0.1, # adding some jitter here, as initializing a larger number of *identical* trees will likely result in mortality waves due to missing differentiation between individuals
			height_to=sap$height+0.1,
			age=sap$age
		)
		summary(sapinit)
	
str(sapinit)
summary(sapinit)
head(sapinit)
write.table(sapinit,file="stand_model_init_lps.txt",col.names=T,row.names=F,sep=";") # write out the sap initiation file


####
###
###Reading in LP_NS
####
###
lp10=read.csv("lp_10.csv",header=T,sep=",",dec=".",na.strings="." )
###
###reating dataframes for each of just the variables I want. 
summary(lp10)
lp10$DBH=0
lp10$height=lp10$height/100
lp10=cbind.data.frame(lp10$DBH, lp10$height, lp10$age,lp10$count )
colnames(lp10) <- c("DBH", "height","age","count")
str(lp10)

###Okay now I repeat each set three times to start lining them up with the environment file
lp2=sapply(lp10, rep.int, times=120)

str(lp2)

lp2=data.frame(lp2)
summary(lp2)
library(plyr)


# the environment file for the ids
env=read.csv("master_list.csv",sep=",") ###this reads in the master excel file which I use to get the climate table names.
env2=read.table("environment_short.txt", head=TRUE)# Read in the environment file to get the id number that lines up with the model climate table name in the master list.
summary(env)
summary(env2)
str(env)
lpenv=subset(env,species=="LP" & distance==50)

colnames(env2)=c("id")
env2
str(lpenv)
library(plyr)
lpenv=cbind.data.frame(lpenv,env2) # combine model climate table name and id number 
dfenv
###Repeat each set of rows the correct number of times (to match the number of trees in each stand age) by creating a freq variable and then repeating rows based on the frequency table. 
lpenv$freq=25
summary(lpenv)
lpenv.ex <- lpenv[rep(row.names(lpenv), lpenv$freq), 1:8]
str(lpenv.ex)
lpenvname=data.frame(lpenv.ex[c(1,8)])
summary(lpenvname)

lp_trees=data.frame(cbind(lpenvname,lp2))
summary(lp_trees)
lp_trees$species="Pico"
#####
#
## saplings <4m height
#
#####

sap=subset(lp_trees, lp_trees$height<4) #subset data to trees less than four meter
summary(sap)	
str(sap)
		sapinit=cbind.data.frame(
			stand_id=sap$id,
			species=sap$species, 
			count=sap$count,
			height_from=sap$height-0.1, # adding some jitter here, as initializing a larger number of *identical* trees will likely result in mortality waves due to missing differentiation between individuals
			height_to=sap$height+0.1,
			age=sap$age
			
		)
		summary(sapinit)
	
str(sapinit)
summary(sapinit)
head(sapinit)
write.table(sapinit,file="stand_model_init_lp.txt",col.names=T,row.names=F,sep=";") # write out the sap initiation file
####
###
###Reading in LP_S
####
###
lp10=read.csv("lp_s10.csv",header=T,sep=",",dec=".",na.strings="." )
###
###reating dataframes for each of just the variables I want. 
summary(lp10)
lp10$DBH=0
lp10$height=lp10$height/100
lp10=cbind.data.frame(lp10$DBH, lp10$height, lp10$age,lp10$count )
colnames(lp10) <- c("DBH", "height","age","count")
str(lp10)

###Okay now I repeat each set three times to start lining them up with the environment file
lp2=sapply(lp10, rep.int, times=120)

str(lp2)

lp2=data.frame(lp2)
summary(lp2)
library(plyr)


# the environment file for the ids
env=read.csv("master_list.csv",sep=",") ###this reads in the master excel file which I use to get the climate table names.
env2=read.table("environment_short.txt", head=TRUE)# Read in the environment file to get the id number that lines up with the model climate table name in the master list.
summary(env)
summary(env2)
str(env)
lpenv=subset(env,species=="LPS" & distance==50)

colnames(env2)=c("id")
env2
str(lpenv)
library(plyr)
lpenv=cbind.data.frame(lpenv,env2) # combine model climate table name and id number 
dfenv
###Repeat each set of rows the correct number of times (to match the number of trees in each stand age) by creating a freq variable and then repeating rows based on the frequency table. 
lpenv$freq=25
summary(lpenv)
lpenv.ex <- lpenv[rep(row.names(lpenv), lpenv$freq), 1:8]
str(lpenv.ex)
lpenvname=data.frame(lpenv.ex[c(1,8)])
summary(lpenvname)

lp_trees=data.frame(cbind(lpenvname,lp2))
summary(lp_trees)
lp_trees$species="PicS"
#####
#
## saplings <4m height
#
#####

sap=subset(lp_trees, lp_trees$height<4) #subset data to trees less than four meter
summary(sap)	
str(sap)
		sapinit=cbind.data.frame(
			stand_id=sap$id,
			species=sap$species, 
			count=sap$count,
			height_from=sap$height-0.1, # adding some jitter here, as initializing a larger number of *identical* trees will likely result in mortality waves due to missing differentiation between individuals
			height_to=sap$height+0.1,
			age=sap$age
			 
		)
		summary(sapinit)
	
str(sapinit)
summary(sapinit)
head(sapinit)
write.table(sapinit,file="stand_model_init_lps.txt",col.names=T,row.names=F,sep=";") # write out the sap initiation file



















