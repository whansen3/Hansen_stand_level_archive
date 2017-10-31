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
df10=read.csv("df_10.csv",header=T,sep=",",dec=".",na.strings="." )
###
###reating dataframes for each of just the variables I want. 
summary(df10)
df10=cbind.data.frame(df10$DBH, df10$Ht, df10$Age)
colnames(df10) <- c("DBH", "height","age")
str(df10)

###Okay now I repeat each set three times to start lining them up with the environment file
df2=sapply(df10, rep.int, times=360)

str(df2)

df2=data.frame(df2)
summary(df2)
library(plyr)

summary(df2)
# the environment file for the ids
env=read.csv("master_list.csv",sep=",") ###this reads in the master excel file which I use to get the climate table names.
env2=read.table("environment.txt", head=TRUE)# Read in the environment file to get the id number that lines up with the model climate table name in the master list.
summary(env)
summary(env2)
str(env)

dfenv=env[721:1080,] # subset the list just to df
dfenv2=data.frame(env2[721:1080,1])
colnames(dfenv2)=c("id")
dfenv2
str(dfenv)
library(plyr)
dfenv=cbind.data.frame(dfenv,dfenv2) # combine model climate table name and id number 
dfenv
###Repeat each set of rows the correct number of times (to match the number of trees in each stand age) by creating a freq variable and then repeating rows based on the frequency table. 
dfenv$freq=10
summary(dfenv)
dfenv.ex <- dfenv[rep(row.names(dfenv), dfenv$freq), 1:4]
str(dfenv.ex)
dfenvname=data.frame(dfenv.ex[c(1,4)])
summary(dfenvname)

df_trees=data.frame(cbind(dfenvname,df2))
summary(df_trees)
df_trees$count=55
df_trees$species="Psme"
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
lp2=sapply(lp10, rep.int, times=360)

str(lp2)

lp2=data.frame(lp2)
summary(lp2)
library(plyr)


# the environment file for the ids
env=read.csv("master_list.csv",sep=",") ###this reads in the master excel file which I use to get the climate table names.
env2=read.table("environment.txt", head=TRUE)# Read in the environment file to get the id number that lines up with the model climate table name in the master list.
summary(env)
summary(env2)
str(env)

dfenv=env[361:720,] # subset the list just to df
dfenv2=data.frame(env2[361:720,1])
colnames(dfenv2)=c("id")
dfenv2
str(dfenv)
library(plyr)
dfenv=cbind.data.frame(dfenv,dfenv2) # combine model climate table name and id number 
dfenv
###Repeat each set of rows the correct number of times (to match the number of trees in each stand age) by creating a freq variable and then repeating rows based on the frequency table. 
dfenv$freq=25
summary(dfenv)
dfenv.ex <- dfenv[rep(row.names(dfenv), dfenv$freq), 1:4]
str(dfenv.ex)
dfenvname=data.frame(dfenv.ex[c(1,4)])
summary(dfenvname)

lp_trees=data.frame(cbind(dfenvname,lp2))
summary(lp_trees)
lp_trees$species="Pico"
####
###
###Reading in LP_S
####
###
lps10=read.csv("lp_s10.csv",header=T,sep=",",dec=".",na.strings="." )
###
###reating dataframes for each of just the variables I want. 
summary(lps10)
lps10$DBH=0
lps10$height=lps10$height/100
lps10=cbind.data.frame(lps10$DBH, lps10$height, lps10$age,lps10$count )
colnames(lps10) <- c("DBH", "height","age","count")
str(lps10)

###Okay now I repeat each set three times to start lining them up with the environment file
lps2=sapply(lps10, rep.int, times=360)

str(lps2)

lps2=data.frame(lps2)
summary(lps2)
library(plyr)


# the environment file for the ids
env=read.csv("master_list.csv",sep=",") ###this reads in the master excel file which I use to get the climate table names.
env2=read.table("environment.txt", head=TRUE)# Read in the environment file to get the id number that lines up with the model climate table name in the master list.
summary(env)
summary(env2)
str(env)

dfenv=env[1:360,] # subset the list just to df
dfenv2=data.frame(env2[1:360,1])
colnames(dfenv2)=c("id")
dfenv2
str(dfenv)
library(plyr)
dfenv=cbind.data.frame(dfenv,dfenv2) # combine model climate table name and id number 
dfenv
###Repeat each set of rows the correct number of times (to match the number of trees in each stand age) by creating a freq variable and then repeating rows based on the frequency table. 
dfenv$freq=25
summary(dfenv)
dfenv.ex <- dfenv[rep(row.names(dfenv), dfenv$freq), 1:4]
str(dfenv.ex)
dfenvname=data.frame(dfenv.ex[c(1,4)])
summary(dfenvname)

lp_strees=data.frame(cbind(dfenvname,lps2))
summary(lp_strees)
lp_strees$species="PicS"
trees_f=rbind(lp_strees,lp_trees)
trees_f=rbind(trees_f,df_trees)
str(trees_f)

#####
#
## saplings <4m height
#
#####

sap=subset(trees_f, trees_f$height<4) #subset data to trees less than four meter
summary(sap)	
str(sap)
		sapinit=cbind.data.frame(
			stand_id=sap$id,
			species=sap$species, 
			count=sap$count,
			height_from=sap$height-0.1, # adding some jitter here, as initializing a larger number of *identical* trees will likely result in mortality waves due to missing differentiation between individuals
			height_to=sap$height+0.1,
			age=sap$age,
			min_lif=0 
		)
		summary(sapinit)
	
str(sapinit)
summary(sapinit)
head(sapinit)



write.table(sapinit,file="stand_model_init.txt",col.names=T,row.names=F,sep=";") # write out the sap initiation file



#####
#
## trees >4m height
#
#####

df_tree=subset(df_trees, df_trees$height>4) # subset to trees greater than four meters
summary(df_tree)
treeinit=data.frame()
	treeinit=cbind.data.frame(
			stand_id=df_tree$id,
			species="Psme", # for now for testing purposes only, because we don't have pico 
			count=df_tree$count,
			dbh_from=df_tree$DBH-0.2, # adding some jitter here, as initializing a larger number of *identical* trees will likely result in mortality waves due to missing differentiation between individuals
			dbh_to=df_tree$DBH+0.2,
			hd=df_tree$height*100/df_tree$DBH-0.2,
			age=df_tree$age		
		)
	


summary(treeinit)
head(treeinit)

# any trees <4m?
summary(treeinit$dbh_from/100*treeinit$hd)


write.table(treeinit,file="YNP_init_trees_psme.txt",col.names=T,row.names=F,sep=";") # write out the tree initialization file. 

