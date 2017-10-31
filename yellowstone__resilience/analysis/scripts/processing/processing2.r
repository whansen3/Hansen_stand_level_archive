##iLand stand level analysis of resilience mechanisms#####
###Hansen 2017
##Processing Script 3/3
#This script is used to summarize the frost after bud birst from the establishment debug for use in analyses of regeneration failure
####

#####
#
### loading in data
#
#####
env=read.table("experiment_key.txt",header=T) 
library(plyr)
for (i in 1:20){
frost=read.csv( paste("debug_establishment",i,".csv",sep=""),header=T,sep=";")
head(frost)

f.1=merge(frost,env,by="rid",all.x=TRUE)

w.210=subset(f.1,model.stand.age==10)
w.2102<- ddply(w.210, c("rid"), summarise,
               frost.short    = mean(TACAfrostFREE[year>1&year<5]),
			   frost.long    = mean(TACAfrostFREE[year>1&year<31]))
			   
			   
w.220=subset(f.1,model.stand.age==20)
w.2202<- ddply(w.220, c("rid"), summarise,
               frost.short    = mean(TACAfrostFREE[year>11&year<15]),
			   frost.long    = mean(TACAfrostFREE[year>11&year<41]))
			  	   
			   

w.250=subset(f.1,model.stand.age==50)
w.2502<- ddply(w.250, c("rid"), summarise,
               frost.short    = mean(TACAfrostFREE[year>41&year<45]),
			   frost.long    = mean(TACAfrostFREE[year>41&year<71]))

w.3100=subset(f.1,model.stand.age==100)
w.53002<- ddply(w.3100, c("rid"), summarise,
               frost.short    = mean(TACAfrostFREE[year>91&year<95]),
			   frost.long    = mean(TACAfrostFREE[year>91&year<121]))
			   
	w.final=rbind(w.2102,w.2202,w.2502,w.21002)	
w.final$replicate=i	
write.table(w.final,paste("frost",i,".txt"),sep="\t") 
} 