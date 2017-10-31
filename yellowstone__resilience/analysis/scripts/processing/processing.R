##iLand stand level analysis of resilience mechanisms#####
###Hansen 2017
##Processing Script 2/3
#This script is used to summarize climate data from all replicates into variables useful for analysis. This was run on an HTC cluster due to the computational demand.
####

#####
#
### loading in data
#
#####

env=read.table("experiment_key.txt",header=T) 
library(plyr)
library(lubridate)
for (i in 1:20){
water=read.csv( paste("debug_water_cycle",i,".csv",sep=""),header=T,sep=";")
head(water)
water$date <- as.Date(as.character(water$date), format='%Y%m%d')
water$cal.year=year(water$date)
water$month=month(water$date)
water$julian=yday(water$date)
w.2<- ddply(water, c("rid","year","month"), summarise,
               temp    = mean(temp),
			   prec    = sum(prec),
			   potential   = mean(psi_kpa/1000),
			   content   = mean(content))
              
### Calculating 
w.3<- ddply(w.2, c("rid","year"), summarise,
               temp    = mean(temp[month>3&month<9]),
			   prec    = sum(prec),
			   potential   = mean(potential[month>3&month<9]),
			   content   = mean(content[month>3&month<9]))

w.4=merge(w.3,env,by="rid",all.x=TRUE)
w.510=subset(w.4,model.stand.age==10)
w.5102<- ddply(w.510, c("rid"), summarise,
               temp.short    = mean(temp[year>1&year<5]),
			   temp.long    = mean(temp[year>1&year<31]),
			   prec.short   = mean(prec[year>1&year<5]),
			   prec.long    = mean(prec[year>1&year<31]),
			   potential.short    = mean(potential[year>1&year<5]),
			   potential.long    = mean(potential[year>1&year<31]),
			    content.short    = mean(content[year>1&year<5]),
			   content.long    = mean(content[year>1&year<31]))
			   
w.520=subset(w.4,model.stand.age==20)
w.5202<- ddply(w.520, c("rid"), summarise,
               temp.short    = mean(temp[year>11&year<15]),
			   temp.long    = mean(temp[year>11&year<41]),
			   prec.short   = mean(prec[year>11&year<15]),
			   prec.long    = mean(prec[year>11&year<41]),
			   potential.short    = mean(potential[year>11&year<15]),
			   potential.long    = mean(potential[year>11&year<41]),
			    content.short    = mean(content[year>11&year<15]),
			   content.long    = mean(content[year>11&year<41]))			   
			   

w.550=subset(w.4,model.stand.age==50)
w.5502<- ddply(w.550, c("rid"), summarise,
               temp.short    = mean(temp[year>41&year<45]),
			   temp.long    = mean(temp[year>41&year<71]),
			   prec.short   = mean(prec[year>41&year<45]),
			   prec.long    = mean(prec[year>41&year<71]),
			   potential.short    = mean(potential[year>41&year<45]),
			   potential.long    = mean(potential[year>41&year<71]),
			    content.short    = mean(content[year>41&year<45]),
			   content.long    = mean(content[year>41&year<71]))	



w.5100=subset(w.4,model.stand.age==100)
w.51002<- ddply(w.5100, c("rid"), summarise,
               temp.short    = mean(temp[year>91&year<95]),
			   temp.long    = mean(temp[year>91&year<121]),
			   prec.short   = mean(prec[year>91&year<95]),
			   prec.long    = mean(prec[year>91&year<121]),
			   potential.short    = mean(potential[year>91&year<95]),
			   potential.long    = mean(potential[year>91&year<121]),
			    content.short    = mean(content[year>91&year<95]),
			   content.long    = mean(content[year>91&year<121]))	
			   
	w.final=rbind(w.5102,w.5202,w.5502,w.51002)	
w.final$replicate=i	
write.table(w.final,paste("climate",i,".txt"),sep="\t") 
} 