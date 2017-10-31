###iLand stand level analysis of resilience mechanisms#####
###Hansen 2017
##Processing Script 1/3
#This script is used to combine all replicates of the iLand outputs into one and to merge those with correpsonding climate data
####
setwd("C:/work/stand level/the model/yellowstone__resilience/analysis/lps_short")
#setwd("H:/model working folder/stand level/the model/yellowstone__resilience/analysis/lps_short")##Update to appropriate path
filelist=list.files(pattern = ".*.txt")
data <- lapply(filelist, function(x)read.table(x,header=T))
summary(data)
r = do.call("rbind", data) 
summary(r)
table(r$replicate)
r$period=factor(r$period, levels = c("hist","2030","2070"))
r$elevation=factor(r$elevation, levels = c("low","med","high"))
r$rcp=factor(r$rcp, levels = c("0","4.5","8"))
r$age_burned=factor(r$model.stand.age.x.post, levels = c("10","20","50","100"))

setwd("C:/work/stand level/the model/yellowstone__resilience/analysis/lps_short/climate_output")

#setwd("H:/model working folder/stand level/the model/yellowstone__resilience/analysis/lps_short/climate_output") ##Update to appropriate path
filelist=list.files(pattern = ".*.txt")
data <- lapply(filelist, function(x)read.table(x,header=T))
summary(data)
r2 = do.call("rbind", data) 
summary(r2)
str(r2)
table(r2$replicate)
r3=data.frame(r2[order(r2$rid,r2$replicate),])
table(r3$rid)
data2=merge(r,r2,by=c("rid","replicate"))
summary(data2)
str(data2)

setwd("C:/work/stand level/the model/yellowstone__resilience/analysis/lps_short/frost")

#setwd("H:/model working folder/stand level/the model/yellowstone__resilience/analysis/lps_short/frost") ##Update to appropriate path
filelist=list.files(pattern = ".*.txt")
data <- lapply(filelist, function(x)read.table(x,header=T))
summary(data)
r4 = do.call("rbind", data) 
summary(r4)
str(r4)
table(r4$replicate)
r5=data.frame(r4[order(r4$rid,r4$replicate),])
table(r5$rid)
data3=merge(data2,r4,by=c("rid","replicate"))
summary(data3)
str(data3)
write.table(data3,"C:/work/stand level/the model/yellowstone__resilience/analysis/lps_short_agg.txt",sep="\t") ###Update to appropriate path and name









