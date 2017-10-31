#####
#
## climate data import for stand level analysis of resilience mechanisms
#
#####
#1. Reading in the Maca data files.  Here I used the Macav2-Metdata because it has relative humiditiy which is necessary for calculating vpd. I also used the CNRM gcm for the same reason.  
#2. Reading in daylength data from Daymet for my low elevation, mid elevation, and high elevation sites (note that actually it doesn't matter which site.... Note because I don't have future day lengths I use 1980 as the day length year.  
#3. subsetting the future datasets and merging the day length 
#4. Rename variables and rbind into a single large dataset
#5. Unit conversions

### 1. reading in maca

master=read.csv("master_list.csv",header=T,sep=";") # the master table
#read in future data
low_rcp_4.5=read.csv("low_rcp4.5_cnrm.csv",header=T,sep=",",skip=17)
head(low_rcp_4.5)

low_rcp_8=read.csv("low_rcp8_cnrm.csv",header=T,sep=",",skip=17)
head(low_rcp_8)

mid_rcp_4.5=read.csv("mid_rcp4.5_cnrm.csv",header=T,sep=",",skip=17)
head(mid_rcp_4.5)

mid_rcp_8=read.csv("mid_rcp8_cnrm.csv",header=T,sep=",",skip=17)
head(mid_rcp_8)

high_rcp_4.5=read.csv("high_rcp4.5_cnrm.csv",header=T,sep=",",skip=17)
head(high_rcp_4.5)

high_rcp_8=read.csv("high_rcp8_cnrm.csv",header=T,sep=",",skip=17)
head(high_rcp_8)


medium_hist=read.csv("mid_historic_cnrm.csv",header=T,sep=",",skip=17)
head(medium_hist)
high_hist=read.csv("high_historic_cnrm.csv",header=T,sep=",",skip=17)
head(high_hist)

###
##
#2.Reading in day length...
##
####
lowday=read.csv("elk_creek_7.csv",header=T,sep=",",skip=7) # the master table
summary(lowday)
l.day=subset(lowday,year==1980)
l.day=data.frame(l.day[,3])
summary(l.day)
str(l.day)
l.day$day=c(c(1:31),c(1:28),c(1:31),c(1:30),c(1:31),c(1:30),c(1:31),c(1:31),c(1:30),c(1:31),c(1:30),c(1:31)) # NB: no leap-years in Daymet - all years have only 365 days
l.day$month=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))

midday=read.csv("CougMod.csv",header=T,sep=",",skip=7) # the master table
summary(midday)
m.day=subset(midday,year==1980)
m.day=data.frame(m.day[,5])
summary(m.day)
str(m.day)
m.day$day=c(c(1:31),c(1:28),c(1:31),c(1:30),c(1:31),c(1:30),c(1:31),c(1:31),c(1:30),c(1:31),c(1:30),c(1:31)) # NB: no leap-years in Daymet - all years have only 365 days
m.day$month=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))


highday=read.csv("SumLkSE.csv",header=T,sep=",",skip=7) # the master table
summary(highday)
h.day=subset(highday,year==1980)
h.day=data.frame(h.day[,5])
summary(h.day)
h.day$day=c(c(1:31),c(1:28),c(1:31),c(1:30),c(1:31),c(1:30),c(1:31),c(1:31),c(1:30),c(1:31),c(1:30),c(1:31)) # NB: no leap-years in Daymet - all years have only 365 days
h.day$month=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
########
###
#3. subsetting future datasets and converting dates.. Also merging with daylength from above
###
#######
library(lubridate)
low_rcp_4.5$yyyy.mm.dd=as.Date(low_rcp_4.5$yyyy.mm.dd, format="%Y-%m-%d")
low_rcp_4.5$yyyy.mm.dd=as.POSIXlt(low_rcp_4.5$yyyy.mm.dd)
low_rcp_4.5$year=year(low_rcp_4.5$yyyy.mm.dd)
low_rcp_4.5$month=month(low_rcp_4.5$yyyy.mm.dd)
low_rcp_4.5$day=day(low_rcp_4.5$yyyy.mm.dd)
summary(low_rcp_4.5)
low_rcp_4.5_2030=subset(low_rcp_4.5, year>2028 &year<2060)
low_rcp_4.5_2030$id="low_rcp_45_2030"
low_rcp_4.5_2030=merge(low_rcp_4.5_2030,l.day, by=c("month","day"),all.x=TRUE)
low_rcp_4.5_2030$l.day...3.= ifelse(is.na(low_rcp_4.5_2030$l.day...3.) ==TRUE,39053,low_rcp_4.5_2030$l.day...3.)
summary(low_rcp_4.5_2030)


low_rcp_4.5_2070=subset(low_rcp_4.5, year>2068 &year<2101)
low_rcp_4.5_2070$id="low_rcp_45_2070"
low_rcp_4.5_2070=merge(low_rcp_4.5_2070,l.day, by=c("month","day"),all.x=TRUE)
low_rcp_4.5_2070$l.day...3.= ifelse(is.na(low_rcp_4.5_2070$l.day...3.) ==TRUE,39053,low_rcp_4.5_2070$l.day...3.)
summary(low_rcp_4.5_2070)
summary(low_rcp_4.5_2070)


low_rcp_8$yyyy.mm.dd=as.Date(low_rcp_8$yyyy.mm.dd, format="%Y-%m-%d")
low_rcp_8$yyyy.mm.dd=as.POSIXlt(low_rcp_8$yyyy.mm.dd)
low_rcp_8$year=year(low_rcp_8$yyyy.mm.dd)
low_rcp_8$month=month(low_rcp_8$yyyy.mm.dd)
low_rcp_8$day=day(low_rcp_8$yyyy.mm.dd)
summary(low_rcp_8)
low_rcp_8_2030=subset(low_rcp_8, year>2028 &year<2060)
low_rcp_8_2030$id="low_rcp_8_2030"
low_rcp_8_2030=merge(low_rcp_8_2030,l.day, by=c("month","day"),all.x=TRUE)
low_rcp_8_2030$l.day...3.= ifelse(is.na(low_rcp_8_2030$l.day...3.) ==TRUE,39053,low_rcp_8_2030$l.day...3.)
summary(low_rcp_8_2030)



summary(low_rcp_8_2030)
low_rcp_8_2070=subset(low_rcp_8, year>2068 &year<2101)
low_rcp_8_2070$id="low_rcp_8_2070"
low_rcp_8_2070=merge(low_rcp_8_2070,l.day, by=c("month","day"),all.x=TRUE)
low_rcp_8_2070$l.day...3.= ifelse(is.na(low_rcp_8_2070$l.day...3.) ==TRUE,39053,low_rcp_8_2070$l.day...3.)
summary(low_rcp_8_2070)
summary(low_rcp_8_2070)

mid_rcp_4.5$yyyy.mm.dd=as.Date(mid_rcp_4.5$yyyy.mm.dd, format="%Y-%m-%d")
mid_rcp_4.5$yyyy.mm.dd=as.POSIXlt(mid_rcp_4.5$yyyy.mm.dd)
mid_rcp_4.5$year=year(mid_rcp_4.5$yyyy.mm.dd)
mid_rcp_4.5$month=month(mid_rcp_4.5$yyyy.mm.dd)
mid_rcp_4.5$day=day(mid_rcp_4.5$yyyy.mm.dd)
summary(mid_rcp_4.5)
mid_rcp_4.5_2030=subset(mid_rcp_4.5, year>2028 &year<2060)

mid_rcp_4.5_2030$id="mid_rcp_45_2030"
mid_rcp_4.5_2030=merge(mid_rcp_4.5_2030,m.day, by=c("month","day"),all.x=TRUE)
mid_rcp_4.5_2030$midday...5.= ifelse(is.na(mid_rcp_4.5_2030$midday...5.) ==TRUE,39053,mid_rcp_4.5_2030$midday...5.)
summary(mid_rcp_4.5_2030)


summary(mid_rcp_4.5_2030)
mid_rcp_4.5_2070=subset(mid_rcp_4.5, year>2068 &year<2101)
mid_rcp_4.5_2070$id="mid_rcp_45_2070"
mid_rcp_4.5_2070=merge(mid_rcp_4.5_2070,m.day, by=c("month","day"),all.x=TRUE)
mid_rcp_4.5_2070$midday...5.= ifelse(is.na(mid_rcp_4.5_2070$midday...5.) ==TRUE,39053,mid_rcp_4.5_2070$midday...5.)
summary(mid_rcp_4.5_2070)
summary(mid_rcp_4.5_2070)


mid_rcp_8$yyyy.mm.dd=as.Date(mid_rcp_8$yyyy.mm.dd, format="%Y-%m-%d")
mid_rcp_8$yyyy.mm.dd=as.POSIXlt(mid_rcp_8$yyyy.mm.dd)
mid_rcp_8$year=year(mid_rcp_8$yyyy.mm.dd)
mid_rcp_8$month=month(mid_rcp_8$yyyy.mm.dd)
mid_rcp_8$day=day(mid_rcp_8$yyyy.mm.dd)
summary(mid_rcp_8)
mid_rcp_8_2030=subset(mid_rcp_8, year>2028 &year<2060)
mid_rcp_8_2030$id="mid_rcp_8_2030"
mid_rcp_8_2030=merge(mid_rcp_8_2030,m.day, by=c("month","day"),all.x=TRUE)
mid_rcp_8_2030$midday...5.= ifelse(is.na(mid_rcp_8_2030$midday...5.) ==TRUE,39053,mid_rcp_8_2030$midday...5.)
summary(mid_rcp_8_2030)

summary(mid_rcp_8_2030)
mid_rcp_8_2070=subset(mid_rcp_8, year>2068 &year<2101)
mid_rcp_8_2070$id="mid_rcp_8_2070"
mid_rcp_8_2070=merge(mid_rcp_8_2070,m.day, by=c("month","day"),all.x=TRUE)
mid_rcp_8_2070$midday...5.= ifelse(is.na(mid_rcp_8_2070$midday...5.) ==TRUE,39053,mid_rcp_8_2070$midday...5.)
summary(mid_rcp_8_2070)
summary(mid_rcp_8_2070)

high_rcp_4.5$yyyy.mm.dd=as.Date(high_rcp_4.5$yyyy.mm.dd, format="%Y-%m-%d")
high_rcp_4.5$yyyy.mm.dd=as.POSIXlt(high_rcp_4.5$yyyy.mm.dd)
high_rcp_4.5$year=year(high_rcp_4.5$yyyy.mm.dd)
high_rcp_4.5$month=month(high_rcp_4.5$yyyy.mm.dd)
high_rcp_4.5$day=day(high_rcp_4.5$yyyy.mm.dd)
summary(high_rcp_4.5)

high_rcp_4.5_2030=subset(high_rcp_4.5, year>2028 &year<2060)
high_rcp_4.5_2030$id="high_rcp_45_2030"
high_rcp_4.5_2030=merge(high_rcp_4.5_2030,h.day, by=c("month","day"),all.x=TRUE)
high_rcp_4.5_2030$highday...5.= ifelse(is.na(high_rcp_4.5_2030$highday...5.) ==TRUE,39053,high_rcp_4.5_2030$highday...5.)

summary(high_rcp_4.5_2030)
high_rcp_4.5_2070=subset(high_rcp_4.5, year>2068 &year<2101)
summary(high_rcp_4.5_2070)
high_rcp_4.5_2070$id="high_rcp_45_2070"
high_rcp_4.5_2070=merge(high_rcp_4.5_2070,h.day, by=c("month","day"),all.x=TRUE)
high_rcp_4.5_2070$highday...5.= ifelse(is.na(high_rcp_4.5_2070$highday...5.) ==TRUE,39053,high_rcp_4.5_2070$highday...5.)
summary(high_rcp_4.5_2070)

high_rcp_8$yyyy.mm.dd=as.Date(high_rcp_8$yyyy.mm.dd, format="%Y-%m-%d")
high_rcp_8$yyyy.mm.dd=as.POSIXlt(high_rcp_8$yyyy.mm.dd)
high_rcp_8$year=year(high_rcp_8$yyyy.mm.dd)
high_rcp_8$month=month(high_rcp_8$yyyy.mm.dd)
high_rcp_8$day=day(high_rcp_8$yyyy.mm.dd)
summary(high_rcp_8)
high_rcp_8_2030=subset(high_rcp_8, year>2028 &year<2060)
high_rcp_8_2030$id="high_rcp_8_2030"
high_rcp_8_2030=merge(high_rcp_8_2030,h.day, by=c("month","day"),all.x=TRUE)
high_rcp_8_2030$highday...5.= ifelse(is.na(high_rcp_8_2030$highday...5.) ==TRUE,39053,high_rcp_8_2030$highday...5.)
summary(high_rcp_8_2030)
high_rcp_8_2070=subset(high_rcp_8, year>2068 &year<2101)
summary(high_rcp_8_2070)
high_rcp_8_2070$id="high_rcp_8_2070"
high_rcp_8_2070=merge(high_rcp_8_2070,h.day, by=c("month","day"),all.x=TRUE)
high_rcp_8_2070$highday...5.= ifelse(is.na(high_rcp_8_2070$highday...5.) ==TRUE,39053,high_rcp_8_2070$highday...5.)
summary(high_rcp_8_2070)

low_hist=read.csv("low_historic_cnrm.csv",header=T,sep=",",skip=17)
med_hist=read.csv("mid_historic_cnrm.csv",header=T,sep=",",skip=17)
summary(medium_hist)
high_hist=read.csv("high_historic_cnrm.csv",header=T,sep=",",skip=17)
head(high_hist)
#read in 


low_hist$yyyy.mm.dd=as.Date(low_hist$yyyy.mm.dd, format="%Y-%m-%d")
low_hist$yyyy.mm.dd=as.POSIXlt(low_hist$yyyy.mm.dd)
low_hist$year=year(low_hist$yyyy.mm.dd)
low_hist$month=month(low_hist$yyyy.mm.dd)
low_hist$day=day(low_hist$yyyy.mm.dd)
summary(low_hist)
low_hist=subset(low_hist, year>1949 &year<1981)
low_hist$id="low_hist"
low_hist=merge(low_hist,l.day, by=c("month","day"),all.x=TRUE)
low_hist$l.day...3.= ifelse(is.na(low_hist$l.day...3.) ==TRUE,39053,low_hist$l.day...3.)
summary(low_hist)

med_hist$yyyy.mm.dd=as.Date(med_hist$yyyy.mm.dd, format="%Y-%m-%d")
med_hist$yyyy.mm.dd=as.POSIXlt(med_hist$yyyy.mm.dd)
med_hist$year=year(med_hist$yyyy.mm.dd)
med_hist$month=month(med_hist$yyyy.mm.dd)
med_hist$day=day(med_hist$yyyy.mm.dd)
summary(med_hist)
med_hist$id="med_hist"




med_hist=subset(med_hist, year>1949 &year<1981)
med_hist=merge(med_hist,m.day, by=c("month","day"),all.x=TRUE)
med_hist$midday...5.= ifelse(is.na(med_hist$midday...5.) ==TRUE,39053,med_hist$midday...5.)
summary(med_hist)

high_hist$yyyy.mm.dd=as.Date(high_hist$yyyy.mm.dd, format="%Y-%m-%d")
high_hist$yyyy.mm.dd=as.POSIXlt(high_hist$yyyy.mm.dd)
high_hist$year=year(high_hist$yyyy.mm.dd)
high_hist$month=month(high_hist$yyyy.mm.dd)
high_hist$day=day(high_hist$yyyy.mm.dd)
summary(high_hist)
high_hist=subset(high_hist, year>1949 &year<1981)
high_hist$id="high_hist"
high_hist=merge(high_hist,h.day, by=c("month","day"),all.x=TRUE)
high_hist$highday...5.= ifelse(is.na(high_hist$highday...5.) ==TRUE,39053,high_hist$highday...5.)
####
###
#4. renaming variables and binding into a master dataset
###
####
###
dfs=list(low_hist, med_hist, high_hist,low_rcp_4.5_2030 ,low_rcp_8_2030,low_rcp_4.5_2070 ,low_rcp_8_2070,mid_rcp_4.5_2030 ,mid_rcp_8_2030,
mid_rcp_4.5_2070 ,mid_rcp_8_2070 ,high_rcp_4.5_2030 ,high_rcp_8_2030,high_rcp_4.5_2070 ,high_rcp_8_2070 )
ChangeNames <- function(x) {
    names(x) <- c("month","day","date", "max_temp", "min_temp","rel_max","rel_min","prec","rad","year","id","day_length")
    return(x)
}
dfs <- lapply(dfs, ChangeNames)
masterclim=data.frame(do.call("rbind", dfs))

summary(masterclim)

str(masterclim)
####
##
#5. doing unit conversions
#######
# changing max and min temp from kelvin to celsius
masterclim$max_temp=masterclim$max_temp-273.15
masterclim$min_temp=masterclim$min_temp-273.15


# calculating the sum of daily radiation by converting from watts per meter squared to MJ/m2/day
masterclim$rad=masterclim$rad*masterclim$day_length/1000000 # in MJ/m²/day
summary(masterclim) # looks reasonable, albeit the max values are quite high
summary(master.clim[master.clim$rad>30,])

# vpd in kPa Used the FAO version of penman monteith http://www.fao.org/docrep/x0490e/x0490e07.htm
###First we calculate mean saturation vapor pressure

#et=0.6108exp(17.27T/T+237.3)
masterclim$maxe=0.6108*exp(17.27*masterclim$max_temp/(masterclim$max_temp+237.3))

masterclim$mine=0.6108*exp(17.27*masterclim$min_temp/(masterclim$min_temp+237.3))
masterclim$meane=(masterclim$maxe+masterclim$mine)/2
#ea= etmin*rhmax/100+etmax*rmin/100/2

masterclim$ea=(masterclim$mine*(masterclim$rel_max/100)+masterclim$maxe*(masterclim$rel_min/100))/2
summary(masterclim)
masterclim$date <- NULL
masterclim$vpd=masterclim$meane-masterclim$ea
masterclim$id=as.factor(masterclim$id)

list(masterclim$id)
masterclim$year=as.integer(masterclim$year)
masterclim$month=as.integer(masterclim$month)
masterclim$max_temp=round(masterclim$max_temp, digits=1)
masterclim$min_temp=round(masterclim$min_temp, digits=1)
masterclim$prec=round(masterclim$prec, digits=1)
masterclim$rad=round(masterclim$rad, digits=2)
masterclim$vpd=round(masterclim$vpd, digits=2)
head(masterclim)
#masterclim=masterclim[!(masterclim$month==2 & masterclim$day==29),]
library(plyr)
masterclim=arrange(masterclim,id,year,month,day)

str(dt)
# looks ok, although I would expect the max VPD values to be higher


### write sqlite database
library(RSQLite)
# NB: first need to create an empty sqlite db with that name in the respective folder
db.conn <<- dbConnect(RSQLite::SQLite(), dbname="stand_level_climate.sqlite" )
for(i in levels(masterclim$id))
	{
	# getting them in the right order and with the correct column headings - see http://iland.boku.ac.at/ClimateData
	dat=masterclim[masterclim$id==i,c(9,1,2,4,3,7,8,16)] 
	names(dat)[4]="min_temp"
	names(dat)[5]="max_temp"
	names(dat)[6]="prec"
	names(dat)[8]="vpd"
	
	dbWriteTable(db.conn, name=paste(i),dat, row.names=F)
	}
	
	
dbDisconnect(db.conn)
	
	
