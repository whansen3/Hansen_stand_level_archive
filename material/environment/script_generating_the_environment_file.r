#####
#
## generating environment files for my 9 simulation environmnets (3 distances from seed source by 3 species)
#
#####

### read in the plot info
########NOTE that the grid is backward. See the updated YNP_stand_grid to see how it should be.
master=read.csv("master_list.csv",header=T,sep=",") # the master table
summary(master)
str(master)

### some data preparation
# 120 stands= 10*12
x=c(0:9) # 10 RUs in x dimension
y=c(0:11) # 12 RUs in y dimension

xi=rep(x,length(y))
xi
yi=c(rep(0,length(x)),rep(1,length(x)),rep(2,length(x)),rep(3,length(x)),rep(4,length(x)),rep(5,length(x)),rep(6,length(x)),rep(7,length(x)),rep(8,length(x)),rep(9,length(x)),rep(10,length(x)),rep(11,length(x)))
yi
# # grid this is blocked out because I already made it. 
id=matrix(1:120,ncol=10,byrow=T)
 id
###flipping the matrix vertically yo
 # id2=apply(t(id),1,rev)
 # id2
 # write.table(id2,file="stand_grid_df_short.txt",col.names=T,row.names=F,quote=F)

# ### write the environment file
m.1=subset(master,species=="LPS"&distance==500)
summary(m.1)
env=data.frame()
for(i in 1:dim(m.1)[1])
	{
	env=rbind(env,data.frame(
		id=i,
		x=xi[i],
		y=yi[i],
		model.climate.tableName=m.1$model.climate.name[i],
		model.site.availableNitrogen=ifelse(m.1$soil[i]=="rhyolite",45,55),
		model.site.soilDepth=95,
		model.site.pctSand=ifelse(m.1$soil[i]=="rhyolite",62,51),
		model.site.pctSilt=ifelse(m.1$soil[i]=="rhyolite",30,37),
		model.site.pctClay=ifelse(m.1$soil[i]=="rhyolite",8,12)
		))
	}

summary(env)

write.table(env,file="environment_df_short_test.txt",col.names=T,row.names=F,quote=F)