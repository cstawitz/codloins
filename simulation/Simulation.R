wd <- "/Users/megsiesiple/Dropbox/Research Derby/regroupemail"
setwd(wd)
source("SimulationFunctions.R")

#Read in two data sources
restaurants<-read.csv("restaurants.csv")
percentage.table<-read.csv("MislabelledFishies.csv")
stock.status<-read.csv("StockStatus.csv")
restaurants<-cbind(1:201,restaurants)

#Restrict to our four main stocks
restaurants<-restaurants[,2:7]
nsims=100
nPeople=100
resultsMatChi<-resultsMatNY<-resultsMatHou<-resultsMatLA<-matrix(nrow=nsims,ncol=3)
#Dummy NHANES data - to be populated with true probabilities once we get em
Chi <- c(.25,.25,.25,.25)
NY <- c(.25,.2,.3,.25)
LA <- c(.1,.25,.25,.4)
Hou <- c(.2,.3,.25,.25)
NHANES <- cbind(Chi,NY,LA,Hou)
rownames(NHANES)<-c("Red Snapper","Tuna","Salmon","Atlantic Cod")
for(i in 1:nsims){
#Cities must be: Chi, LA, NYC, or Hou
chicago.df<-make_consumer_choices("Chi",nPeople)
print(chicago.df)
ny.df<-make_consumer_choices("NY",nPeople)
houston.df<-make_consumer_choices("Hou",nPeople)
la.df<-make_consumer_choices("LA",nPeople)

resultsMatChi[i,1:3]<-apply(chicago.df[5:7],2,mean,na.rm=T)
print(resultsMatChi)
resultsMatHou[i,1:3]<-apply(houston.df[5:7],2,mean,na.rm=T)
resultsMatLA[i,1:3]<-apply(la.df[5:7],2,mean,na.rm=T)
resultsMatNY[i,1:3]<-apply(ny.df[5:7],2,mean,na.rm=T)
}

apply(resultsMatChi,2,min)
apply(resultsMatChi,2,max)
apply(resultsMatNY,2,min)
apply(resultsMatNY,2,max)
