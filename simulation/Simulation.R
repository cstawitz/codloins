if("Christine" == Sys.info()[["user"]]){
  setwd("C:/Users/Christine/Documents/GitHub/codloins/R")
  toload <- list.files()
  for(i in 1:length(toload)){
    source(toload[i])    
  }
  data.dir <- "C:/Users/Christine/Documents/Dropbox/Seafood Mislabeling/R files and data"
} else{
  wd <- "/Users/megsiesiple/Dropbox/Research Derby/regroupemail"
  setwd(wd)
  source("SimulationFunctions.R")
  data.dir <- wd
}

#Read in two data sources
restaurants<-read.csv(file.path(data.dir,"restaurants.csv"))
percentage.table<-read.csv(file.path(data.dir,"MislabelledFishies_mcs.csv"))
stock.status<-read.csv(file.path(data.dir,"StockStatus.csv"))
# restaurants<-cbind(1:201,restaurants)
# #Restrict to our four main stocks
# restaurants<-restaurants[,2:7]
# resultsMatChi<-resultsMatNY<-resultsMatHou<-resultsMatLA<-matrix(nrow=nsims,ncol=3)

nsims=100
nPeople=100
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
