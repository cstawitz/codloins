if(grep('Christine',Sys.info()[["user"]])==1){
  setwd(paste("C:/Users/",Sys.info()[["user"]],"/Documents/GitHub/codloins/R",sep=""))
  toload <- list.files()
  for(i in 1:length(toload)){
    source(toload[i])    
  }
  data.dir <- paste("C:/Users/",Sys.info()[["user"]],"/Dropbox/Seafood Mislabeling/R files and data",sep='')
} else{
  wd <- "/Users/megsiesiple/Dropbox/Research Derby/regroupemail"
  setwd(wd)
  source("SimulationFunctions.R")
  data.dir <- wd
}

#Read in two data sources
restaurants<-read.csv(file.path(data.dir,"restaurants.csv"))
percentage.table<-read.csv(file.path(data.dir,"MislabelledFishies_LongForm.csv"))
stock.status<-read.csv(file.path(data.dir,"StockStatus.csv"))
# restaurants<-cbind(1:201,restaurants)
# #Restrict to our four main stocks
# restaurants<-restaurants[,2:7]
# resultsMatChi<-resultsMatNY<-resultsMatHou<-resultsMatLA<-matrix(nrow=nsims,ncol=3)


resultsMat <- matrix(nrow=nsims,ncol=5)
nsims=100
nPeople=100
#Dummy NHANES data - to be populated with true probabilities once we get em
# if(length(NHANES.dat)>1){
#   Chi <- c(.25,.25,.25,.25)
#   NY <- c(.25,.2,.3,.25)
#   LA <- c(.1,.25,.25,.4)
#   Hou <- c(.2,.3,.25,.25)
#   NHANES <- cbind(Chi,NY,LA,Hou)  
# } else{
#   NHANES <- matrix(c(.25,.25,.25,.25),4)
# }
# rownames(NHANES)<-c("Red Snapper","Tuna","Salmon","Atlantic Cod")
NHANES.dat <- read.csv(file.path(data.dir,"NHANES Data/NHANES nationwide.csv"))
if(ncol(NHANES.dat)==5){
names(NHANES.dat) <- c("ID","Label","Mean","SD","N")
Ntot <- sum(NHANES.dat$Mean)
#Get the MLE and upper and loWer CIs. Currently wrong
NHANES <- filter(NHANES.dat,Label %in% c("cod","tuna","salmon","unknown_fish","breaded_fish","other_fish")) %>%
  mutate(mle=Mean/sum(Mean), upper = mle+SD/sum(SD), lower = mle-(SD/(sum(SD))))
NHANES[NHANES$lower<0,]$lower<-rep(0,2)

#Plot the data
NHANES %>% 
  group_by(Label) %>%
  ggplot(aes(x=factor(Label),y=mle, fill=1)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=lower,ymax=upper)) +
  theme(axis.text.x = element_text(angle=45,hjust=1))
} else{
  #Todo: Insert code to do the above per region
}
  
for(i in 1:nsims){
#Cities must be: Chi, LA, NYC, or Hou
  if(ncol(NHANES)>8){
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
  } else{
    usa.df<-make_consumer_choices("USA",nPeople, NHANES)
    stock<-as.tbl(stock.status)
    usa.df$realstock <- as.integer(usa.df$realstock)
    names(stock)[1]<-"realstock"
    results.tot <- left_join(usa.df,stock)
    resultsMat[i,1:5]<-apply(results.tot[,c("Market.price","Overfishing","Overfished","Low.data","MSC.Certified")],2,mean,na.rm=T)
  }

}
print(resultsMat)
names(resultsMat)<-c("")
apply(resultsMatChi,2,min)
apply(resultsMatChi,2,max)
apply(resultsMatNY,2,min)
apply(resultsMatNY,2,max)
