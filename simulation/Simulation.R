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

food.words <- c("fil+et","frozen", "steaks", "fried", "steamed", "baked", 
                "special", "whole", "sushi", "wild", "caught", "cooked", "farmed",
                "farm-raised", "loin", "salted", "seafood", "gourmet")

foreign.names <- matrix(c("thunfisch|thon|Aton|atum","tuna", "bacalhau|baccalo|kabeljou|cabillaud","cod",
                   "heller|hamachi", "yellowfin tuna", "Pazifischer","Pacific", "seezunge", "sole", "maguro|toro|akami", "bluefin tuna",
                   "basa", "pangasius bocourti", "swai", "pangasianodon hypopthalmus"),ncol=2, byrow=T)
name.col <- percentage.table$Market.name
true.name <- percentage.table$Actual.stock
out <- clean_up_labels(name.col, food.words, foreign.names,percentage.table$Actual.stock)
cleaned.up <- read.csv("DataFixed.csv")
table.match <- read.csv("FDA_Acceptable_Names.csv")
cleaned.up$Sci.actuals <- as.character(cleaned.up$Sci.actuals)
cleaned.up$Sci.labels <- as.character(cleaned.up$Sci.labels)
cleaned.up$Market.name <- as.character(cleaned.up$Market.name)
table.match$SCIENTIFIC.NAME <- as.character(table.match$SCIENTIFIC.NAME)
#Match common names from spreadsheet to FDA accepted common names to get species
  for(j in 1:nrow(cleaned.up)){
    if(is.na(cleaned.up$Sci.labels[j])){
      ind <- grep(cleaned.up$Market.name[j],table.match$COMMON.NAME, ignore.case=TRUE)
      if(length(ind)==1){

          cleaned.up$Sci.labels[j] <- table.match$SCIENTIFIC.NAME[ind]
        }

    } else{
    if(cleaned.up$Sci.labels[j]==""){

      ind <- grep(cleaned.up$Market.name[j],table.match$COMMON.NAME, ignore.case=TRUE)
      if(length(ind)==1){
        cleaned.up$Sci.labels[j] <- table.match$SCIENTIFIC.NAME[ind]
      }
    }
    }
    if(is.na(cleaned.up$Sci.actuals[j])){
      ind <- grep(cleaned.up$Actual.stock[j],table.match$COMMON.NAME, ignore.case=TRUE)
      if(length(ind)==1){
        cleaned.up$Sci.actuals[j] <- table.match$SCIENTIFIC.NAME[ind]
      }
    } else{
    if(cleaned.up$Sci.actuals[j]==""){
      ind <- grep(cleaned.up$Actual.stock[j],table.match$COMMON.NAME, ignore.case=TRUE)
      if(length(ind)==1){
        cleaned.up$Sci.actuals[j] <- table.match$SCIENTIFIC.NAME[ind]
        }
    }
    }
}
require(dplyr)
write.csv(cleaned.up,"DataFixed2.csv")
data.input <- read.csv(file.path(data.dir,"DataFixed3.csv"))
data.input$Sci.labels <- as.character(data.input$Sci.labels)
data.input$Sci.actuals <- as.character(data.input$Sci.actuals)
data.tbl <- select(data.input, c(Sci.labels,Sci.actuals,Mislabeled,Generally.labeled,Country.of.sample,
                                 Loc, DISTRIBUTOR,SUSHI,GROCERY,
                                 MARKET,RESTAURANT,PORT,Study,Prop,N.label, N, N.per.lab,Prob)) %>%
            filter(Study!="") %>%
            filter(Study!="Cline 2012")

##Only in initial data processing
#data.tbl <- mutate(data.tbl,N=N.label*Prop)
#data.tbl <- mutate(data.tbl, N=ifelse(grepl("Helyar",Study),1,N))
#data.tbl <- mutate(data.tbl, N=ifelse(grepl("Lamendin",Study),1,N)) %>%
#  mutate(N=ifelse(grepl("Carvalho",Study),1,N)) %>%
#  mutate(N=ifelse(grepl("Galal-Khallaf",Study),1,N)) %>%
#  mutate( N=ifelse(grepl("Ardura",Study),N.label,N)) %>%
#  mutate(N=ifelse(grepl("Nagalakshmi",Study),1,N)) %>%
#  mutate(N=ifelse(grepl("Pappalardo and Felito",Study),1,N))

#Calculate n labels and mislabeling probability
data.tbl<- get_n_per_label(data.tbl) %>% 
  get_mislabeling_prob
data.tbl$Sci.labels <- sub("\\([^\\]*?\\)","",data.tbl$Sci.labels)
data.tbl$Sci.actuals<- sub("\\([^\\]*?\\)","",data.tbl$Sci.actuals)
mutate()
data.tbl$Mislabeled<-get_mislabeled(data.tbl)
write.csv(data.tbl,"DataFixed4.csv")
mislabeling <- data.tbl %>%
  group_by(Sci.labels) %>%
  mutate(numerator=sum(Mislabeled*N)) %>%
  mutate(weighted.mean=numerator/sum(N.per.lab.y), tot.N = sum(N.per.lab.y)) %>%
  select(weighted.mean, tot.N) %>%
  unique()
  plot(unique(mislabeling)$weighted.mean)
col.cod<- ifelse(sort(unique(mislabeling)$weighted.mean)==0,3,1)
col.cod<- ifelse(sort(unique(mislabeling)$weighted.mean)==1,2,col.cod)
plot(sort(unique(mislabeling)$weighted.mean),pch=19,cex=.5,axes=NA,col=col.cod)
axis(2,las=1)
mtext("Proportion mislabeled",side=2,line=3)
mtext("Species",side=1,line=2)
str(mislabeling)
write.csv(unique(mislabeling), "MislabeledByLabel.csv")
power <- mislabeling %>%
  group_by(tot.N) %>%
  summarise(count.n=n())
plot(power$count.n~power$tot.N, log="x")

reduced_set <- mislabeling %>%
  filter(tot.N>20) %>%
  bind_cols("Genus"=as.data.frame(unlist(regmatches(reduced_set$Sci.labels,regexec("[[:upper:]][[:lower:]]+",reduced_set$Sci.labels)))))
names(reduced_set) <- c("Sci.labels", "weighted.mean", "tot.N", "Genus")
mislabel.by.genus <- reduced_set %>%
  group_by(Genus) %>%
  summarise(Genus.prob=sum(tot.N*weighted.mean)/sum(tot.N)) 

write.csv(mislabel.by.genus, "MislabelByGenus.csv")
mislabel.by.genus$Genus <- reorder(mislabel.by.genus$Genus, mislabel.by.genus$Genus.prob)
p <- ggplot(mislabel.by.genus,aes(Genus, Genus.prob))
p + geom_bar(stat="identity") +
  scale_y_continuous("Prob") +
 # scale_x_discrete(limits=as.character(mislabel.by.genus[order(mislabel.by.genus$Genus.prob),"Genus"])) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=14), axis.text.y = element_text(size=14)) +
  theme(axis.title.x = element_text( size=14), axis.title.y = element_text(size=14)) + 
  theme(panel.background = element_rect(colour="white"))

#Combine rows of mislabeling types with IUCN status
IUCN <- read.csv(file.path(data.dir,"IUCNStatus.csv"))
names(data.tbl)
iucn_labels <- semi_join(data.tbl, IUCN, by=c("Sci.labels"="species"))
iucn_both <- semi_join(iucn_labels, IUCN, by=c("Sci.actuals"="species"))

iucn_label <- iucn_both %>% 
  group_by(IUCNstatus.x) %>%
  summarise(count=sum(N)) 

iucn_actual <- iucn_both %>% 
  group_by(IUCNstatus.y) %>%
  summarise(count=sum(N)) %>%
  left_join(iucn_label,by=c("IUCNstatus.y"="IUCNstatus.x"))
names(iucn_actual) <- c("IUCNstatus","actual","label")

require(lme4)


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
