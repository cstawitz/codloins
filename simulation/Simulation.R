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
stock.status<-read.csv(file.path(data.dir,"FishyStatuses.csv"), strip.white=TRUE)
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


################################################################
# Start here unless DataFixed4 needs to be remade!!
#############################################################
data.tbl <- read.csv(file.path(data.dir,"DataFixed9.csv"), strip.white=TRUE)
data.tbl$Sci.labels <- as.character(data.tbl$Sci.labels)
data.tbl$Sci.actuals <- as.character(data.tbl$Sci.actuals)
require(dplyr)
data.tbl<-get_n_per_label(data.tbl)
data.tbl <- data.tbl[,-c(19)]
names(data.tbl)[19]<- "N.per.lab"
data.tbl <- get_mislabeling_prob(data.tbl)
write.csv(data.tbl, file.path(data.dir,"DataFixed9.csv"))



data.tbl <- data.tbl %>%
  bind_cols("Genus"=as.data.frame(unlist(regmatches(data.tbl$Sci.labels,regexec("[[:upper:]][[:lower:]]+",data.tbl$Sci.labels)))))
names(data.tbl)[20] <- "Genus"
mislabeling <- data.tbl %>%
  group_by(Sci.labels) %>%
  mutate(numerator=sum(Mislabeled*N)) %>%
  mutate(weighted.mean=numerator/sum(N.per.lab.y), tot.N = sum(N.per.lab.y)) %>%
  select(weighted.mean, tot.N, Genus) %>%
  unique()
  plot(unique(mislabeling)$weighted.mean)
  
  
makeSampleMaps(data.tbl, "MapUpdated.png")  
  
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

## Read in price data
price.tbl <- read.csv(file.path(data.dir,"Rearranged_prices.csv"))
m <- regexec("[1-2][0-9]{2,4}",as.character(data.tbl$Study))
study_years <- regmatches(as.character(data.tbl$Study), m)
##Add in years
data.tbl$year <- study_years
data.tbl[data.tbl$Study=="FDA",]$year<-rep(2012,104)
data.tbl[data.tbl$Study=="FSH340_Lorenz",]$year<-rep(2007,39)


get_prices <- function(data.tbl, price.tbl){
price.tbl$scientific_name <- as.character(price.tbl$scientific_name)

data.tbl$price_label <- data.tbl$price_actual <- rep(NA, nrow(data.tbl))
#Match prices - USD/metric ton
for(i in 1:nrow(data.tbl)){
  actual <- data.tbl$Sci.actuals[i]
  label <- data.tbl$Sci.labels[i]
  year <- data.tbl$year[i]
  data.for.actual <- slice(price.tbl, grep(actual, price.tbl$scientific_name))
  data.for.label <- slice(price.tbl, grep(label, price.tbl$scientific_name))
  print(i)

  if(length(unique(data.for.label$scientific_name))>1){
   label <- paste(label,"sp") 
   data.for.label <- slice(price.tbl, grep(label, price.tbl$scientific_name))
  }
  if(length(unique(data.for.actual$scientific_name))>1){
    actual <- paste(actual, "sp")
    data.for.actual <- slice(price.tbl, grep(actual, price.tbl$scientific_name))
  }
  #Match up species and year for actual 
  if(nrow(data.for.actual)>0){
  if(!is.na(data.for.actual$Year)){
  if(year > max(data.for.actual$Year, na.rm=TRUE)){
    data.tbl$price_actual[i] <- data.for.actual[which.max(data.for.actual$Year),]$wtexvessel
  } else if(year < min(data.for.actual$Year, na.rm=TRUE)){
    data.tbl$price_actual[i] <- data.for.actual[which.min(data.for.actual$Year),]$wtexvessel
  } else if(year %in% data.for.actual$Year){
    data.tbl$price_actual[i] <- filter(data.for.actual, Year==year)$wtexvessel
  }
  else {
    print(paste("No actual match for ", i))
  }
  }
  }
  #Match up species and year for label 

  if(nrow(data.for.label)>0){
  if(!is.na(data.for.label$Year)){
  if(year > max(data.for.label$Year, na.rm=TRUE)){
    data.tbl$price_label[i] <- data.for.label[which.max(data.for.label$Year),]$wtexvessel
  } else if(year < min(data.for.label$Year, na.rm=TRUE)){
    data.tbl$price_label[i] <- data.for.label[which.min(data.for.label$Year),]$wtexvessel
  } else if(year %in% data.for.label$Year){
    data.tbl$price_label[i] <- filter(data.for.label, Year==year)$wtexvessel
  }
  else {
    print(paste("No label match for ", i))
  }
    }
  }
}
return(data.tbl)
}
data.tbl <- get_prices(data.tbl, price.tbl)


#Calculate total money lost
actual_price <- data.tbl %>%
  filter(price_actual!=0) %>%
  summarise(sum(N*price_actual, na.rm=T))

label_price <- data.tbl %>%
  summarise(sum(N*price_label, na.rm=T))

(label_price-actual_price)/(1000)

#Money lost/gained by SPP
price.per.genus <- data.tbl %>%
  group_by(Genus) %>%
  summarise('percent'=sum(N*(price_actual/price_label), na.rm=T)/sum(N), 'tot'=sum(N))


non_zero <- price.per.genus %>%
  filter(percent!=1, percent!=0)

zeros <- as.character(price.per.genus$Genus[(price.per.genus$percent==1)||price.per.genus$percent==0])

non_zero$Genus <- factor(non_zero$Genus, levels=as.character(non_zero$Genus), exclude=zeros)
non_zero <- non_zero %>%
  filter(tot>=10) %>%
  mutate("Diff"=(lab-act)/1000)

#combine groupers
non_zero[8,]$percent <- (non_zero[8,]$percent*non_zero[8,]$tot+non_zero[24,]$percent*non_zero[24,]$tot)/(non_zero[8,]$tot+non_zero[24,]$tot)
non_zero <- non_zero[-24,]
non_zero[25,]$percent <- (non_zero[25,]$percent*non_zero[25,]$tot+non_zero[36,]$percent*non_zero[36,]$tot)/(non_zero[25,]$tot+non_zero[36,]$tot)
non_zero <- non_zero[-36,]


price.per.genus %>% 
  filter(percent!=0) %>%
  summarise(sum(percent*tot)/sum(tot))


non_zero$Common <- c("Eel","White seabass", "Mud sole","Dolphinfish", "Sea bass", "Chilean sea bass", "Anchovy", 
                     "Grouper", "Cod", "Cusk eel", "Halibut", "Skipjack",
                     "Lates perch", "Lemon sole","Monkfish", "Snapper", "Marlin", "Haddock",
                    "Whiting", "Hake", "Dover sole", "Striped bass", "Smooth hound",
                    "Pacific salmon", "Seabream", "Flounder","Perch", "Croaker","Plaice","Eur. pollock",
                    "Atlantic salmon", 
                    "Mackerel", "Wahoo", "Amberjack", "Sole", "Seabream",
                    "Tuna", "Snoek", "Swordfish")
non_zero$Common <- reorder(non_zero$Common, non_zero$percent)
require(reshape2)
non_zero.m <- melt(non_zero)
require(ggplot2)
textdf <- data.frame("x"=c(2,-2), "y"=c(10,10), "label"=c("Consumer loses value","Consumer gains value"))
non_zero$pos<-ifelse(non_zero$percent<0, "minus","plus")

p <- ggplot(non_zero, aes(Common, (percent-1)*100, fill=log(tot))) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip() +
  theme_classic() +
  scale_y_continuous("Actual price percentage of label", labels=c(0,50,100,200)) +
  scale_x_discrete("Labeled genus") +
  scale_fill_continuous("log(sample size)") +
  theme(axis.text=element_text(size=6), axis.title=element_text(size=12), 
        legend.title=element_text(size=8), legend.text=element_text(size=10)) 
p 

#Only different prices
non_zero <- non_zero %>%
  filter(act!=lab)
require(reshape2)
non_zero.m <- melt(non_zero)

p <- ggplot(non_zero.m, aes(Genus, fill=variable, y=value)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip() +
  theme_classic() +
  scale_y_continuous("Price/Item (USD)") +
  #scale_x_discrete(expand=c(-0.2,-0.2))+
  scale_fill_discrete("Prices", labels=c("Actual fish price", "Labeled fish price"))
p + theme(axis.text=element_text(size=20))

#Aggregate by genus for calculations
by_genus <- data.tbl %>%
  group_by(Genus) %>%
  mutate(numerator=sum(N*Mislabeled)) %>%
  mutate("weighted.mean"=numerator/sum(N), "tot.N" = sum(N)) 

sum(data.tbl$N*data.tbl$Mislabeled)
sum(data.tbl$N)

prop <- data.tbl %>%
  group_by(Study) %>%
  summarise("prop"=sum(N*Mislabeled),sum(N))



mean(prop$prop)

1413/6241
#Remove genera which are less than 5% of total
#Remove anything with less than 10 per genus
by_genus <- by_genus %>%
  filter(tot.N>=10)

names(reduced_set) <- c("Sci.labels", "weighted.mean", "tot.N", "Genus")
mislabel.by.genus <- by_genus

write.csv(mislabel.by.genus, "MislabelByGenus.csv")
mislabel.by.genus$Genus <- reorder(mislabel.by.genus$Genus, mislabel.by.genus$weighted.mean)
mislabel.by.genus <- mislabel.by.genus %>% filter(weighted.mean!=0)
require(ggplot2)
p <- ggplot(mislabel.by.genus,aes(x=Genus, y=weighted.mean, col=fam))
p + geom_bar(stat="identity") +
  scale_y_continuous("Prob", limits=c(0,1)) +
 # scale_x_discrete(limits=as.character(mislabel.by.genus[order(mislabel.by.genus$Genus.prob),"Genus"])) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=14), axis.text.y = element_text(size=14)) +
  theme(axis.title.x = element_text( size=14), axis.title.y = element_text(size=14)) + 
  theme(panel.background = element_rect(colour="white")) + coord_flip()

library(taxize)
mislabel.by.genus$fam <- rep(NA,nrow(mislabel.by.genus))
for(i in 128:nrow(mislabel.by.genus)){
  if(as.character(mislabel.by.genus$Genus[i])=="Pangasianodon"){
    code <- unlist(classification("Pangasius", db='itis', return_id=FALSE))
  } else{
    code <- unlist(classification(as.character(mislabel.by.genus$Genus[i]), db='itis', return_id=FALSE))
  }
  ind <- which(code=="Family")
  mislabel.by.genus$fam[i] <- as.character(code[ind/2-1])
}

write.csv(mislabel.by.genus,file.path(data.dir,"MislabelByGenusFam.csv"))

pdf(file.path(data.dir,"MIslabelByGenus.pdf"), height=10)
p + geom_point() +
  scale_y_continuous("Mean proportion mislabeled") +
  scale_x_discrete() +
  scale_colour_hue("Family")+
  theme(axis.text.x = element_text(angle=45, hjust=1, size=8), axis.text.y = element_text(size=7)) +
  theme(axis.title.x = element_text( size=12), axis.title.y = element_text(size=14), legend.text=element_text(size=7)) + coord_flip() +
  theme_classic()
dev.off()


Prob.by.country <- data.tbl %>%
  group_by(Country.of.sample) %>%
  summarise("Mislabel.prob"=sum(Mislabeled*Prob)/sum(Prob*N), "Mislabel.prob.uncert"=sqrt((sum(Mislabeled*Prob)/sum(Prob*N))*(1-(sum(Mislabeled*Prob)/sum(Prob*N)))/sum(N)), "Tot"=sum(N)) %>%
  filter(Tot>10)

str(Prob.by.country)

plot(data=Prob.by.country)
require(ggplot2)
Prob.by.country$Country.of.sample <- reorder(Prob.by.country$Country.of.sample, Prob.by.country$Mislabel.prob)
ggplot(Prob.by.country, aes(factor(Country.of.sample),Mislabel.prob)) + 
  geom_point() +
  geom_segment(aes(y=Mislabel.prob-1.96*Mislabel.prob.uncert, yend=Mislabel.prob+1.96*Mislabel.prob.uncert, x=Country.of.sample, xend=Country.of.sample)) +
  coord_flip() +
  scale_y_continuous("Mislabeled proportion") +
  scale_x_discrete("Country")
data.tbl <- data.tbl %>%
  mutate(N=round(N,0))

duplicate_n <- function(data.row){
  n.samples <- as.numeric(data.row[17])
  rows.to.add <- data.row
  if(n.samples!=1){
    for(i in 2:n.samples){
      rows.to.add<- rbind(rows.to.add, data.row)
    }
  }
  return(rows.to.add)
}
data.duped <- data.tbl[1,]
for(i in 2:nrow(data.tbl)){
  data.duped <- bind_rows(data.duped,duplicate_n(data.tbl[i,]))
}
names(data.duped)[21] <- "Genus"
write.csv(data.duped,file.path(data.dir,"Dataduped.csv"))

#Which are the most common genera?
byGenus <- data.tbl %>% 
  group_by(Genus) %>%
  summarise("Tot"=sum(N)) 
reorder(byGenus$Genus,byGenus$Tot)

data.tbl$DISTRIBUTOR[is.na(data.tbl$DISTRIBUTOR)]<-rep(0, sum(is.na(data.tbl$DISTRIBUTOR)))
data.tbl$SUSHI[is.na(data.tbl$SUSHI)]<-rep(0, sum(is.na(data.tbl$SUSHI)))
data.tbl$GROCERY[is.na(data.tbl$GROCERY)]<-rep(0, sum(is.na(data.tbl$GROCERY)))
data.tbl$MARKET[is.na(data.tbl$MARKET)]<-rep(0, sum(is.na(data.tbl$MARKET)))
data.tbl$RESTAURANT[is.na(data.tbl$RESTAURANT)]<-rep(0, sum(is.na(data.tbl$RESTAURANT)))
data.tbl$PORT[is.na(data.tbl$PORT)]<-rep(0, sum(is.na(data.tbl$PORT)))


bySource <- data.tbl %>%
  group_by(PORT, GROCERY, MARKET, RESTAURANT, SUSHI, DISTRIBUTOR) %>%
  summarise("prop_source"=sum(Mislabeled*N)/sum(N)) %>%
  filter(sum(PORT,GROCERY,MARKET,RESTAURANT,SUSHI,DISTRIBUTOR)==1)


barplot(sort(bySource$prop_source)[-7], axes=F)
axis(1, labels=c("Market", "Grocery", "Distributor", "Port", "Restaurant", "Sushi"), at=seq(0.5,6.5,length.out=6))
axis(2, las=1)
mtext(side=2, "Mean Mislabeled Proportion", line=3)

p <- ggplot(bySource, aes(PORT, GROCERY, MARKET, RESTAURANT, SUSHI, DISTRIBUTOR, prop_source))
p + geom_bar(stat="identity") +
  scale_y_continuous("Prob") +
  # scale_x_discrete(limits=as.character(mislabel.by.genus[order(mislabel.by.genus$Genus.prob),"Genus"])) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=14), axis.text.y = element_text(size=14)) +
  theme(axis.title.x = element_text( size=14), axis.title.y = element_text(size=14)) + 
  theme(panel.background = element_rect(colour="white"))

pdf(file.path(data.dir,"MIslabelByGenus.pdf"), height=10)
p + geom_bar(stat="identity") +
  scale_y_continuous("Prob") +
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), axis.text.y = element_text(size=11)) +
  theme(axis.title.x = element_text( size=14), axis.title.y = element_text(size=14)) + coord_flip()


grouped_Mislabel <- data.tbl %>%
  filter(Genus %in% c("Salmo", "Oncorhynchus",  "Thunnus", "Ictalurus", "Gadus")) %>%
  group_by(Country.of.sample, Genus, DISTRIBUTOR, SUSHI, GROCERY, MARKET ,RESTAURANT,PORT) %>%
  mutate("Mislabeled.num"=Mislabeled*N) %>%
  summarise("Wrong"=round(sum(Mislabeled.num),0), "Total"=round(sum(N),0)) %>%
  ungroup() %>%
  mutate(Genus = factor(Genus, levels=c("Salmo", "Oncorhynchus",  "Thunnus", "Ictalurus", "Gadus"))) %>%
  mutate(Country.of.sample=as.character(Country.of.sample))

countvals <- function(x) {
  ux <- unique(x)
  return(length(ux))
}

grouped_Mislabel <- grouped_Mislabel %>%
  group_by(Study) %>%
  summarise("TotWrong"=sum(Wrong), "Tot"=sum(Total), "NLabel"=countvals(as.character(Genus)))

data.tbl %>%
  group_by(Study,Sci.actuals) %>%
  summarise("tot.actual"=sum(N))

#Run funnel plot
dat <- escalc(measure="PLO", xi=TotWrong, ni=Tot, data=grouped_Mislabel)
model.test <- rma(yi, vi, data=dat,   method="REML")
ranktest(model.test)
funnel(model.test)

grouped_Mislabel$DISTRIBUTOR[is.na(grouped_Mislabel$DISTRIBUTOR)]<-rep(0, sum(is.na(grouped_Mislabel$DISTRIBUTOR)))
grouped_Mislabel$SUSHI[is.na(grouped_Mislabel$SUSHI)]<-rep(0, sum(is.na(grouped_Mislabel$SUSHI)))
grouped_Mislabel$GROCERY[is.na(grouped_Mislabel$GROCERY)]<-rep(0, sum(is.na(grouped_Mislabel$GROCERY)))
grouped_Mislabel$MARKET[is.na(grouped_Mislabel$MARKET)]<-rep(0, sum(is.na(grouped_Mislabel$MARKET)))
grouped_Mislabel$RESTAURANT[is.na(grouped_Mislabel$RESTAURANT)]<-rep(0, sum(is.na(grouped_Mislabel$RESTAURANT)))
grouped_Mislabel$PORT[is.na(grouped_Mislabel$PORT)]<-rep(0, sum(is.na(grouped_Mislabel$PORT)))

require(gamlss)
#Check to see if a random effects term is needed for Country.of.sample
topSp <- quote(gamlss(cbind(Wrong,Total)~ Genus+ Country.of.sample + DISTRIBUTOR+ SUSHI + GROCERY 
                      + MARKET + RESTAURANT +PORT, data=grouped_Mislabel, family=BB, control = gamlss.control(trace=FALSE)))
find.hyper(topSp,lower=0.001,upper=25,p=1,pen=2)

#P hyper = .001, so we don't need a RE term
topSp1 <- gamlss(cbind(Wrong,Total-Wrong)~ Genus + Country.of.sample + DISTRIBUTOR+ SUSHI + GROCERY 
                      + MARKET + RESTAURANT +PORT, data=grouped_Mislabel, family=BB, control = gamlss.control(trace=FALSE))
topSp2 <- gamlss(cbind(Wrong,Total-Wrong)~ Country.of.sample + DISTRIBUTOR+ SUSHI + GROCERY 
                 + MARKET + RESTAURANT +PORT, data=grouped_Mislabel, family=BB, control = gamlss.control(trace=FALSE))
topSp3 <- gamlss(cbind(Wrong,Total-Wrong)~ Genus + DISTRIBUTOR+ SUSHI + GROCERY 
                 + MARKET + RESTAURANT +PORT, data=grouped_Mislabel, family=BB, control = gamlss.control(trace=FALSE))
topSp4 <- gamlss(cbind(Wrong,Total-Wrong)~ Genus + Country.of.sample, 
                 data=grouped_Mislabel, family=BB, control = gamlss.control(trace=FALSE))
topSp5 <- gamlss(cbind(Wrong,Total-Wrong)~ Genus, 
                 data=grouped_Mislabel, family=BB, control = gamlss.control(trace=FALSE))
topSp6 <- gamlss(cbind(Wrong,Total)~ DISTRIBUTOR+ SUSHI + GROCERY 
                 + MARKET + RESTAURANT +PORT, 
                 data=grouped_Mislabel, family=BB, control = gamlss.control(trace=FALSE))
AIC(topSp1,topSp2,topSp3,topSp4, topSp5, topSp6)
weights <- exp(-.5*AIC(topSp1,topSp2,topSp3,topSp4, topSp5, topSp6)$AIC)
weights/sum(weights)

par(cex=.7)
fitVals <- lpred(topSp6, se.fit=TRUE, type="response")
plot(fitVals$fit[c(64,74,1,8,10, 6)], axes=F, pch=19, xlab="Source", ylab="Estimated mean probability", ylim=c(0,.5))
axis(1, labels=c("Port","Distributor","Grocery","Market", "Restaurant", "Sushi"), at=1:6)
axis(2, las=1)
segments(x0=1:6, x1=1:6, y0=fitVals$fit[c(64,74,1,8,10,6)]+1.96*fitVals$se.fit[c(64,74,1,8,10,6)], y1=fitVals$fit[c(64,74,1,8,10,6)]-1.96*fitVals$se.fit[c(64,74,1,8,10,6)])


plot(fitVals$mu.coefficients, col=grouped_Mislabel$Genus, pch=19, ylim=c(0,0.5), axes=NA)
axis(1, labels=c("Gadus", "Oncorhynchus", "Salmo", "Thunnus"), at=1:4)
axis(2, las=1)
mtext(side=1, "Genus", line=2)
mtext(side=2, "Fitted mislabel probability", line=3)
segments(x0=1:4, x1=1:4, y0=fitVals$fit[1:4]+1.96*fitVals$se.fit[1:4], y1=fitVals$fit[1:4]-1.96*fitVals$se.fit[1:4], col=grouped_Mislabel$Genus)

fitVals <- lpred(topSp5, se.fit=TRUE, type="terms")
plot(fitVals$fit[1:4], col=grouped_Mislabel$Genus, pch=19, ylim=c(-1.5,1), axes=NA)
axis(1, labels=c("Gadus", "Oncorhynchus", "Salmo", "Thunnus"), at=1:4)
axis(2, las=1)
mtext(side=1, "Genus", line=2)
mtext(side=2, "Fitted mislabel probability", line=3)
segments(x0=1:4, x1=1:4, y0=fitVals$fit[1:4]+1.96*fitVals$se.fit[1:4], y1=fitVals$fit[1:4]-1.96*fitVals$se.fit[1:4], col=grouped_Mislabel$Genus)



#Best model includes genus only if the data set is restricted
plot(topSp5$residuals[1:4]~fitted(topSp5)[1:4], col=grouped_Mislabel$Genus)



noGenus <- glmer(cbind(Wrong,Total)~(1|Country.of.sample)+ DISTRIBUTOR+ SUSHI + GROCERY + MARKET + RESTAURANT +PORT, data=grouped_Mislabel, family=binomial(link=logit))
noSource <- glmer(cbind(Wrong,Total)~(1|Country.of.sample)+ Genus , data=grouped_Mislabel, family=binomial(link=logit))
# Did not converge: remov1 <- glmer(cbind(Wrong,Total)~(1|Country.of.sample)+ Genus + SUSHI + GROCERY + MARKET + RESTAURANT +PORT, data=grouped_Mislabel, family=binomial(link=logit))
#Can compare these since random effects structure is the same
AIC(topSp, noSource)


#Now re-run with REML to compare with Genus random effect
topSp <- glmer(cbind(Wrong,Total)~(1|Country.of.sample)+ (1|Genus) + DISTRIBUTOR+ SUSHI + GROCERY + MARKET + RESTAURANT +PORT, data=grouped_Mislabel, family=binomial(link=logit))
noGenus <- glmer(cbind(Wrong,Total)~(1|Country.of.sample)+ DISTRIBUTOR+ SUSHI + GROCERY + MARKET + RESTAURANT +PORT, data=grouped_Mislabel, family=binomial(link=logit))

plot(topSp)

se <- sqrt(diag(vcov(topSp)))
(tab <- cbind(Est = fixef(topSp), LL = fixef(topSp) - 1.96 * se, UL = fixef(topSp) + 1.96 *
                se))
unLogit <- function(mat){
  return(exp(mat)/(1-exp(mat)))
}
unLogit(tab)

#Calculate overdispersion
n<- length(resid(topSp))
sqrt(sum(c(resid(topSp),topSp@u))^2/n)
#Model appears overdispersed

grouped_Mislabel$obs <- 1:nrow(grouped_Mislabel)
topSp <- glmer(cbind(Wrong,Total)~(1|obs) +Genus + DISTRIBUTOR+ SUSHI + GROCERY + MARKET + RESTAURANT +PORT, data=grouped_Mislabel, family=binomial(link=logit))


gbm(cbind(Wrong,Total) ~ Genus + Country.of.sample #+ DISTRIBUTOR + SUSHI + GROCERY + MARKET + RESTAURANT +PORT
         , data=grouped_Mislabel)
anova(topSp, test="Chisq")
plot(order(resid(topSp, type="pearson")), col=grouped_Mislabel$Genus)



allSp <- unique(c(data.tbl$Sci.labels, data.tbl$Sci.actuals))
for(i in 1:nrow(stock.status)){
  stock.status$species[i] <-gsub(" sp","", stock.status$species[i], ignore.case=TRUE)
}

trim.trailing <- function (x) sub("\\s+$", "", x)

unmatched<-NULL
for(i in 1:length(allSp)){
  #allSp[i] <-sub("sp|spp|sp.|spp.","", allSp[i], ignore.case=TRUE)
  allSp[i] <- trim.trailing(allSp[i])
  if(!allSp[i] %in% unique(stock.status$species)){
   unmatched<- c(unmatched,allSp[i])
  }
}

write.csv(unmatched, "NoIUCN.csv")

#Combine rows of mislabeling types with IUCN status
stock.status$species = as.character(stock.status$species)
stock.status$IUCNstatus = as.character(stock.status$IUCNstatus)
stock.status <- stock.status[,-1]
stock.status<- unique(stock.status)
data.tbl$Status.labels <- data.tbl$Status.actuals <- rep(NA,nrow(data.tbl))
vect<-NULL
for(i in 1:nrow(data.tbl)){
  match.labels <- filter(stock.status, data.tbl$Sci.labels[i]== stock.status$species)
  match.actuals <- filter(stock.status, data.tbl$Sci.actuals[i]== stock.status$species)
  if((nrow(match.labels)==1)&&(nrow(match.actuals)==1)){
    data.tbl$Status.labels[i] <- match.labels$IUCNstatus
    data.tbl$Status.actuals[i] <- match.actuals$IUCNstatus
  }
}


for(i in 1:nrow(data.tbl)){
  if(is.na(data.tbl$Status.actuals[i])){
    match.actuals <- filter(stock.status, trim.trailing(sub("sp|spp|sp.|spp.","",data.tbl$Sci.actuals[i]))== sub("sp|spp|sp.|spp.","",stock.status$species))
    match.actuals <- unique(match.actuals)
    if(nrow(match.actuals)==1)
    {
      data.tbl$Status.actuals[i] <- match.actuals$IUCNstatus
    }
  }
  if(is.na(data.tbl$Status.labels[i])){
    match.labels <- filter(stock.status, trim.trailing(sub("sp|spp|sp.|spp.","",data.tbl$Sci.labels[i]))== sub("sp|spp|sp.|spp.","",stock.status$species))
    match.labels <- unique(match.labels)
    if(nrow(match.labels)==1)
    {
      data.tbl$Status.labels[i] <- match.labels$IUCNstatus
    } else{
      print(i)
      print(nrow(match.labels))
    }
  }
}


missin <- filter(data.tbl, is.na(Status.labels)) %>% unique() 
missin$Sci.labels

missin2 <- filter(data.tbl, is.na(Status.actuals)) %>% unique() 
missin2$Sci.actuals

filter(data.tbl, is.na(Status.labels))
data.tbl$Status.labels

data.tbl$year <- unlist(data.tbl$year)
write.csv(data.tbl, file=file.path(data.dir,"DataPricesStatuses.csv"))
data.tbl <- read.csv(file.path(data.dir,"DataPricesStatuses.csv"))



overall <- c(get_summary_price(data.tbl), get_mislabeled(data.tbl), get_summary_IUCN(data.tbl))

#Get sensitivity to each study
studies <-as.character(unique(data.tbl$Study))
sensitivityStudy<-matrix(data=NA, nrow=length(studies), ncol=4)
sens <- data.frame(sensitivityStudy)
names(sens) <- c("Study", "Price", "Mislabel", "IUCN")
for(i in 1:length(studies)){
  data.cut <- data.tbl %>%
    filter(Study!=studies[i])
  sens$Study[i] <- studies[i]
  sens$Price[i] <- as.numeric(get_summary_price(data.cut))
  sens$Mislabel[i]<- as.numeric(get_mislabeled(data.cut))
  sens$IUCN[i]<- as.numeric(get_summary_IUCN(data.cut))
}
str(sens)
par(oma=c(0,4,0,0))
plot(sens$Price, rep(1,length(studies)), axes=F, xlab="True price/Label price", ylab="",
     ylim=c(0,3.2), xlim=c(0,.6), type="l")
lines(sens$Mislabel,rep(2,length(studies)))
lines(sens$IUCN,rep(3,length(studies)))
axis(1)
axis(2, labels=c("Price", "Mislabel proportion", "IUCN status"), at=c(1,2,3), las=1)
points(overall,c(1,2,3),col="red", pch=20)

#Get sensitivity to each genus
genera <-as.character(unique(data.tbl$Genus))
sensitivityStudy<-matrix(data=NA, nrow=length(genera), ncol=4)
sens <- data.frame(sensitivityStudy)
names(sens) <- c("Genus", "Price", "Mislabel", "IUCN")
for(i in 1:length(genera)){
  data.cut <- data.tbl %>%
    filter(as.character(Genus)!=genera[i])
  sens$Genus[i] <- genera[i]
  sens$Price[i] <- as.numeric(get_summary_price(data.cut))
  sens$Mislabel[i]<- as.numeric(get_mislabeled(data.cut))
  sens$IUCN[i]<- as.numeric(get_summary_IUCN(data.cut))
}

#Remove each genus one at a time
str(sens)


data.tbl <- data.tbl %>% 
  mutate("Resamp"=N/sum(N))

data.tbl$Pick<- rep(NA,nrow(data.tbl))
data.tbl$Pick[1]<-data.tbl$Resamp[1]
for(i in 2:nrow(data.tbl)){
  data.tbl$Pick[i]<-sum(data.tbl$Resamp[1:i])
}

n<-1000
data.resampled <- matrix(NA, nrow=n,ncol=3)
data.resampled <- data.frame(data.resampled)
names(data.resampled) <- c("Price", "Mislabel", "IUCN","Diversity")
#data.resampled$Diversity<-rep(NA,n)
##Do bootstrap
system.time(for(i in 1:n){
  df <- data.tbl
  for(j in 1:nrow(data.tbl)){
    ran.unif<-runif(1)
    row.index<-which(data.tbl$Pick>=ran.unif)[1]
    df[j,]<-slice(data.tbl,row.index)
  }
  #data.resampled$Price[i]<-as.numeric(get_summary_price(df))
  #data.resampled$Mislabel[i]<- as.numeric(get_mislabeled(df))
  #data.resampled$IUCN[i]<- as.numeric(get_summary_IUCN(df))
  data.resampled$Diversity[i]<-as.numeric(get_summary_diversity(df))
})

quantile(data.resampled$Price,c(.025,.5,.975))
quantile(data.resampled$Mislabel,c(.025,.5,.975))
quantile(data.resampled$IUCN,c(.025,.5,.975))
quantile(data.resampled$Diversity,c(.025,.5,.975))
par(mfrow=c(1,3), mar=c(1,1,1,0), oma=c(2,3,2,0))
hist(data.resampled$Price, main="Actual/Label Price")
mtext("Frequency",2, line=2)
hist(data.resampled$Mislabel, main="Mislabeled Proportion")
hist(data.resampled$IUCN, main="IUCN status difference")


IUCNsumm<- data.tbl %>% 
  group_by(Status.labels, Status.actuals) %>%
  summarise(count=sum(N)) 

data.tbl %>%
  group_by(Genus) %>%
  summarise(amount=sum(N*Mislabeled)/sum(N), samp=sum(N)) %>%
  filter(samp>=10)%>%
  write.csv(file.path(data.dir,"MG.csv"))


filter(data.tbl, is.na(Status.labels)) %>% select(Sci.labels) %>% unique()
filter(data.tbl, is.na(Status.actuals)) %>% select(Sci.actuals) %>% unique()

data.tbl$Status.labels <- as.character(data.tbl$Status.labels)
data.tbl$Status.actuals <- as.character(data.tbl$Status.actuals)
data.tbl$Status.actuals[i]<-"not evaluated"
data.tbl$Label.num <- data.tbl$Actual.num <- rep(NA, nrow(data.tbl))
for(i in 1:nrow(data.tbl)){
  data.tbl$Label.num[i]<-get_num_status(data.tbl$Status.labels[i])
  data.tbl$Actual.num[i]<-get_num_status(data.tbl$Status.actuals[i])
  print(i)
}
data.tbl$Sci.labels <- as.character(data.tbl$Sci.labels)
data.tbl$Sci.actuals <- as.character(data.tbl$Sci.actuals)
data.tbl$diff <- rep(NA, nrow(data.tbl))
for(i in 1:nrow(data.tbl)){
  if(data.tbl$Sci.labels[i]==data.tbl$Sci.actuals[i]){
    data.tbl$diff[i] <- 1
  } else{
    data.tbl$diff[i] <- data.tbl$Actual.num[i]-data.tbl$Label.num[i]
  }
}


IUCNsumm$diff<-rep(NA, nrow(IUCNsumm))
IUCNsumm$diff <- c(0,2,0,0,0,0,0,-1,0,0,
                   3,0,2,1,-4,0,-3,0,0,-1,
                   -2,0,0,0,0,0,0,0,0,-2,
                   1,0,0,-1,0,-1,2,0,1,0,0)
sum(IUCNsumm$diff*IUCNsumm$count)/sum(IUCNsumm$count)


data.tbl %>% 
  group_by(Genus) %>%
  summarise(count=count(Status.labels))

IUCNsumm %>% group_by(Status.labels) %>% summarise(sum(count))
IUCNsumm %>% group_by(Status.actuals) %>% summarise(sum(count))

(data.tbl$Status.labels)
data.tbl[grep("Thunnus s", data.tbl$Sci.labels),]$Status.labels<- rep(NA,64)
data.tbl[grep("Thunnus s", data.tbl$Sci.actuals),]$Status.actuals<- rep(NA,length(data.tbl[grep("Thunnus s", data.tbl$Sci.actuals),]$Status.actuals))

#Use placeholder so statuses aren't messed up
factorize <- data.tbl
#Ridiculously complicated way to make character statuses into categorical! Remove dd, not evaluated, and NA
factorize$Status.labels <- as.integer(factor(factorize$Status.labels, 
                  levels=c("CR", "EN", "VU", "NT", "LC"), 
                  exclude=c("not evaluated", "DD"), ordered=TRUE))

#Do the same for the actual fish iucn statuses
factorize$Status.actuals <- as.integer(factor(data.tbl$Status.actuals, 
                  levels=c("CR", "EN", "VU", "NT", "LC"), 
                  exclude=c("not evaluated", "DD"), ordered=TRUE))


genera_IUCN <- factorize %>%
  filter(N>=10) %>%
  group_by(Genus, Status.labels, Status.actuals, N) %>%
  summarise("meanLabel"=mean(Status.labels, na.rm=T), "meanActual"=mean(Status.actuals, na.rm=T)) %>%
  group_by(Genus) %>%
  summarise("weighted.mean.label"=sum(N*meanLabel, na.rm=T)/sum(N, na.rm=T), "weighted.mean.actual"=sum(N*meanActual, na.rm=T)/sum(N, na.rm=T), "tot"=sum(N))



toplot <- genera_IUCN %>%
  filter(weighted.mean.label!=0) %>%
  filter(weighted.mean.actual!=0) %>%
  mutate("relative"= weighted.mean.actual-weighted.mean.label)

toplot %>%
  summarise(sum(weighted.mean.label*tot)/sum(tot))

toplot %>%
  summarise(sum(weighted.mean.actual*tot)/sum(tot))

toplot[toplot$Genus=="Epinephelus",]$weighted.mean.label <-(4*15+2.031746*189)/(189+4)
toplot[toplot$Genus=="Epinephelus",]$weighted.mean.actual <-(4*15+4.21164*189)/(189+4)

toplot <- toplot %>%
  filter(Genus!="Mycteroperca")



toplot <- cbind(toplot,c("Eel","Croaker","Dolphinfish", "Sea bass", "Grouper", "Cod", "Channel catfish",
               "Skipjack", "Monkfish","Snapper", "Megalodon","Haddock", "Whiting", "Hake",
               "Smooth hound","Smalleye croaker", "Swai", "Flounder","Perch" ,"Plaice",
               "Atlantic salmon", "Croaker", "Amberjack","Tuna","Swordfish"))
names(toplot)[6]<- "Common"
toplot<-toplot %>%
  filter(relative!=0)
toplot$Common <- factor(toplot$Common)
toplot$Common <- reorder(toplot$Common, -toplot$weighted.mean.actual)

arrowPlot <- toplot %>%
  filter(!relative==0)


require(ggplot2)
library(scales)
library(grid)
p <- ggplot(toplot, aes(x=Common, weighted.mean.label, weighted.mean.actual)) +
  geom_segment(aes(x=Common, xend=Common, y=weighted.mean.label, yend=weighted.mean.actual),
               arrow=arrow(length=unit(.3,"cm")), colour="gray") +
  geom_point(data=toplot, aes(x=Common, y=weighted.mean.label,colour=weighted.mean.label), size=5) +
  geom_point(aes(x=Common, y=weighted.mean.actual, colour=weighted.mean.actual), size=5) +
 scale_y_discrete("IUCN status", labels=c("Critically endangered",
                                          "Endangered", "Vulnerable",
                                          "Near threatened", "Least concern", "")) +
  scale_x_discrete("Common name")+
  theme_classic() +
  coord_flip() +
  theme(axis.text = element_text(size=8), axis.title=element_text(size=8),
        legend.text=element_text(size=6), axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title=element_text(size=6)) +
  scale_colour_gradient(low="red", high="green", breaks=-1:5, labels=c('CR',"CR","CR","EN", "VU", "NT", "LC"), "IUCN status")
p


summIUCN <- data.tbl %>% 
  group_by(Status.labels, Status.actuals, Genus) %>%
  summarise("IUCNdiffByGenus"=sum(N))


data.tbl %>%
  group_by(Sci.labels) %>%
  summarise("Prop"=sum(N*Mislabeled)/sum(N), "SampSize"=sum(N)) %>%
  write.csv(file.path(data.dir,"MislabelByLabel.csv"))

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
