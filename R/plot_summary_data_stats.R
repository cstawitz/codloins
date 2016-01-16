plotSummaryData <- function(data.table){
  #Plot world map of number of studies
  makeSampleMap(data.table, "WorldSampSizeMap.pdf")
} 

makeSampleMaps <- function(data.table, filename){
  require("rworldmap")
  require("maptools")
  require("RColorBrewer")
  isos <- c("ZAF", "USA", "ITA", "ESP", "GRC", "CAN", "GBR", "GBR","PRT", "IRL", "DEU", "FRA")
  #Find each study that contains each code
  isoVect <- matrix(rep("", nrow(percentage.table)*(length(isos))),nrow=nrow(percentage.table),ncol=length(isos))
  totSamp <- rep(0,ncol(isoVect))
  for(i in 1:length(as.character(unique(percentage.table$Country.of.sample)[-c(7,9,15,16)]))){
    matchInd <- grep(as.character(unique(percentage.table$Country.of.sample))[i],percentage.table$Country.of.sample)
    isoVect[matchInd,i]<-rep(1,length(matchInd))
    totSamp[i] <- sum(as.numeric(isoVect[matchInd,i])*percentage.table$N.samples.with.this.label.in.this.study.[matchInd], na.rm=TRUE)
  }
  tblIso <- cbind(percentage.table,isoVect)
  names(tblIso) <- c(names(percentage.table),isos)
  totState <- rep(0,ncol(tblIso))
  locations <- as.character(unique(tblIso$Location.of.sample))
  states <- unlist(unique(regmatches(locations,regexec('[A-Z]{2}',locations)))[-1])
  stateVect <- matrix(rep("", nrow(tblIso)*length(states)),nrow=nrow(tblIso),ncol=length(states))
  totState <- rep(0,ncol(stateVect))
  for(j in 1:length(states)){
    matchInd <- grep(states[j],as.character(tblIso$Location.of.sample))
    stateVect[matchInd,j]<-rep(1,length(matchInd))
    totState[j] <- sum(as.numeric(stateVect[matchInd,j])*percentage.table$N.samples.with.this.label.in.this.study.[matchInd],na.rm=TRUE)
  }
  tblPlot <- cbind(tblIso, stateVect)
  names(tblPlot) <-c(names(tblIso),states)
  isoTot <- cbind(isos,as.numeric(totSamp))
  isoTot <- as.data.frame(rbind(isoTot[-c(7,8),],c("GBR",sum(as.numeric(isoTot[c(7,8),2])))))
  stateTot <- as.data.frame(cbind(states,as.numeric(totState)))
  isoTot$V2 <- as.integer(levels(isoTot$V2))[isoTot$V2]
  stateTot$V2 <- as.integer(levels(stateTot$V2))[stateTot$V2]
  isoTot <- isoTot[-2,]
  colVect <- pretty(c(stateTot$V2,isoTot$V2))
  mypal<-brewer.pal(5,"Greens")
  spdf <- joinCountryData2Map(isoTot, joinCode="ISO3", nameJoinColumn="isos")
  US <- readShapePoly(file.path(data.dir,"states_21basic","states.shp"))
  spdf2 <- joinData2Map(stateTot,nameMap=US, nameJoinIDMap="STATE_ABBR",nameJoinColumn="states")
  mapCountryData(spdf, nameColumnToPlot="V2", catMethod=colVect, colourPalette=mypal, mapTitle="Sample Size By Location")
  mapPolys(spdf2, nameColumnToPlot="V2", catMethod=colVect, add=TRUE, colourPalette=mypal, mapTitle="")

  }





