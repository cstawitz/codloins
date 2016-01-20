plotSummaryData <- function(data.table){
  #Plot world map of number of studies
  makeSampleMap(data.table, "WorldSampSizeMap.pdf")
} 

makeSampleMaps <- function(data.table, filename){
  require("rworldmap")
  require("maptools")
  require("RColorBrewer")
  isos <- c("ZAF", "HKG","USA","PRT", "CHN","CAN","GBR",
          "ITA", "ESP", "FRA","AUS","GRC","BRA", "IRL","GBR", "IRN","DEU", "EGY","IND")
  #Find each study that contains each code

  isoVect <- matrix(rep("", nrow(data.table)*(length(isos))),nrow=nrow(data.table),ncol=length(isos))
  totSamp <- rep(0,ncol(isoVect))
  matching.names <- as.character(unique(data.table$Country.of.sample)[-c(2,15,17,18,22)])
  for(i in 1:length(matching.names)){
    matchInd <- grep(matching.names[i],data.table$Country.of.sample)
    isoVect[matchInd,i]<-rep(1,length(matchInd))
    totSamp[i] <- sum(as.numeric(isoVect[matchInd,i])*data.table$N[matchInd], na.rm=TRUE)
  }
  tblIso <- cbind(data.table,isoVect)
  names(tblIso) <- c(names(data.table),isos)
  totState <- rep(0,ncol(tblIso))
  locations <- as.character(unique(tblIso$Loc))
  states <- unique(unlist(regmatches(locations,gregexpr('[A-Z]{2}',locations))))
  stateVect <- matrix(rep("", nrow(tblIso)*length(states)),nrow=nrow(tblIso),ncol=length(states))
  totState <- rep(0,ncol(stateVect))
  for(j in 1:length(states)){
    matchInd <- grep(states[j],as.character(tblIso$Loc))
    stateVect[matchInd,j]<-rep(1,length(matchInd))
    totState[j] <- sum(as.numeric(stateVect[matchInd,j])*data.table$N[matchInd],na.rm=TRUE)
  }
  tblPlot <- cbind(tblIso, stateVect)
  names(tblPlot) <-c(names(tblIso),states)
  isoTot <- cbind(isos,as.numeric(totSamp))
  isoTot <- as.data.frame(rbind(isoTot[-c(7,15),],c("GBR",sum(as.numeric(isoTot[c(7,15),2])))))
  stateTot <- as.data.frame(cbind(states,as.numeric(totState)))
  isoTot$V2 <- as.integer(levels(isoTot$V2))[isoTot$V2]
  isoTot <- isoTot[-3,]
  stateTot$V2 <- as.integer(levels(stateTot$V2))[stateTot$V2]
  colVect <- c(0,10,50,200,400,1000,1500)
  mypal<-brewer.pal(6,"Greens")
  spdf <- joinCountryData2Map(isoTot, joinCode="ISO3", nameJoinColumn="isos")
  US <- readShapePoly(file.path(data.dir,"states_21basic","states.shp"))
  spdf2 <- joinData2Map(stateTot,nameMap=US, nameJoinIDMap="STATE_ABBR",nameJoinColumn="states")
  mapCountryData(spdf, nameColumnToPlot="V2", catMethod=colVect, colourPalette=mypal, mapTitle="Sample Size By Location")
  mapPolys(spdf2, nameColumnToPlot="V2", catMethod=colVect, add=TRUE, colourPalette=mypal, mapTitle="")

  }





