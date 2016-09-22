get_summary_price <-function(data.set){
  price.per.genus <- data.set %>%
    group_by(Genus) %>%
    summarise('percent'=sum((price_actual/price_label)*N, na.rm=T)/sum(N),'tot'=sum(N)) %>%
    filter(tot>=10)
  
  price_summary <- price.per.genus %>%
    summarise("percent"=sum(percent*tot)/sum(tot))
  
  return(price_summary$percent)
}

get_summary_RAM_qual <-function(data.set){
  RAM.qual <- data.set %>%
    mutate("OF.lab"=ifelse(RAM.U.lab>1, 1,2), "B.lab"=ifelse(RAM.B.lab>1, 1,2),
           "OF.act"=ifelse(RAM.U.act>1, 1,2), "B.act"=ifelse(RAM.B.act>1, 1,2)) %>%
    summarise('percentU'=sum((OF.act-OF.lab)*N, na.rm=T)/sum(N),
              'percentB'=sum((B.act-B.lab)*N, na.rm=T)/sum(N), 'tot'=sum(N)) %>%
    filter(tot>=10)
  
  return(RAM.qual)
}

get_summary_RAM_quant <- function(data.set){
  RAM.summ<- data.set %>%
    summarise('percentU'=sum((RAM.U.act/RAM.U.lab)*N, na.rm=T)/sum(N),
              'percentB'=sum((RAM.B.act/RAM.B.lab)*N, na.rm=T)/sum(N), 'tot'=sum(N)) %>%
    filter(tot>=10)
  return(RAM.summ)
}

get_mislabeled <- function(data.set){
  by_genus <- data.set %>%
    summarise("weighted.mean"=(sum(N*Mislabeled))/sum(N), "tot.N" = sum(N))  %>%
    filter("tot.N">=10)
  return(by_genus$weighted.mean)
}

match_RAM <- function(RAMdb, data.set){
  for(i in 1:nrow(data.set)){
    if(data.set$Sci.labels[i]=="Argyrops spinifer"){
      lab <- data.set$Sci.labels[i]
    } else{
      lab <- sub(" sp| spp| sp.| spp.", "", data.set$Sci.labels[i])
    }

    if(data.set$Sci.actuals[i]=="Argyrops spinifer"){
      act <- data.set$Sci.actuals[i]
    } else{
      act <-sub(" sp| spp| sp.| spp.", "", data.set$Sci.actuals[i])
    }
    yr <- data.set$year[i]
    match.lab <- filter(RAMdb, (sci.name==lab)) 
    if(nrow(match.lab)>0){
      if(any(match.lab$year==yr)){
        match.lab.yr <- filter(match.lab, year==yr)
        } else{
          if(all(match.lab$year<yr)){
            upper <- max(match.lab$year)
            match.lab.yr <- filter(match.lab, year==upper)
          } 
        }
          
        if(nrow(match.lab.yr)==1){
          data.set$RAM.B.lab[i] <- match.lab.yr$B.Bmsy
          data.set$RAM.U.lab[i] <- match.lab.yr$U.Umsy
        } else{
          noNAB <- filter(match.lab.yr, !is.na(B.Bmsy))
          noNAU <- filter(match.lab.yr, !is.na(U.Umsy))
          if(sum(!is.na(noNAB$weight.SSB))>1){
            numNA <- sum(is.na(noNAB$weight.SSB))
            noNAB$weight.SSB[is.na(noNAB$weight.SSB)] <- rep(1/numNA, numNA)
            data.set$RAM.B.lab[i] <- sum(noNAB$B.Bmsy*noNAB$weight.SSB)/sum(noNAB$weight.SSB)
          } else{
            if(any(!is.na(noNAB$weight.TB))){
              numNA <- sum(is.na(noNAB$weight.TB))
              noNAB$weight.TB[is.na(noNAB$weight.TB)] <- rep(1/numNA, numNA)
            data.set$RAM.B.lab[i] <- sum(noNAB$B.Bmsy*noNAB$weight.TB)/sum(noNAB$weight.TB)
            } else{
              data.set$RAM.B.lab[i] <- mean(noNAB$B.Bmsy)
            }
          }
          if(sum(!is.na(noNAU$weight.SSB))>1){
            numNA <- sum(is.na(noNAU$weight.SSB))
            noNAU$weight.SSB[is.na(noNAU$weight.SSB)] <- rep(1/numNA, numNA)
            data.set$RAM.U.lab[i] <- sum(noNAU$U.Umsy*noNAU$weight.SSB)/sum(noNAU$weight.SSB)
          } else{
            if(any(!is.na(noNAU$weight.TB))){
              numNA <- sum(is.na(noNAU$weight.TB))
              noNAU$weight.TB[is.na(noNAU$weight.TB)] <- rep(1/numNA, numNA)
              data.set$RAM.U.lab[i] <- sum(noNAU$U.Umsy*noNAU$weight.TB)/sum(noNAU$weight.TB)
            } else{
              data.set$RAM.U.lab[i] <- mean(noNAU$U.Umsy[!is.na(noNAU$U.Umsy)])
            }
          }
        }
      } 
    else{
      data.set$RAM.B.lab[i] <- NA
      data.set$RAM.U.lab[i] <- NA
    }
    
    match.act <- filter(RAMdb, (sci.name==act)) 
    if(nrow(match.act)>0){
      if(any(match.act$year==yr)){
        match.act.yr <- filter(match.act, year==yr)
      } else{
        if(all(match.act$year<yr)){
          upper <- max(match.act$year)
          match.act.yr <- filter(match.act, year==upper)
        } 
      }
      
      if(nrow(match.act.yr)==1){
        data.set$RAM.B.act[i] <- match.act.yr$B.Bmsy
        data.set$RAM.U.act[i] <- match.act.yr$U.Umsy
      } else{
        noNAB <- filter(match.act.yr, !is.na(B.Bmsy))
        noNAU <- filter(match.act.yr, !is.na(U.Umsy))
        if(sum(!is.na(noNAB$weight.SSB))>1){
          numNA <- sum(is.na(noNAB$weight.SSB))
          noNAB$weight.SSB[is.na(noNAB$weight.SSB)] <- rep(1/numNA, numNA)
          data.set$RAM.B.act[i] <- sum(noNAB$B.Bmsy*noNAB$weight.SSB)/sum(noNAB$weight.SSB)
        } else{
          if(any(!is.na(noNAB$weight.TB))){
            numNA <- sum(is.na(noNAB$weight.TB))
            noNAB$weight.TB[is.na(noNAB$weight.TB)] <- rep(1/numNA, numNA)
            data.set$RAM.B.act[i] <- sum(noNAB$B.Bmsy*noNAB$weight.TB)/sum(noNAB$weight.TB)
          } else{
            data.set$RAM.B.act[i] <- mean(noNAB$B.Bmsy)
          }
        }
        if(sum(!is.na(noNAU$weight.SSB))>1){
          numNA <- sum(is.na(noNAU$weight.SSB))
          noNAU$weight.SSB[is.na(noNAU$weight.SSB)] <- rep(1/numNA, numNA)
          data.set$RAM.U.act[i] <- sum(noNAU$U.Umsy*noNAU$weight.SSB)/sum(noNAU$weight.SSB)
        } else{
          if(any(!is.na(noNAU$weight.TB))){
            numNA <- sum(is.na(noNAU$weight.TB))
            noNAU$weight.TB[is.na(noNAU$weight.TB)] <- rep(1/numNA, numNA)
            data.set$RAM.U.act[i] <- sum(noNAU$U.Umsy*noNAU$weight.TB)/sum(noNAU$weight.TB)
          } else{
            data.set$RAM.U.act[i] <- mean(noNAU$U.Umsy[!is.na(noNAU$U.Umsy)])
          }
        }
      }
    } 
    else{
      data.set$RAM.B.act[i] <- NA
      data.set$RAM.U.act[i] <- NA
    }
  }
  
  return(data.set)
}

get_num_status <- function(colum){
  if(!is.na(colum)){ num <- switch(colum, EX=-1,
                                   CR = 0, EN = 1, VU = 2, NT = 3, LC = 4,
                                   "not evaluated" = NA, DD = NA)
  } else{
    num <- NA
  }
  return(num)
}

get_summary_IUCN <- function(data.set){
  IUCN.diff <- data.set %>%
    summarise("prop"=sum(N*(Actual.num-Label.num), na.rm=T)/sum(N,na.rm=T))
  return(IUCN.diff$prop)
}

get_summary_FAO <- function(data.set){
  FAO.diff <- data.set %>%
    summarise("prop"=sum(N*(Fao.actual-Fao.label), na.rm=T)/sum(N,na.rm=T))
  return(FAO.diff$prop)
}

get_summary_diversity <- function(data.set){
  library(reshape)
  library(vegan)
  dat <- data.set %>%
    group_by(Sci.labels) %>%
    summarise(tot=sum(N)) %>%
    as.data.frame()
  maus <- cast(dat,~Sci.labels)
  maus <- maus[,-1]
  H <- diversity(maus)
  
  dat <- data.set %>%
    group_by(Sci.actuals) %>%
    summarise(tot=sum(N)) %>%
    as.data.frame()
  taus <- cast(dat,~Sci.actuals)
  taus <- taus[,-1]
  I <- diversity(taus)
  return(H-I)
}

plot_summ_stat <- function(summstats){
  par(oma=c(0,4,0,0))
  plot(summstats$Price, rep(1,nrow(summstats)), axes=F, xlab="Summary metric", ylab="",
       ylim=c(0,3.2), xlim=c(0,.6), type="l")
  lines(summstats$Mislabel,rep(2,nrow(summstats)))
  lines(summstats$IUCN,rep(3,nrow(summstats)))
  axis(1)
  axis(2, labels=c("Price", "Mislabel proportion", "IUCN status"), at=c(1,2,3), las=1)
  points(overall,c(1,2,3),col="red", pch=20)
}