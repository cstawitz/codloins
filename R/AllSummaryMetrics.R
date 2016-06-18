get_summary_price <-function(data.set){
  price.per.genus <- data.set %>%
    group_by(Genus) %>%
    summarise('percent'=sum((price_actual/price_label)*N, na.rm=T)/sum(N),'tot'=sum(N)) %>%
    filter(tot>=10)
  
  price_summary <- price.per.genus %>%
    summarise("percent"=sum(percent*tot)/sum(tot))
  
  return(price_summary$percent)
}

get_mislabeled <- function(data.set){
  by_genus <- data.set %>%
    summarise("weighted.mean"=(sum(N*Mislabeled))/sum(N), "tot.N" = sum(N))  %>%
    filter("tot.N">=10)
  return(by_genus$weighted.mean)
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