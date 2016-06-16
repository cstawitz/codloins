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
  
}