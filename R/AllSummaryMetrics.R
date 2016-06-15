get_summary_price <-function(data.set){
  price.per.genus <- data.set %>%
    group_by(Genus) %>%
    summarise('percent'=sum((price_actual/price_label)*N, na.rm=T)/sum(N),'tot'=sum(N)) %>%
    filter(tot>=10)
  
  price_summary <- price.per.genus %>%
    summarise(sum(percent*tot)/sum(tot))
  
  return(price_summary)
}

get_summary_IUCN <- function(data.set){
  by_genus <- data.set %>%
    group_by(Genus) %>%
    mutate(numerator=sum(N*Mislabeled)) %>%
    mutate("weighted.mean"=numerator/sum(N), "tot.N" = sum(N))  %>%
    filter("tot.N">=10)
  IUCN <- by_genus %>% sum(weighted.mean*tot.N)/sum(tot.N)
  
  return(IUCN)
}

get_summary_IUCN <- function(data.set){
  tot.prop <- data.set %>%
    summarise("prop"=sum(N*Mislabeled),sum(N))
  return(tot.prop)
}

get_summary_diversity <- function(data.set){
  
}