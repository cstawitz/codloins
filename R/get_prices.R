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