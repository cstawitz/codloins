
#' @description Function which matches people to restaurants and orders
#' @param city = Chich city is chosen (character string)
#' @param npeeps = number of people dining in that city (int)
#' @return people.df = data.frame which contains person, ordered fish, received fish
#' stock status (overfishing?, overfished?, low data?)
#' @author Christine Stawitz
#' @export
make_consumer_choices<-function(city, npeeps){
  #Match restaurant indices to city
  restIndices<-switch(city,Chi=1:51,LA=52:101,NY=102:151,Hou=152:201)
  #Pick restaurant for each person
  #Populate Person column with person index, Rest column with chosen restaurant
  peopledf<-data.frame(Person=1:npeeps,Rest=sample(restIndices,npeeps,replace=T))
  
  #Create vectors for chosen meal, actual fish customer receives, stock status
  meal<-realfish<-overfishing<-overfished<-low.data<-rep(NA,npeeps)
  #Pick meal for each person randomly from seafood choices
  #TODO: vectorize this
  for(i in 1:nrow(peopledf)){meal[i]<-pick_meal(peopledf$Rest[i])}
  peopledf<-cbind(peopledf,meal,realfish,overfishing,overfished,low.data)
  
  #Replace stock numbers with names and pick true fish
  stockNames<-c("Red Snapper","Tuna","Salmon","Atlantic Cod")
  for(i in 3:6){
    peopledf$meal[peopledf$meal==i]<-stockNames[i-2]
    peeps<-which(peopledf$meal==stockNames[i-2])
    peopledf$realfish[peeps]<-get_true_fish(stockNames[i-2],length(peeps))
  }
  
  #Match up true fish to stock status data
  for(i in 1:npeeps){
    if(!is.na(peopledf$realfish[i])){
      indices<-which(str_detect(stock.status[,1],ignore.case(peopledf$realfish[i])))
      #If only one stock, use that status
      if(length(indices)==1){
        peopledf$overfishing[i]<-stock.status$Overfishing[indices]
        peopledf$overfished[i]<-stock.status$Overfished[indices]
        peopledf$low.data[i]<-stock.status$Low.data[indices]
      }
      #If more than one stock, randomly sample one
      if(length(indices)>1){
        
        ind.to.use<-sample(indices,1)
        peopledf$overfishing[i]<-stock.status$Overfishing[ind.to.use]
        peopledf$overfished[i]<-stock.status$Overfished[ind.to.use]
        peopledf$low.data[i]<-stock.status$Low.data[ind.to.use]
      }
    }
  }
  return(peopledf)
  
}