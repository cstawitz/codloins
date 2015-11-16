
#' @description Function which matches people to restaurants and orders
#' @param city = Chich city is chosen (character string)
#' @param npeeps = number of people dining in that city (int)
#' @return people.df = data.frame which contains person, ordered fish, received fish
#' stock status (overfishing?, overfished?, low data?)
#' @author Christine Stawitz
#' @export
make_consumer_choices<-function(city, npeeps, prefs){
  #Match restaurant indices to city
  restIndices<-switch(city,Chi=1:51,LA=52:101,NY=102:151,Hou=152:201,USA=1:200)
  #Pick restaurant for each person
  #Populate Person column with person index, Rest column with chosen restaurant
  peopledf<-data.frame(Person=1:npeeps) #,Rest=sample(restIndices,npeeps,replace=T))
  
  #Create vectors for chosen meal, actual fish customer receives, stock status
  meal<-realfish<-rep(NA,npeeps)
  realstock<-rep(0,npeeps)
  #Pick meal for each person randomly from seafood choices
  #TODO: vectorize this
  #browser()
  for(i in 1:nrow(peopledf)){meal[i]<-pick_meal(city, prefs)}
  peopledf<-cbind(peopledf,meal,realfish,realstock)
  #Replace stock numbers with names and pick true fish
  stockNames<-c("","Tuna","Atlantic Cod","Salmon","Red Snapper")
  for(i in 2:5){
    peopledf$meal[peopledf$meal==i]<-stockNames[i]
    peeps<-which(peopledf$meal==stockNames[i])
    peopledf$realfish[peeps]<-get_true_fish(stockNames[i],length(peeps))
  }
 #Match up true fish to stock status data
  for(j in 1:npeeps){
    if(!is.na(peopledf$realfish[j])){
      indices<-grep(stock.status[,"Species"], pattern=peopledf$realfish[j], ignore.case=TRUE)
      #If only one stock, use that status
      if(length(indices)==1){
        peopledf[j,4]<-indices
      } else{
        p <- runif(1,0,1)
        probs <- stock.status$Landings[indices]
        for(k in 2:length(probs)){
          probs[k] <- sum(probs[c(k-1,k)])
        }
        peopledf[j,4] <- indices[which(probs>p)[1]]
      }
    }
  }
  return(peopledf)
}