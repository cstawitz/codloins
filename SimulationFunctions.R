#' @description Function which chooses a meal based on selected restaurant
#' @param X = chosen restaurant index
#' @return meal = index of fish selected by customer
pick_meal<-function(X){
  #If there is no fish on the menu, return NA since they did not pick seafood option
  if(length(which(restaurants[X,]==1))==0){
    meal<-NA
  }
  #Otherwise, if there is only one fish item, set that as the chosen meal
  else if(length(which(restaurants[X,]==1))==1){
    meal<-which(restaurants[X,]==1)
  }else{
    #If there are multiple fish items on the menu, sample one randomly with equal probability
    meal<-sample(which(restaurants[X,]==1),1)
  }
  #Return chosen fish 
  return(meal)
}

#' @description Function which chooses the true fish based on the menu-listed fish that was ordered
#' @param labelname = ordered fish (char?)
#' @param numPeople = number of people who ordered that fish by label (int)
#' @return true.fish = character vector of stock names given to customers
get_true_fish<-function(labelname,numPeople){
  #Get probabilities that each ordered fish is each true stock
  probs<-percentage.table[percentage.table[,1]==labelname,7:13]
  
  #Get minimum and maximum probs from percentage data
  minp<-maxp<-p<-rep(0,nrow(probs))
  minp<-apply(probs,1,min,na.rm=T)
  maxp<-apply(probs,1,max,na.rm=T)
  #print(minp)
  
  #Select fish based on probabilities
  #This draws a random uniform number between 0,1
  for(i in 1:nrow(probs)){
    p[i]<-runif(1,minp[i],maxp[i])
  }
  
  #Draw multinomial samples of how many people get each type of fish
  #dependent upon drawn random probability
  num.fish<-rmultinom(1,numPeople,p)
  
  #Actual stock names
  actual<-as.character(percentage.table[percentage.table[,1]==labelname,2])
  true.fish<-rep(actual,num.fish)
  #print(true.fish)
  
  #Return stock names of actual fish
  return(true.fish)
}

#' @description Function which matches people to restaurants and orders
#' @param city = Chich city is chosen (character string)
#' @param npeeps = number of people dining in that city (int)
#' @return people.df = data.frame which contains person, ordered fish, received fish
#' stock status (overfishing?, overfished?, low data?)
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