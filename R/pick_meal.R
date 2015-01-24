#' @description Function which chooses a meal based on selected restaurant
#' @param X = chosen restaurant index
#' @return meal = index of fish selected by customer
#' @author Christine Stawitz
#' @export
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
