#' @description Function which chooses a meal based on selected restaurant
#' @param X = chosen restaurant index
#' @param City = string for which city is used
#' @return meal = index of fish selected by customer
#' @author Christine Stawitz
#' @export
pick_meal<-function(X, City){
  #If there is no fish on the menu, return NA since they did not pick seafood option
  if(length(which(restaurants[X,]==1))==0){
    meal<-NA
  }
  #Otherwise, if there is only one fish item, set that as the chosen meal
  else if(length(which(restaurants[X,]==1))==1){
    meal<-which(restaurants[X,]==1)
  }else{
    ##If there are multiple fish items on the menu, sample one randomly with equal probability
    #meal<-sample(which(restaurants[X,]==1),1)
    #If there are multiple fish items on the menu, sample them based on NHANES data
    #Vector of probabilities for each city, where rows are seafood type
    probs<-NHANES[,City]
    #Check that probs sum to 1
    if(sum(probs)==1){
      p <- runif(1,0,1)
      
      #Need to normalize the probabilities which are present on the restaurant menu to 1
      seafood.options <- which(restaurants[X,]==1)-2
      mult <- 1/sum(probs[seafood.options])
      prob.subset <- mult*probs[seafood.options]
      
      #Make into cumulative probabilities
      for(i in 2:length(prob.subset)){
        prob.subset[i]<-sum(prob.subset[c(i-1,i)])
      }
      meal <- which(p<prob.subset)[1]+2
    }else{
      print(paste("Error! Probabilities for city",City,"sum to",sum(probs),"and should sum to 1."))
      break
    }
  }
  #Return chosen fish 
  return(meal)
}
