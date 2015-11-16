#' @description Function which chooses a meal based on city
#' @param X = person index
#' @param City = string for which city is used
#' @return meal = index of fish selected by customer
#' @author Christine Stawitz
#' @export
pick_meal<-function(City, prefs){
  #If there is no region-specific data, use the same probabilities for all people
  if(City=="USA"){
    probs <- prefs
  }else{
    #Probability of each fish is based on NHANES data
    #Vector of probabilities for each city, where rows are seafood type
    probs <- prefs[,City]
  }
    if(sum(probs$mle)==1){
      p <- runif(1,0,1)
      
      if(City!='USA'){
        #Need to normalize the probabilities which are present on the restaurant menu to 1
        #Need to deprecate this probably
        seafood.options <- which(restaurants[X,]==1)-2
        mult <- 1/sum(probs[seafood.options])
        prob.subset <- mult*probs[seafood.options]
      }else{
        prob.subset <- probs
      }
      probs.vect<-prob.subset$mle
      #Make into cumulative probabilities
      for(i in 2:nrow(prob.subset)){
        probs.vect[i]<-sum(probs.vect[c(i-1,i)])
      }
      meal <- which(p<probs.vect)[1]
      if(meal %in% c(1,5,6)){meal <- sample(2:5,1)}
    }else{
      print(paste("Error! Probabilities for city",City,"sum to",sum(probs),"and should sum to 1."))
      break
    }
  #Return chosen fish

  return(meal)
}
