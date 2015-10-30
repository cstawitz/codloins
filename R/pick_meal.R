#' @description Function which chooses a meal based on city
#' @param X = person index
#' @param City = string for which city is used
#' @return meal = index of fish selected by customer
#' @author Christine Stawitz
#' @export
pick_meal<-function(X, City){
  #If there is no region-specific data, use the same probabilities for all people
  if(City=="USA"){
    probs <- NHANES
  }else{
    #Probability of each fish is based on NHANES data
    #Vector of probabilities for each city, where rows are seafood type
    probs <- NHANES[,City]
  }
    if(sum(probs)==1){
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
      
      #Make into cumulative probabilities
      for(i in 2:length(prob.subset)){
        prob.subset[i]<-sum(prob.subset[c(i-1,i)])
      }
      meal <- which(p<prob.subset)[1]+2
    }else{
      print(paste("Error! Probabilities for city",City,"sum to",sum(probs),"and should sum to 1."))
      break
    }
  #Return chosen fish 
  return(meal)
}
