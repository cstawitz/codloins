
#' @description Function which chooses the true fish based on the menu-listed fish that was ordered
#' @param labelname = ordered fish (char?)
#' @param numPeople = number of people who ordered that fish by label (int)
#' @return true.fish = character vector of stock names given to customers
#' @author Christine Stawitz
#' @export
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
  browser()
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
