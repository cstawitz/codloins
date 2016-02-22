#' @description Takes input messy data and consolidates common names and international names into generic labels
#'
clean_up_labels <- function(name.col, food.words, foreign.names, true.name){
  #Get rid of things describing preparation only
  labels.fish <- as.character(name.col)
  new.labels <- labels.fish
  for(i in 1:length(food.words)){
    new.labels <- sub(food.words[i], "", new.labels, ignore.case=TRUE)
  }
  for(i in 1:nrow(foreign.names)){
    new.labels <- sub(foreign.names[i,1], foreign.names[i,2], new.labels, ignore.case=TRUE)
  }
  x <- gregexpr("[[:upper:]][[:lower:]]+ [[:lower:]]+", c(new.labels,true.name))
  possible.sci.names <- unlist(unique(regmatches(c(new.labels,true.name),x)))
  possible.sci.names <- possible.sci.names[-grep("Alaska|Pacific|Atlantic|Africa|Yellow|Russia|Canada|tuna|told|Black|blue|white|yellow|cape",possible.sci.names, ignore.case=TRUE)]
  real.sci.names <- possible.sci.names[-c(15,17,20,25,28,31,33,40,43,47,49,54,56,61,64,66,67,69,
                                          71,80,91,94,95,101,102,103,104,106,109:124,144:149,151,
                                          159,160,162,164,167:170,183,190:216,242,243,279:284,286,289,
                                          293,295,297:299,315,316,318:323,343)]
  scis <- unique(real.sci.names)
 # write.csv(unique(real.sci.names),"SciNames1.csv")

  sci.names.matched <- sci.actuals.matched <- rep("",length(new.labels))
  for(i in 1:length(scis)){
    inds<-grep(scis[i], new.labels, ignore.case=TRUE)
    sci.names.matched[inds] <- rep(scis[i],length(inds))
    inds.actual<-grep(scis[i], true.name, ignore.case=TRUE)
    sci.actuals.matched[inds.actual] <- rep(scis[i],length(inds.actual))
  }
  return(list("fixed.labels"=new.labels,"sci.labels"=sci.names.matched,"sci.actuals"=sci.actuals.matched))
}

get_mislabeled <- function(data.table){
  w.out.sp.label <- sub("sp|spp|sp.|spp.","",data.table$Sci.labels)
  w.out.sp.actual <- sub("sp|spp|sp.|spp.","",data.table$Sci.actuals)
  mislabeled <- rep(1,nrow(data.table))
  for(i in 1:nrow(data.table)){
    if((length(w.out.sp.label[i])==0)||(length(w.out.sp.actual[i])==0)||is.na(w.out.sp.actual[i])||is.na(w.out.sp.label[i])){
      mislabeled[i]<-NA
    } else{
    if(grepl(w.out.sp.label[i],w.out.sp.actual[i],ignore.case=TRUE)||grepl(w.out.sp.actual[i],w.out.sp.label[i],ignore.case=TRUE)){
      mislabeled[i]<-0
    }
    }
  }
  return(mislabeled)
}

get_n_per_label <- function(data.table){
  grouped <- group_by(data.table,Study,Sci.labels)
  PerLabelPerStudy<- ungroup(summarise(grouped, N.per.lab=sum(N)))
  return(left_join(x=data.table,y=PerLabelPerStudy,by=c("Study","Sci.labels")))
}

get_mislabeling_prob <- function(data.table){
  return(mutate(data.table,Prob=N/N.per.lab.y))
}


