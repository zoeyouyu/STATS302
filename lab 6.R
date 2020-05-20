qval.pi0<-function(pvals, pi0=1){
  #pvals should not have missing values
  #default value of pi0 is 1, but another can be specified.
  ord<-order(pvals, decreasing = TRUE)
  pord<-pvals[ord]
  answer.ord<-rep(NA,length(pvals) )
  answer<-rep(NA, length(pvals))
  denominator<-sum(pord <= pord[1]) #number of discoveries using pord[i] as the threshold
  numerator<- pord[1]*length(pord)*pi0  #expected number of false discoveries
  answer.ord[1]<-numerator/denominator
  for(i in 1: length(pord)){
    denominator<-sum(pord <= pord[i])
    numerator<- pord[i]*length(pord)*pi0
    if( numerator/denominator <= min(answer.ord[1:(i-1)]) ) answer.ord[i]<-numerator/denominator
    if( numerator/denominator > min(answer.ord[1:(i-1)]) ) answer.ord[i]<- min(answer.ord[1:(i-1)])
  }
  answer[ord]<-answer.ord
  return(answer)
}