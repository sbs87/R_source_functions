#General Functions

#Arthimetic Standard Deviation
#Computes the stdandard deviation given an input vector. optional is the mean, which is calculated by default using R's built in mean function

arth.stdev<-function(log.cts,u=mean(log.cts,na.rm=TRUE)){
  n<-sum(!is.na(log.cts))
  if(sum(is.na(log.cts))==length(log.cts)){
    return(NaN)
  }
  else{
    sigma<-sqrt(sum((log.cts-u)^2,na.rm=TRUE)/(n-1))
    return(sigma)
  }
}
