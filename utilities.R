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

#Taken fromSOURCE this saves the file to disk
ExportPlot <- function(gplot, filename, width=2, height=1.5) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}