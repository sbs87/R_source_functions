calc_stats<-function(qpcr){
  species<-unique(qpcr$species)
  time<-unique(qpcr$time_text)
  time.num<-unique(qpcr$time_number)
  num.species<-length(species)
  num.time<-length(time)
  num.stats<-4
  
  #Defin the stats df, which will hold the statistics calculated. Note that species and time_text must be maintained as factors, but the remianders must not be factors in order for proper plotting!
  stats<-data.frame(cbind(rep(as.character(species),each=num.time),rep(as.character(time),times=num.species)))
  stats<-data.frame(stats,matrix(rep(time.num,times=num.species)))#The only way i could figure out how to isolate factors from non factors
  stats<-data.frame(stats,matrix(NA,nrow=num.time*num.species,ncol=num.stats))
  names(stats)<-c("species","time_text","time_number","mean_artitmetic","mean_geometric","stdev_artitmetic","stdev_geometric")#Species and time_text only become a factor after this naming
  
  #For each species and time point, find stats, i.e., artihmetic and geometric means and both sets of stdev.
  for(s in species){
    x<-qpcr[qpcr$species==s,]
    for(t in time){
      y<-x[x$time_text==t,]
      u.art<-mean(y$copy_num,na.rm=T) #Artihmatic mean
      u.geo<-mean(y$log.copy_num,na.rm=T) #Geometric mean
      s.art<-arth.stdev(y$copy_num) #Artihmatic standard deviation (n-1)
      s.geo<-arth.stdev(y$log.copy_num,u.geo) #Geometric standard deviation
      stats[stats$species==s & stats$time_text==t,]$mean_artitmetic<-u.art
      stats[stats$species==s & stats$time_text==t,]$mean_geometric<-u.geo
      stats[stats$species==s & stats$time_text==t,]$stdev_artitmetic<-s.art
      stats[stats$species==s & stats$time_text==t,]$stdev_geometric<-s.geo
      
    }
  }
  return(stats)
}

#COMMENT THIS
relabund<-function(mean.species.copynum,mean.panbac.copynum){
  #mean.species.copynum<-species.sub
  #mean.panbac.copynum<-pb.sub
  
  time<-as.character(unique(mean.species.copynum$time))
  species<-unique(mean.species.copynum$species)

  rel.copynum<-matrix(rep(0,times=length(time)*length(species)),nrow=length(time)*length(species))
  time.numeric<-as.numeric(as.character(rep(time,times=length(species))))
  rel.copynum<-data.frame(rep(as.character(species),each=length(time)),time.numeric,rel.copynum)
  names(rel.copynum)<-c("species","time","relative_abundance")
  
  for(s in species){

    species.copynum<-mean.species.copynum[mean.species.copynum$species==s,]
    for(t in time){

      sp.cn<-species.copynum[species.copynum$time==t,3]
      pb.cn<-mean.panbac.copynum[mean.panbac.copynum$time==t,2]
      
      rel.copynum[rel.copynum$species==s & rel.copynum$time==t,3]<-sp.cn/pb.cn
    }
  }
  
  return(rel.copynum)
}

