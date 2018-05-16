###############################################################
# Read Nephrops fisheries into a FLR stock object
# Created by: Carlos Mesquita
# Date: 30/11/2015
# Created: 31/04/2016
# Packges used: 
# R (3.1.2)
##This new "nephup" function replaces previous function created by
#Neil. This function reads the stock object from the previous year
#and adds new data from txt files (same format as before)
###############################################################

nephup <-
function(wdir, stock.object, lfile, discfile, msfile)
{
  ###################
  ##  Load data    ##
  ###################
  
  #Load previous years stock object
  env <- new.env()
  load(paste0(wdir,stock.object), envir=env, verbose = T)
  obj.name<- ls(env=env)[grep("nephup.", ls(env=env))]
  stock.obj<- env[[obj.name]]
  
  
  ################
  ##  LANDINGS  ##
  ################
  
  gear.names<- dimnames(stock.obj@landings)$landings
  season<- dimnames(stock.obj@landings)$season
  dimnames(stock.obj@landings)$season
  quarterly.landings.data<-read.table(paste(wdir, lfile, sep=""), skip=3, header=F)
  colnames(quarterly.landings.data)<-c("Year", "Quarter", gear.names)
  quarterly.landings.data$Quarter<- gsub("[[:alpha:]]", "", quarterly.landings.data$Quarter)
  landings.start.year <- as.numeric(strsplit(readLines(paste(wdir, lfile, sep=""))[2], split="\t")[[1]][1])
  landings.end.year   <- as.numeric(strsplit(readLines(paste(wdir, lfile, sep=""))[2], split="\t")[[1]][2])
  
  #Expand existing object to the new landings year 
  stock.obj<- suppressWarnings(expand(stock.obj, year=as.numeric(stock.obj@range[names(stock.obj@range) == "minyear"]):landings.end.year))
  
  for(y in as.character(seq(landings.start.year,landings.end.year)))
  {
    for(g in gear.names)
    {
      for(s in season)
      {
        stock.obj@landings[g,y,,s,,]<- quarterly.landings.data[quarterly.landings.data$Year %in% y & quarterly.landings.data$Quarter %in% s,g]
      }
    }
  }
  
  
  ################
  ##  DISCARDS  ##
  ################
  
  discard.files <- readLines(paste(wdir, discfile, sep=""))
  # reads discard index file
  discard.start.year <- as.numeric(strsplit(discard.files[1], " ")[[1]][1])
  discard.end.year   <- as.numeric(strsplit(discard.files[1], " ")[[1]][length(strsplit(discard.files[1], " ")[[1]])])
  # strips start and end year out of discard file
  discard.files<-discard.files[2:length(discard.files)]
  # chops off years from file list
  
  dat<- data.frame(year=as.character(discard.start.year:discard.end.year), discard.files, stringsAsFactors=F)
  season<- dimnames(stock.obj@discards.n)$season
  len.class<- dimnames(stock.obj@discards.n)$lengths
  sex.vector<- dimnames(stock.obj@landings.n)$unit
  
  for(y in dat$year)
  {
    temp <- read.table(paste0(wdir,dat[dat$year==y,"discard.files"]), skip=4, sep="\t", comment.char="", header=T)
    temp<-cbind(temp[,2], temp[,3], temp[,4], temp[,6], temp[,7], temp[,9], temp[,10], temp[,12], temp[,13])
    temp2<- matrix(nrow=length(seq(9,71,by=1)), ncol=ncol(temp))
    temp2[,1]<- seq(9,71,by=1)
    temp2[temp2[,1] %in% temp[,1],2:9]<- temp[,2:9]
    temp2[is.na(temp2)]<- 0
    temp2<- as.data.frame(temp2)
    temp2$vec<- sort(rep(seq(9,71,by=2),2))[-64]
    temp2<- aggregate(list(temp2[,2:9]), list(temp2[,c("vec")]), sum)
    grid<- expand.grid(sex.vector,season)
    colnames(temp2)<- c("L", paste(grid$Var1, grid$Var2, sep="_"))
    
    for(l in len.class)
    {
      for(s in season)
      {
        for(x in sex.vector)
        {
          stock.obj@discards.n[l,y,x,s,,]<- round(temp2[temp2$L==l,paste(x,s,sep="_")]/1000,3)
        }
      }
    }
  }
  
    
  #######################
  ##  MARKET SAMPLING  ##
  #######################
  
  ## reads in the market sampling index file
  sampling.files<- readLines(paste(wdir, msfile, sep=""))
  market.sampling.start.year <- as.numeric(strsplit(sampling.files[2], split=",")[[1]][1])
  market.sampling.end.year <- as.numeric(strsplit(sampling.files[2], split=",")[[1]][2])
  sampling.files<-sampling.files[3:length(sampling.files)]
  
  dat<- data.frame(year=as.character(market.sampling.start.year:market.sampling.end.year), sampling.files, stringsAsFactors=F)
  season<- dimnames(stock.obj@landings.n)$season
  len.class<- dimnames(stock.obj@landings.n)$lengths
  sex.vector<- dimnames(stock.obj@landings.n)$unit
  
  for(y in dat$year)
  {
    temp <- read.table(paste0(wdir,dat[dat$year==y,"sampling.files"]), skip=4, sep="\t", comment.char="", header=T)
    temp<-cbind(temp[,2], temp[,3], temp[,4], temp[,6], temp[,7], temp[,9], temp[,10], temp[,12], temp[,13])
    temp2<- matrix(nrow=length(seq(9,71,by=1)), ncol=ncol(temp))
    temp2[,1]<- seq(9,71,by=1)
    temp2[temp2[,1] %in% temp[,1],2:9]<- temp[,2:9]
    temp2[is.na(temp2)]<- 0
    temp2<- as.data.frame(temp2)
    temp2$vec<- sort(rep(seq(9,71,by=2),2))[-64]
    temp2<- aggregate(list(temp2[,2:9]), list(temp2[,c("vec")]), sum)
    grid<- expand.grid(sex.vector,season)
    colnames(temp2)<- c("L", paste(grid$Var1, grid$Var2, sep="_"))
    
    for(l in len.class)
    {
      for(s in season)
      {
        for(x in sex.vector)
        {
          stock.obj@landings.n[l,y,x,s,,]<- round(temp2[temp2$L==l,paste(x,s,sep="_")]/1000,3)
        }
      }
    }
  }
  
  
  #############################
  ##  Complete stock object  ##
  #############################
   
  #For the new year(s), sum landings and discards to obtain catch.n & get mean wts at length from previous year
  for(y in as.character(seq(landings.start.year,landings.end.year)))
  {
    stock.obj@catch.n[,y,,,,]<- stock.obj@landings.n[,y,,,,] + stock.obj@discards.n[,y,,,,]
    stock.obj@stock.wt[,y,,,,]<- stock.obj@stock.wt[,as.character(as.numeric(y)-1),,,,]
    stock.obj@catch.wt[,y,,,,]<- stock.obj@catch.wt[,as.character(as.numeric(y)-1),,,,]
    stock.obj@landings.wt[,y,,,,]<- stock.obj@landings.wt[,as.character(as.numeric(y)-1),,,,]
    stock.obj@discards.wt[,y,,,,]<- stock.obj@discards.wt[,as.character(as.numeric(y)-1),,,,]
    stock.obj@stock[,y,,,,]<- stock.obj@stock[,as.character(as.numeric(y)-1),,,,]
  }
  
  #Catch and discards biomass (numbers*length)
  stock.obj@catch <- computeCatch(stock.obj)
  stock.obj@discards <- computeDiscards(stock.obj)
  
  #Return new object
  return(stock.obj)  
}
