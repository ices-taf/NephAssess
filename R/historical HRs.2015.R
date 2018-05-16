#changed 13/06/2015 to add dead discards weight to the summary table (CM)

exploitation.table.2015 <-
function(wk.dir, stock, f.u,stock.object, international.landings, survey) 
{
    f.u <- check.fu (f.u)
    
    survey.data.all<- read.csv(survey)
    year.ran <-survey.data.all$year
	  int.landings.all<- subset(read.csv(international.landings), Year %in% year.ran)$Total
    names(int.landings.all) <-subset(read.csv(international.landings), Year %in% year.ran)$Year

#    if(length(int.landings.all)==(length(survey.data.all$year)-1)){
 #     int.landings.all <-c(int.landings.all,NA)
#    }
#	  names(int.landings.all)<- year.ran
#	  int.landings.all<- int.landings.all[names(int.landings.all) %in% year.ran]
	
    bias <-
    switch(f.u,
      "fladen"=1.35,
      "firth forth"=1.18,
      "moray firth"=1.21,
      "north minch"=1.33,
      "south minch"=1.32,
      "clyde"=1.19)
	 # "jura"=1.19) # jura bias to be discussed. No discards available 
    
	  if (max(year.ran)>=2011){
      yr.ran.ls <-list(seq(min(year.ran),2010,1),seq(2011,max(year.ran),1))
    } else{
      yr.ran.ls <-list(year.ran)
    } 	
    out.all <-data.frame()
    for (l in 1:length(yr.ran.ls)){
	    
      yr.ran <-yr.ran.ls[[l]]
      
	    stock.yr.ran <-seq(min(yr.ran),min(max(yr.ran),dims(stock.object)$maxyear),1)
      
#      stock <-trim(stock.object,year=yr.ran)
      stock <-trim(stock.object,year=stock.yr.ran)
     
      survey.data <-subset(survey.data.all,year %in% yr.ran)
	  
 #     stock <-trim(stock.object,year=survey.data$year)
      
	    landings.n<- seasonSums(quantSums(unitSums(stock@landings.n)))
      discards.n <-seasonSums(quantSums(unitSums(stock@discards.n)))
	    dead.discards<- 0.75*seasonSums(quantSums(unitSums(stock@discards.n)))
      catches <-seasonSums(quantSums(unitSums(stock@catch.n)))
    	removals <-seasonSums(quantSums(unitSums(stock@landings.n)))+ dead.discards
      
      tot.wt <-seasonSums(quantSums(unitSums(stock@landings.n*stock@landings.wt)))
      
      if (max(yr.ran)>=2011){
        raising.factor <-1
      }else{
        raising.factor <-(int.landings.all[names(int.landings.all) %in% stock.yr.ran]/tot.wt)
      }
    
    #Discards
      Discard.rate<- round((discards.n/catches),3)
      Dead.discard.rate<- round((dead.discards/removals),3)
      
      discards.wt<- seasonSums(quantSums(unitSums(stock@discards.n*stock@discards.wt)))
      dead.discards.wt<- seasonSums(quantSums(unitSums(0.75*stock@discards.n*stock@discards.wt)))

    
    #Mean weights
    #		mean.weights<- mean.weight.landings(stock)[,,1,,]
      mean.weights.land <- mean.weight2(stock.list=list(stock))[,2]		
    #Discards
      mean.weights.disc <-mean.wt.disc2(stock.list=list(stock))[,2]
      
    
    tmp <-cbind(#year=survey.data$year,
                landings.numbers = round(raising.factor*landings.n/1000, 0),					#millions
                discard.numbers = round(raising.factor*discards.n/1000, 0), 						#millions
                removals.numbers = round(raising.factor*removals/1000, 0),					#millions                
                landings.tonnes = int.landings.all[names(int.landings.all) %in% yr.ran],
                discard.tonnes = round(raising.factor*discards.wt, 0),
				dead.discard.tonnes = round(raising.factor*dead.discards.wt, 0),
                discard.rate = round(Discard.rate*100, 1),
                mean.wt.landings = round(mean.weights.land, 2),
                mean.wt.discards = round(mean.weights.disc, 2),
                dead.discard.rate = round(Dead.discard.rate*100, 1))
      
    tmp <-rbind(tmp,matrix(NA,nrow=length(yr.ran)-nrow(tmp),ncol=ncol(tmp)))

    if(f.u == "north minch")
      {	
      #Harvest ratio
        length(removals) <-length(yr.ran)
        HR.VMS <- raising.factor*removals/(1000*survey.data$abundance.VMS.2/bias)
        HR.sediment <- raising.factor*removals/(1000*survey.data$abundance.sediment/bias)
        
        surv.tmp <-cbind(year=survey.data$year,adjusted.abundance.sediment = round(survey.data$abundance.sediment/bias, 0),		#millions
                         adjusted.abundance.VMS = round(survey.data$abundance.VMS.2/bias, 0),
                         harvest.ratio.sediment = round(HR.sediment*100, 1),
                          harvest.ratio.VMS = round(HR.VMS*100, 1))
    }else{
        #Harvest ratio
        length(removals) <-length(yr.ran)
		HR <- raising.factor*removals/(1000*survey.data$abundance/bias)    

        surv.tmp <-cbind(year=survey.data$year,adjusted.abundance = round(survey.data$abundance/bias, 0),		#millions
                          harvest.ratio = round(HR*100, 1))
    }
    out.all <-rbind(out.all,cbind(surv.tmp,tmp))
  }
	
	write.table(out.all, paste(wk.dir, "/", f.u, "_Exploitation summary.csv", sep = ""), row.names=FALSE, sep =",")
	

	
}



