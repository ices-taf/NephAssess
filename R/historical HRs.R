exploitation.table <-
function(wk.dir, stock, f.u,stock.object, international.landings, survey, year.ran) 
{
    f.u <- check.fu (f.u)
    
    survey.data<- read.csv(survey)
	int.landings<- subset(read.csv(international.landings), Year %in% survey.data$year)$Total
	names(int.landings)<- survey.data$year
	int.landings<- int.landings[names(int.landings) %in% year.ran]
	
    bias <-
    switch(f.u,
      "fladen"=1.35,
      "firth forth"=1.18,
      "moray firth"=1.21,
      "north minch"=1.33,
      "south minch"=1.32,
      "clyde"=1.19)
	 # "jura"=1.19) # jura bias to be discussed. No discards available 
    
	if (max(year.ran)>=2010){
      yr.ran <-seq(min(year.ran),2010,1)
      yr.ran.2011 <-seq(2011,max(year.ran),1)
    } else{
      yr.ran <-year.ran
    } 	

	
	stock <-trim(stock.object,year=yr.ran)
    survey.data <-subset(survey.data,year %in% yr.ran)
	
    stock <-trim(stock.object,year=survey.data$year)
    
	
	
    discards <-seasonSums(quantSums(unitSums(stock@discards.n)))
	dead.discards<- 0.75*seasonSums(quantSums(unitSums(stock@discards.n)))
    catches <-seasonSums(quantSums(unitSums(stock@catch.n)))
    landings.n<- seasonSums(quantSums(unitSums(stock@landings.n)))
	
    tot.wt <-seasonSums(quantSums(unitSums(stock@landings.n*stock@landings.wt)))
    
    scot.removals <-seasonSums(quantSums(unitSums(stock@landings.n)))+ dead.discards
    
    (int.landings[1:length(yr.ran)]/tot.wt)*scot.removals -> int.removals 
	
	
	
	if(f.u == "north minch")
	{
			#Harvest ratio
		HR.VMS <- int.removals/(1000*survey.data$abundance.VMS.2/bias)
		HR.sediment <- int.removals/(1000*survey.data$abundance.sediment/bias)
		
		#Discards
		Discard.rate<- round((discards/catches)@.Data[1,,1,1,1,1],3)
		Dead.discard.rate<- round((dead.discards/scot.removals)@.Data[1,,1,1,1,1],3)
		Discards.wt<- seasonSums(quantSums(unitSums(stock@discards.n*stock@discards.wt)))
		
		
		#Mean weights
#		mean.weights<- mean.weight.landings(stock)[,,1,,]
		
		#Removals
		
			
		tmp <-cbind(year=survey.data$year,
						landings.numbers = round(landings.n/1000, 0),								#millions
						discard.numbers = round(discards/1000, 0), 									#millions
						removals.numbers = round(int.removals/1000, 0),								#millions
						adjusted.abundance.sediment = round(survey.data$abundance.sediment/bias, 0),		#millions
						adjusted.abundance.VMS = round(survey.data$abundance.VMS.2/bias, 0),
						harvest.ratio.VMS = round(HR.VMS*100, 1),
						harvest.ratio.sediment = round(HR.sediment*100, 1),
						landings.tonnes = int.landings[1:length(yr.ran)],
						discard.tonnes = round(Discards.wt, 0),
						discard.rate = round(Discard.rate*100, 1),
					#	mean.wt.landings = round(mean.weights, 2),
						dead.discard.rate = round(Dead.discard.rate*100, 1))
						
		
	} else {
	
		#Harvest ratio
		HR <- int.removals/(1000*survey.data$abundance/bias)

		#Discards
		Discard.rate<- round((discards/catches)@.Data[1,,1,1,1,1],3)
		Dead.discard.rate<- round((dead.discards/scot.removals)@.Data[1,,1,1,1,1],3)
		Discards.wt<- seasonSums(quantSums(unitSums(stock@discards.n*stock@discards.wt)))
		
		
		#Mean weights
#		mean.weights<- mean.weight.landings(stock)[,,1,,]
		
		#Removals
		
			
		tmp <-cbind(year=survey.data$year,
						landings.numbers = round(landings.n/1000, 0),					#millions
						discard.numbers = round(discards/1000, 0), 						#millions
						removals.numbers = round(int.removals/1000, 0),					#millions
						adjusted.abundance = round(survey.data$abundance/bias, 0),		#millions
						harvest.ratio = round(HR*100, 1),
						landings.tonnes = int.landings[1:length(yr.ran)],
						discard.tonnes = round(Discards.wt, 0),
						discard.rate = round(Discard.rate*100, 1),
					#	mean.wt.landings = round(mean.weights, 2),
						dead.discard.rate = round(Dead.discard.rate*100, 1))
						
		
	}
	
	
	
	
	
	if(max(year.ran)>=2010)
	{
	
	survey.data<- read.csv(survey)
	stock <-trim(stock.object,year=yr.ran.2011)
    survey.data <-subset(survey.data,year %in% yr.ran.2011)
	
    stock <-trim(stock.object,year=survey.data$year)
	
	
    discards <-seasonSums(quantSums(unitSums(stock@discards.n)))
	dead.discards<- 0.75*seasonSums(quantSums(unitSums(stock@discards.n)))
    catches <-seasonSums(quantSums(unitSums(stock@catch.n)))
    landings.n<- seasonSums(quantSums(unitSums(stock@landings.n)))
	
    tot.wt <-seasonSums(quantSums(unitSums(stock@landings.n*stock@landings.wt)))
    
    scot.removals <-seasonSums(quantSums(unitSums(stock@landings.n)))+ dead.discards
    
    int.removals <- landings.n + dead.discards
	
	
	
	if(f.u == "north minch")
	{
			#Harvest ratio
		HR.VMS <- int.removals/(1000*survey.data$abundance.VMS.2/bias)
		HR.sediment <- int.removals/(1000*survey.data$abundance.sediment/bias)
		
		#Discards
		Discard.rate<- round((discards/catches)@.Data[1,,1,1,1,1],3)
		Dead.discard.rate<- round((dead.discards/scot.removals)@.Data[1,,1,1,1,1],3)
		Discards.wt<- seasonSums(quantSums(unitSums(stock@discards.n*stock@discards.wt)))
		
		
		#Mean weights
#		mean.weights<- mean.weight.landings(stock)[,,1,,]
		
		#Removals
		
			
		tmp.2011 <-cbind(year=survey.data$year,
						landings.numbers = round(landings.n/1000, 0),								#millions
						discard.numbers = round(discards/1000, 0), 									#millions
						removals.numbers = round(int.removals/1000, 0),								#millions
						adjusted.abundance.sediment = round(survey.data$abundance.sediment/bias, 0),		#millions
						adjusted.abundance.VMS = round(survey.data$abundance.VMS.2/bias, 0),
						harvest.ratio.VMS = round(HR.VMS*100, 1),
						harvest.ratio.sediment = round(HR.sediment*100, 1),
						landings.tonnes = int.landings[(length(yr.ran)+1):length(year.ran)],
						discard.tonnes = round(Discards.wt, 0),
						discard.rate = round(Discard.rate*100, 1),
					#	mean.wt.landings = round(mean.weights, 2),
						dead.discard.rate = round(Dead.discard.rate*100, 1))
						
			
	} else {
	
		#Harvest ratio
		HR <- int.removals/(1000*survey.data$abundance/bias)

		#Discards
		Discard.rate<- round((discards/catches)@.Data[1,,1,1,1,1],3)
		Dead.discard.rate<- round((dead.discards/scot.removals)@.Data[1,,1,1,1,1],3)
		Discards.wt<- seasonSums(quantSums(unitSums(stock@discards.n*stock@discards.wt)))
		
		
		#Mean weights
#		mean.weights<- mean.weight.landings(stock)[,,1,,]
		
		#Removals
		
			
		tmp.2011 <-cbind(year=survey.data$year,
						landings.numbers = round(landings.n/1000, 0),					#millions
						discard.numbers = round(discards/1000, 0), 						#millions
						removals.numbers = round(int.removals/1000, 0),					#millions
						adjusted.abundance = round(survey.data$abundance/bias, 0),		#millions
						harvest.ratio = round(HR*100, 1),
						landings.tonnes = int.landings[(length(yr.ran)+1):length(year.ran)],
						discard.tonnes = round(Discards.wt, 0),
						discard.rate = round(Discard.rate*100, 1),
					#	mean.wt.landings = round(mean.weights, 2),
						dead.discard.rate = round(Dead.discard.rate*100, 1))
						
		
	}
	
		tmp <-rbind(tmp,tmp.2011)
		write.table(tmp, paste(wk.dir, "/", f.u, "_Exploitation summary.csv", sep = ""), row.names=FALSE, sep =",")
	
	}
	
	
}



