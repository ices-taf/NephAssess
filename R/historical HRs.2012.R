exploitation.table.2012 <-

function(wk.dir, nep.stock, f.u,int.landings,year.ran) 
{
    f.u <- check.fu (f.u)
    
    nep.survey.data <-read.csv(paste(wk.dir,"/", "fishstats/", f.u, "_TV_results.csv", sep = ""), header=TRUE, sep = ",")   
    
    bias <-
    switch(f.u,
      "fladen"=1.35,
      "firth forth"=1.18,
      "moray firth"=1.21,
      "north minch"=1.33,
      "south minch"=1.32,
      "clyde"=1.19)
    
    if (max(year.ran)>=2010){
      yr.ran <-seq(min(year.ran),2010,1)
      yr.ran.2011 <-seq(2011,max(year.ran),1)
    }else{
      yr.ran <-year.ran
    }    
    stock <-trim(nep.stock,year=yr.ran)
    survey.data <-subset(nep.survey.data,year %in% yr.ran)
    
    discards <-seasonSums(quantSums(unitSums(stock@discards.n)))
    catches <-seasonSums(quantSums(unitSums(stock@catch.n)))
    
    tot.wt <-seasonSums(quantSums(unitSums(stock@landings.n*stock@landings.wt)))
    
    scot.removals <-seasonSums(quantSums(unitSums(stock@landings.n)))+0.75*seasonSums(quantSums(unitSums(stock@discards.n)))
    
#    scot.removals <-seasonSums(quantSums(unitSums(stock@landings.n)))+seasonSums(quantSums(unitSums(stock@discards.n)))
      
    (int.landings$Total[1:length(yr.ran)]/tot.wt)*scot.removals -> int.removals

    HR <-int.removals/(1000*round(survey.data$abundance/bias,0))

    discards.dead <-0.75*seasonSums(quantSums(unitSums(stock@discards.n)))
    

    tmp <-cbind(year=survey.data$year,"Adjusted abundance"=round(survey.data$abundance/bias,0),
                      "Landings"=int.landings[1:length(yr.ran),1],
                      "Discard rate"=round((discards/catches)@.Data[1,,1,1,1,1],2),
                      "Dead discard rate"=round((discards.dead/scot.removals)@.Data[1,,1,1,1,1],2),
                      "Harvest ratio"=round(HR@.Data,3))
#    tmp[,4] <-round(tmp[,4],4)
    
    if(max(year.ran)>=2010){
      stock <-trim(nep.stock,year=yr.ran.2011)
      survey.data <-subset(nep.survey.data,year %in% yr.ran.2011)
    
      discards <-seasonSums(quantSums(unitSums(stock@discards.n)))
      catches <-seasonSums(quantSums(unitSums(stock@catch.n)))
    
    
      int.removals <-seasonSums(quantSums(unitSums(stock@landings.n)))+0.75*seasonSums(quantSums(unitSums(stock@discards.n)))
    
#    scot.removals <-seasonSums(quantSums(unitSums(stock@landings.n)))+seasonSums(quantSums(unitSums(stock@discards.n)))
      

      HR <-int.removals/(1000*round(survey.data$abundance/bias,0))

      discards.dead <-0.75*seasonSums(quantSums(unitSums(stock@discards.n)))
    

      tmp.2011 <-cbind(year=survey.data$year,"Adjusted abundance"=round(survey.data$abundance/bias,0),
                      "Landings"=int.landings[(length(yr.ran)+1):length(year.ran),1],
                      "Discard rate"=round((discards/catches)@.Data[1,,1,1,1,1],2),
                      "Dead discard rate"=round((discards.dead/int.removals)@.Data[1,,1,1,1,1],2),
                      "Harvest ratio"=round(HR@.Data,3))
#      tmp.2011[,4] <-round(tmp[,4],4)
    }
    tmp <-rbind(tmp,tmp.2011)
    write.table(tmp, paste(wk.dir, "Exploitation summary.csv", sep = ""), row.names=FALSE, sep =",")
}
