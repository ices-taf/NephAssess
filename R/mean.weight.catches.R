mean.weight.catches <-

  function(wk.dir, stock, MCS=NULL) 
  {
  
  mean.wts<-  function(obj)
  {
    tot.wt <-seasonSums(quantSums(unitSums(obj$n*obj$wt)))
    tot.num <-seasonSums(quantSums(unitSums(obj$n)))
    mean.w<- as.data.frame(round(1000*tot.wt/tot.num,2))$data
    total.w<- as.data.frame(tot.wt)$data
    return(cbind(mean.w,total.w,tot.num))
  }
  
    
  # finds the common year range for the FUs in the list   
  miny <- stock@range["minyear"]
  maxy <- stock@range["maxyear"]
  years <- miny:maxy
   
  landings.obj<- list(n=stock@landings.n,wt=stock@landings.wt)
  discards.obj<- list(n=stock@discards.n,wt=stock@discards.wt)
  catches.obj<- list(n=stock@catch.n,wt=stock@catch.wt)
  
  out<- lapply(list(landings.obj,discards.obj,catches.obj), mean.wts)
  out<- cbind(years,as.data.frame(do.call("cbind", out)))
  names(out)<- c("years","mean.wt.landings.g","landings.t","landings.n","mean.wt.discards.g","discards.t","discards.n","mean.wt.catches.g","catches.t","catches.n")
  
  #if MCS is even (since the len categories are odd) the numbers in the last categorie must be partioned under and over MCS
  if(!is.null(MCS)) 
  {
    len.seq.under<- seq(min(as.numeric(dimnames(stock@discards.n)$lengths)),MCS,by=2)
    
    if(!MCS%%2) #even MCS
    {
      len.seq.under<- seq(min(as.numeric(dimnames(stock@discards.n)$lengths)),MCS-1,by=2)
      len.seq.over<- seq(MCS+1, max(as.numeric(dimnames(stock@discards.n)$lengths)),by=2)
    }
    
    if(!(!MCS%%2))#odd MCS
    {
      len.seq.under<- seq(min(as.numeric(dimnames(stock@discards.n)$lengths)),MCS-2,by=2)
      len.seq.over<- seq(MCS, max(as.numeric(dimnames(stock@discards.n)$lengths)),by=2)
    }
    discards.obj.under<- list(n=stock@discards.n[as.character(len.seq.under),,,,,],wt=stock@discards.wt[as.character(len.seq.under),,,,,])
    discards.obj.over<- list(n=stock@discards.n[as.character(len.seq.over),,,,,],wt=stock@discards.wt[as.character(len.seq.over),,,,,])
    
    out.mcs<- lapply(list(discards.obj.under,discards.obj.over), mean.wts)
    out.mcs<- as.data.frame(do.call("cbind", out.mcs))
    names(out.mcs)<- c("mean.wt.discards.under.g","wt.discards.under.t","discards.under.n","mean.wt.discards.over.g","wt.discards.over.t","discards.over.n")
    out<- cbind(out,out.mcs)
    out$prop.disc.under.wt<- round(out$wt.discards.under.t/out$catches.t,3)
    out$prop.disc.over.wt<- round(out$wt.discards.over.t/out$catches.t,3)
    out$prop.disc.under.n<- round(out$discards.under.n/out$catches.n,3)
    out$prop.disc.over.n<- round(out$discards.over.n/out$catches.n,3)
  } 
  
  write.table(out, paste(wk.dir, "Mean_weights.csv", sep = ""), row.names = FALSE, sep = ",")
}
