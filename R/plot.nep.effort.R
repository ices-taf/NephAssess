plot.nep.effort<- function(stock, effort.data,type=c("landings","effort","lpue"), by.gear=F)
{
  yr<- as.numeric(dimnames(effort.data)$year)
  if(by.gear==T)
  {
    land.other_trawler<- seasonSums(quantSums(stock@landings[c("OTHER")]))[,as.character(yr),,,,]
    land.nep_trawler<- seasonSums(quantSums(stock@landings[c("OTB_CRU", "OTT_CRU")]))[,as.character(yr),,,,]
    eff.other_trawler<- quantSums(effort.data[c("OTB_DEF", "OTT_DEF")])[,as.character(yr),,,,]
    eff.nep_trawler<- quantSums(effort.data[c("OTB_CRU", "OTT_CRU")])[,as.character(yr),,,,]
    lpue.other_trawler<- (land.other_trawler/eff.other_trawler)*1000    #in kg//day
    lpue.nep_trawler<- land.nep_trawler/eff.nep_trawler*1000            #in kg//day
    
    landRange<- c(0,max(land.other_trawler,land.nep_trawler))
    effRange<- c(0,max(eff.other_trawler,eff.nep_trawler))
    lpueRange<- c(0,max(lpue.other_trawler,lpue.nep_trawler))
    
    if(type=="landings")
    {
      plot(0, 0, type="n", lwd=5, main="Landings", ylim=landRange, xlim=range(yr),xlab="Year", ylab="Landings (tonnes)", cex.axis=0.9, bty="l",axes=T)
      lines(yr, land.other_trawler, lwd=5,col=1)
      lines(yr, land.nep_trawler, lwd=5, col=2)
    }
    
    if(type=="effort")
    {
      plot(0, 0, type="n", lwd=5, main="Effort", ylim=effRange, xlim=range(yr),xlab="Year", ylab="Effort (days)", cex.axis=0.9, bty="l",axes=T)
      lines(yr, eff.other_trawler, lwd=5,col=1)
      lines(yr, eff.nep_trawler, lwd=5,col=2)
    }
    
    if(type=="lpue")
    {
      plot(0, 0, type="n", lwd=5, main="LPUE", ylim=lpueRange, xlim=range(yr),xlab="Year", ylab="LPUE (kg/days)", cex.axis=0.9, bty="l",axes=T)
      lines(yr,lpue.other_trawler, lwd=5,col=1)
      lines(yr, lpue.nep_trawler, lwd=5,col=2)
    }
    legend("topright", c("Nephrops trawlers", "Other trawlers"),bty="n", lwd=5,col=c(2,1))
  }
  
  if(by.gear==F)
  {
    land<- seasonSums(quantSums(stock@landings[c("OTB_CRU", "OTT_CRU", "OTHER")]))[,as.character(yr),,,,]
    eff<- quantSums(effort.data[c("OTB_CRU", "OTT_CRU", "OTB_DEF", "OTT_DEF")])[,as.character(yr),,,,]
    lpue<- land/eff*1000            #in kg//day
    
    landRange<- c(0,max(land))
    effRange<- c(0,max(eff))
    lpueRange<- c(0,max(lpue))
    
    if(type=="landings")
    {
      plot(0, 0, type="n", lwd=5, main="Landings", ylim=landRange, xlim=range(yr),xlab="Year", ylab="Landings (tonnes)", cex.axis=0.9, bty="l",axes=T)
      lines(yr, land, lwd=5,col=1)
    }
    
    if(type=="effort")
    {
      plot(0, 0, type="n", lwd=5, main="Effort", ylim=effRange, xlim=range(yr),xlab="Year", ylab="Effort (days)", cex.axis=0.9, bty="l",axes=T)
      lines(yr, eff, lwd=5,col=1)
    }
    
    if(type=="lpue")
    {
      plot(0, 0, type="n", lwd=5, main="LPUE", ylim=lpueRange, xlim=range(yr),xlab="Year", ylab="LPUE (kg/days)", cex.axis=0.9, bty="l",axes=T)
      lines(yr,lpue, lwd=5,col=1)
    }
  }
}
