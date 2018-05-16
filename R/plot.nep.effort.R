plot.nep.effort<- function(stock, effort.data,type=c("landings","effort","lpue"))
{
  yr<- as.numeric(dimnames(effort.data)$year)
  
  land.tr1<- seasonSums(quantSums(stock@landings[c("OTHER")]))[,as.character(yr),,,,]
  land.tr2<- seasonSums(quantSums(stock@landings[c("OTB_CRU", "OTT_CRU")]))[,as.character(yr),,,,]
  eff.tr1<- quantSums(effort.data[c("OTB_DEF", "OTT_DEF")])[,as.character(yr),,,,]
  eff.tr2<- quantSums(effort.data[c("OTB_CRU", "OTT_CRU")])[,as.character(yr),,,,]
  lpue.tr1<- land.tr1/eff.tr1
  lpue.tr2<- land.tr2/eff.tr2
  
  landRange<- c(0,max(land.tr1,land.tr2))
  effRange<- c(0,max(eff.tr1,eff.tr2))
  lpueRange<- c(0,max(lpue.tr1,lpue.tr2))
  
  if(type=="landings")
  {
    plot(0, 0, type="n", lwd=5, main="Landings", ylim=landRange, xlim=range(yr),xlab="Year", ylab="Landings (tonnes)", cex.axis=0.9, bty="l",axes=T)
    lines(yr, land.tr1, lwd=5,col=1)
    lines(yr, land.tr2, lwd=5, col=2)
    legend("topright", c("TR1","TR2"),bty="n", lwd=5,col=c(1,2))
  }
  
  if(type=="effort")
  {
    plot(0, 0, type="n", lwd=5, main="Effort", ylim=effRange, xlim=range(yr),xlab="Year", ylab="Effort (days)", cex.axis=0.9, bty="l",axes=T)
    lines(yr, eff.tr1, lwd=5,col=1)
    lines(yr, eff.tr2, lwd=5,col=2)
    legend("topright", c("TR1","TR2"),bty="n", lwd=5,col=c(1,2))
  }
  
  if(type=="lpue")
  {
    plot(0, 0, type="n", lwd=5, main="LPUE", ylim=lpueRange, xlim=range(yr),xlab="Year", ylab="LPUE (kg/days)", cex.axis=0.9, bty="l",axes=T)
    lines(yr,lpue.tr1, lwd=5,col=1)
    lines(yr, lpue.tr2, lwd=5,col=2)
    legend("topright", c("TR1","TR2"),bty="n", lwd=5,col=c(1,2))
  }
}
