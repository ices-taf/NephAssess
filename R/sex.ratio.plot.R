sex.ratio.plot<- function(wdir,stock.obj,print.output=F,type="year")
{
  if(type=="year")
  {
    dat<- seasonSums(quantSums(stock.obj@catch.n))[,,"Male",,,] / seasonSums(unitSums(quantSums(stock.obj@catch.n)))
    dat<- as.data.frame(dat)
    dat<- dat[order(dat$year),]
    png(filename=paste0(wdir, stock.obj@name, "_sex_ratio_", type, ".png"), width =700, height = 700, bg = "white", res = NA)
    plot(dat$year, dat$data, type="n", ylim=c(0,1), xlab="Year", ylab="Male proportion", cex.lab=1.4)
    abline(h=0.5, lwd=3,lty=2, col="grey70")
    points(dat$year, dat$data, type="b", cex=1.5, pch=16,lwd=2)
    title(paste(stock.obj@name, "Male sex ratio"),cex=2)
  }
  
  if(type=="quarter")
  {
    dat<- quantSums(stock.obj@catch.n)[,,"Male",,,] / unitSums(quantSums(stock.obj@catch.n))
    dat<- as.data.frame(dat)
    dat<- dat[order(dat$year, dat$season),]
    dat$year_quarter<- dat$year + rep(seq(0,0.75,by=0.25),length(unique(dat$year)))
    png(filename=paste0(wdir, stock.obj@name, "_sex_ratio_", type, ".png"), width =700, height = 700, bg = "white", res = NA)
    plot(dat$year, dat$data, type="n", ylim=c(0,1), xlim=range(dat$year_quarter), xlab="Year", ylab="Male proportion", axes=F, cex.lab=1.4)
    lines(dat$year_quarter, dat$data, type="b")
    points(dat$year_quarter, dat$data, type="b", pch=21, col=dat$season, bg=dat$season,cex=1.5)
    axis(1); axis(2); box()
    abline(h=0.5, lwd=3,lty=2, col="grey70")
    title(paste(stock.obj@name, "Male sex ratio"),cex=2)
    legend("topleft", legend=c("Q1","Q2","Q3","Q4"), pch=21, col=1:4, pt.bg=1:4, bty="n", cex=1.5)
  }
  
  dev.off()
  if(print.output==T)
  {
    return(dat)
  }
}