plots.advice.2014<- 
function(wk.dir, f.u, bias, MSY.hr, stock.object, international.landings, tv_results, Exploitation_summary)
{
  f.u <- check.fu (f.u)
  
  setwd(wk.dir)

  hist.land <- read.csv(international.landings, header=T) 

  first.disc.yr <-switch(f.u,1990,"fladen"=2000,"jura"=9999)
  
  first.catch.yr <-max(min(hist.land$Year),stock.object@range["minyear"])
  last.catch.yr <-stock.object@range["maxyear"]
  
  hist.land <-subset(hist.land,Year>=first.catch.yr)
  stock.object <-trim(stock.object,year=first.catch.yr:last.catch.yr)

  win.metafile(paste(f.u, "_advice_landings.wmf", sep=""), height=6, width=8, pointsize=16)
#bar plot of landings & discards  
  tot.wt <-seasonSums(quantSums(unitSums(stock.object@landings.n*stock.object@landings.wt)))
  discards.wt<- seasonSums(quantSums(unitSums(stock.object@discards.n*stock.object@discards.wt)))
#  If you want a full time series of discards i.e. filled in with average % then delete the following line
  discards.wt[,hist.land$Year<first.disc.yr] <-NA

  raising.factor <-hist.land$Total/tot.wt
  raising.factor[,hist.land$Year>=2011] <-1 
  
  hist.land$discards.tonnes <-round(c(discards.wt*raising.factor),0)
 # hist.land<- hist.land[hist.land$Year %in% read.csv(tv_results, header=T)$year,]

  with(hist.land, barplot(rbind(Total,discards.tonnes),names=Year, col=c("white","grey"), bty="l",axes=T, main="Catches"))
  legend("topleft", legend=c("Landings", "Discards"), fill=c("white","grey"),  inset=0.01, cex=0.8, bty="n")
  dev.off()


##########################################################
#UW tv survey
##########################################################
  win.metafile(paste(f.u, "_advice_tv.wmf", sep=""), height=6, width=8, pointsize=16)
  hist.tv <- read.csv(tv_results, header=T)

  tv.series <-data.frame(year=hist.tv$year)

  first.ci.year <-min(tv.series$year)

  if(f.u == "north minch")
  {	
	#VMS
	  tv.series$abundance <-hist.tv$abundance.VMS.2/bias
    tv.series$confidence.interval <-hist.tv$confidence.interval.VMS.2/bias  
    first.ci.year <-2010 
    
  } else {
  
    tv.series$abundance <-hist.tv$abundance/bias
    tv.series$confidence.interval <-hist.tv$confidence.interval/bias
  }	
  
	tv.series$upper <- with(tv.series, abundance+confidence.interval)
	tv.series$lower <- with(tv.series, abundance-confidence.interval)	

	trigger <-min(tv.series$abundance[tv.series$year<2010],na.rm=TRUE)
  if (f.u == "jura"){trigger <- -100}

  with(tv.series, plot(year,abundance, type="l", lty=1, bty="l",lwd=2,xlab="Year", main="Stock Abundance", ylab="Abundance (millions)", ylim=c(0,max(upper,na.rm=T)*1.05)), pch=1, lty=2)
  with(tv.series, points(year,abundance,  pch=19, cex=1.5))

	with(subset(tv.series,year>=first.ci.year), arrows(year,abundance, year, upper, lty=1, lwd=1.5, angle=90, length=0.1))
	with(subset(tv.series,year>=first.ci.year), arrows(year,abundance,year, lower, lty=1, lwd=1.5, angle=90, length=0.1))
	#Btrigger is    TV value in 2007
	abline(h=trigger, lty=2, col="green3" ,lwd=3)

	dev.off()
	
	
##########################################################
#Harvest rates
##########################################################     
  win.metafile(paste(f.u, "_advice_HR.wmf", sep=""), height=6, width=8, pointsize=16)
  HRs <- read.csv(Exploitation_summary, header=T)
  
  if( f.u == "north minch"){

	  with(HRs, plot(harvest.ratio.VMS~year, type="l", xlab="Year", ylab="Reported Harvest Rate %", main="Harvest rate",cex=1,bty="l", ylim=c(0, max(harvest.ratio.VMS,na.rm=T))))
  	HRpoints<- subset(HRs, year %in% c(1994,1996))
	  points(HRpoints$year, HRpoints$harvest.ratio.VMS)
	
  } else {

	  with(HRs, plot(harvest.ratio~year, type="l", xlab="Year", ylab="Reported Harvest Rate %", main="Harvest rate",cex=1,bty="l", ylim=c(0, max(harvest.ratio,MSY.hr+1,na.rm=T))))

  }	
  abline(h=MSY.hr,lty=2,col="green3",lwd=3)
  dev.off()
 
}
 
 