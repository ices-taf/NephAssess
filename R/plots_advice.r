plots.advice<- function(Wkdir, fu, bias, trigger, MSY.hr, international.landings, tv_results, Exploitation_summary)
{

setwd(Wkdir)

win.metafile(paste(fu, "_advice_landings.wmf", sep=""), height=6, width=8, pointsize=16)

#bar plot of landings
#load(paste(Wkdir,"Nephup/","nephup.fl.new.rdata",sep=""))
hist.land <- read.csv(international.landings, header=T)
with(hist.land, barplot(Total,names=Year, col="white", bty="l",axes=T, main=paste(fu," : International Landings",sep="")))
dev.off()


##########################################################
#UW tv survey
##########################################################


if( fu == "FU 11")
{
	win.metafile(paste(fu, "advice_tv.wmf", sep=""), height=6, width=8, pointsize=16)

	#VMS
	hist.tv <- read.csv(tv_results, header=T)
	#adjust for bias correction, 1.2 at the moment
	hist.tv$abundance.VMS.2 <-hist.tv$abundance.VMS.2/bias
	hist.tv$confidence.interval.VMS.2 <-hist.tv$confidence.interval.VMS.2/bias

	hist.tv$upper <- with(hist.tv, abundance.VMS.2+confidence.interval.VMS.2)
	hist.tv$lower <- with(hist.tv, abundance.VMS.2-confidence.interval.VMS.2)

	
	with(hist.tv, plot(year,abundance.VMS.2, type="l", lty=1, bty="l",lwd=2,xlab="Year", main=paste(fu," :  TV abundance"), ylab="Abundance (millions)", ylim=c(0,max(upper,na.rm=T)*1.05)), pch=1, lty=2)
	with(hist.tv, points(year,abundance.VMS.2,  pch=19, cex=1.5))
	#with(hist, lines(YEAR, upper, lty=2, lwd=1.5))
	#with(hist, lines(YEAR, lower, lty=2, lwd=1.5))
	yr.int<- subset(hist.tv, year >= 2010)
	with(yr.int, arrows(year,abundance.VMS.2, year, upper, lty=1, lwd=1.5, angle=90, length=0.1))
	with(yr.int, arrows(year,abundance.VMS.2,year, lower, lty=1, lwd=1.5, angle=90, length=0.1))
	#Btrigger is    TV value in 2007
	abline(h=trigger, lty=2, col="green3" ,lwd=3)

	
	#Sediment
	hist.tv <- read.csv(tv_results, header=T)
	#adjust for bias correction, 1.2 at the moment
	hist.tv$abundance.sediment <-hist.tv$abundance.sediment/bias
	hist.tv$confidence.interval.sediment <-hist.tv$confidence.interval.sediment/bias

	hist.tv$upper <- with(hist.tv, abundance.sediment+confidence.interval.sediment)
	hist.tv$lower <- with(hist.tv, abundance.sediment-confidence.interval.sediment)

	#with(hist.tv, lines(year,abundance.sediment, type="l", lty=2, bty="l",lwd=2,xlab="Year", main=paste(fu," :  TV abundance"), ylab="Abundance (millions)", ylim=c(0,max(upper,na.rm=T)*1.05)))
	#with(hist.tv, points(year,abundance.sediment,  pch=1, cex=1.5, bg="white"))
	#with(hist, lines(YEAR, upper, lty=2, lwd=1.5))
	#with(hist, lines(YEAR, lower, lty=2, lwd=1.5))
	#with(hist.tv, arrows(year,abundance.sediment, year, upper, lty=1, lwd=1.5, angle=90, length=0.1))
	#with(hist.tv, arrows(year,abundance.sediment,year, lower, lty=1, lwd=1.5, angle=90, length=0.1))
	#Btrigger is    TV value in 2007
	
	#legend("topleft", legend=c("TV abundance (Sediment)", "TV abundance (VMS)"), lty=c(2,1), pch = c(1,19), bty="n", cex=0.6)
	
	abline(h=trigger, lty=2, col="green3" ,lwd=3)
	dev.off()

} else {
	
	win.metafile(paste(fu, "advice_tv.wmf", sep=""), height=6, width=8, pointsize=16)
	
	hist.tv <- read.csv(tv_results, header=T)
	#adjust for bias correction, 1.2 at the moment
	hist.tv$abundance <-hist.tv$abundance/bias
	hist.tv$confidence.interval <-hist.tv$confidence.interval/bias

	hist.tv$upper <- with(hist.tv, abundance+confidence.interval)
	hist.tv$lower <- with(hist.tv, abundance-confidence.interval)

	with(hist.tv, plot(year,abundance, type="l", lty=1, bty="l",lwd=2,xlab="Year", main=paste(fu," :  TV abundance"), ylab="Abundance (millions)", ylim=c(0,max(upper,na.rm=T)*1.05)))
	with(hist.tv, points(year,abundance,  pch=19, cex=1.5))
	#with(hist, lines(YEAR, upper, lty=2, lwd=1.5))
	#with(hist, lines(YEAR, lower, lty=2, lwd=1.5))
	with(hist.tv, arrows(year,abundance, year, upper, lty=1, lwd=1.5, angle=90, length=0.1))
	with(hist.tv, arrows(year,abundance,year, lower, lty=1, lwd=1.5, angle=90, length=0.1))
	#Btrigger is    TV value in 2007
	abline(h=trigger, lty=2, col="green3" ,lwd=3)
	dev.off()
}



	
	
##########################################################
#Harvest rates
##########################################################     

if(fu=="FU 13 - Sound of Jura")
{
	win.metafile(paste(fu, "advice_HR.wmf", sep=""), height=6, width=8, pointsize=16)
	HRs <- read.csv(Exploitation_summary, header=T)
	plot(HRs$harvest.ratio~HRs$year, type="l", xlab="Year", ylab="Reported Harvest Rate %", main=paste(fu," :  Harvest rate"),cex=1,bty="l", ylim=c(0, MSY.hr + 1))
	abline(h=MSY.hr,lty=2,col="green3",lwd=3)
	dev.off()
} 

if( fu == "FU 11")
{

	win.metafile(paste(fu, "advice_HR.wmf", sep=""), height=6, width=8, pointsize=16)
	
	#get total number of individuals per year

	HRs <- read.csv(Exploitation_summary, header=T)

	HRs$harvest.ratio.VMS 
	HRs$harvest.ratio.sediment 

	with(HRs, plot(harvest.ratio.VMS~year, type="l", xlab="Year", ylab="Reported Harvest Rate %", main=paste(fu," :  Harvest rate"),cex=1,bty="l", ylim=c(0, max(harvest.ratio.VMS,na.rm=T))))
	HRpoints<- subset(HRs, year %in% c(1994,1996))
	points(HRpoints$year, HRpoints$harvest.ratio.VMS)

	#with(HRs, lines(harvest.ratio.sediment~year, type="l", xlab="Year", ylab="Reported Harvest Rate %", main=paste(fu," :  Harvest rate"),cex=1,bty="l", lty=2))

	#legend("topleft", legend=c("HR (Sediment)", "HR VMS"), lty=c(2,1), bty="n", cex=0.6)

	abline(h=MSY.hr,lty=2,col="green3",lwd=3)
	


	dev.off()

} 

if( fu != "FU 13 - Sound of Jura" & fu != "FU 11")

{
	win.metafile(paste(fu, "advice_HR.wmf", sep=""), height=6, width=8, pointsize=16)
	
	#get total number of individuals per year

	HRs <- read.csv(Exploitation_summary, header=T)


	#with(hist, plot(HR~YEAR, type="l", xlab="Year", ylab="Reported Harvest Rate %", main="FU6:  Harvest Rate",cex=1,bty="n", ylim=c(0, max(hist$HR))))
	#lines(range(hist$YEAR), rep(8.2, 2), lty=2, col="green3" ,lwd=3)

	with(HRs, plot(harvest.ratio~year, type="l", xlab="Year", ylab="Reported Harvest Rate %", main=paste(fu," :  Harvest rate"),cex=1,bty="l", ylim=c(0, max(harvest.ratio,na.rm=T))))

	abline(h=MSY.hr,lty=2,col="green3",lwd=3)
	#points(HRs[c(1,3),1], HRs[c(1,3),4])


	 dev.off()
}

 
}
 
 