plots.advice <- function(wk.dir, 
                         f.u, 
                         MSY.hr, 
                         stock.object, 
                         international.landings, 
                         tv_results, 
                         Exploitation_summary)
{
  f.u <- check.fu(f.u)
  #setwd(wk.dir)
  
  hist.land <- read.csv(paste0(wk.dir,international.landings), header = TRUE) 
  hist.tv <- read.csv(paste0(wk.dir, tv_results), header = TRUE)
  
  # Format South Minch dataset
  if(f.u %in% c("south minch", "sm", "FU12", "FU 12")){
    hist.tv <- hist.tv[hist.tv$year >= 2007,]
    hist.tv <- hist.tv[,!names(hist.tv) %in% c("abundance", "confidence.interval")]
    names(hist.tv) <- gsub("krig.", "", names(hist.tv))
  }
  
  # Format North Minch dataset
  if(f.u %in% c("north minch", "nm", "FU11", "FU 11")){
    names(hist.tv) <- gsub(".VMS.2", "", names(hist.tv))
  }
  
  first.disc.yr <- switch(f.u, 1990, "fladen"=2000, "jura"=9999)
  first.catch.yr <- min(hist.tv$year)
  last.catch.yr <- stock.object@range["maxyear"]
  hist.land <- subset(hist.land,Year>=first.catch.yr)
  stock.object <- FLCore::trim(stock.object,year=first.catch.yr:last.catch.yr); attr(stock.object,"bms.n")<- FLCore::trim(attr(stock.object,"bms.n"),year=first.catch.yr:last.catch.yr)
  tot.wt <- seasonSums(quantSums(unitSums(stock.object@landings.n*stock.object@landings.wt)))
  discards.wt <- seasonSums(quantSums(unitSums(stock.object@discards.n*stock.object@discards.wt)))
  discards.wt[,hist.land$Year<first.disc.yr] <- NA
  raising.factor <- hist.land$Total/tot.wt
  raising.factor[,hist.land$Year>=2011] <- 1 
  hist.land$discards.tonnes <- round(c(discards.wt*raising.factor),0)
  hist.land$discards.tonnes[is.na(hist.land$discards.tonnes)] <- 0
  
  png(paste0(wk.dir, f.u, "_advice_landings.png"), height=300, width=300/0.55, pointsize=12)
  par(mar=c(3, 4, 2, 1))
  tmp=barplot(rbind(hist.land$Total,hist.land$discards.tonnes)/1000, plot=F, space=1.5)
  with(hist.land, tmp<- barplot(rbind(Total,discards.tonnes)/1000,names=Year, col=c("#9AC2B7","#F15D2A"), axes=F, axisnames = F, 
                                ylim=c(0,max(hist.land$Total + hist.land$discards.tonnes,na.rm=T)*1.1/1000), space=1.5))
  legend("topright", legend=c("Discards","Landings"), fill=c("#F15D2A","#9AC2B7"),  inset=0.01, bty="n", ncol=2)
  axis(1, at=tmp[seq(1,length(tmp),by=5)], labels=hist.land$Year[seq(1,length(tmp),by=5)])  
  axis(2)
  title(ylab="1000 tonnes")
  title("Catches", cex.main=0.9,line=0.5)
  box()
  dev.off()
  
  ##########################################################
  # UWTV survey
  ##########################################################
  
  
  tv.series <- data.frame(year=hist.tv$year)
  first.ci.year <- min(tv.series$year)
  tv.series$abundance <- hist.tv$abundance
  tv.series$confidence.interval <-hist.tv$confidence.interval
  tv.series$upper <- with(tv.series, abundance+confidence.interval)
  tv.series$lower <- with(tv.series, abundance-confidence.interval)	
  trigger <- min(tv.series$abundance[tv.series$year<2010], na.rm = TRUE)
  if(f.u == "jura"){trigger <- 160}
  if(f.u %in% c("south minch", "sm", "FU12", "FU 12")){trigger <- 960}
  
  #Ensure that if there is a discontinuity in the abundance series (e.g. FU9 2020) isolated points are plotted
  c1 <- which(is.na(tv.series$abundance))
  if(any(is.na(tv.series$abundance)))
  {
    c2 <- c1[which(c1[2:length(c1)]-c1[1:(length(c1)-1)]==2)]+1
    isolated_years <- tv.series[c2,"year"]
    if( is.na(tv.series[which.max(tv.series$year)-1,"abundance"]) ) { isolated_years<- c(isolated_years,max(tv.series$year)) }
    isolated_years <- tv.series[tv.series$year %in% isolated_years,]
  }
  
  png(paste0(wk.dir, f.u, "_advice_tv.png"), height=300, width=300/0.55, pointsize=12)
  par(mar=c(3, 4, 2, 1)) 
  with(tv.series, plot(year,abundance, type="l", lty=1, lwd=3,xlab="", ylab="", axes=F,ylim=c(0,max(upper,na.rm=T)*1.05)))
  with(tv.series, lines(year,upper, type="l", lty=3, lwd=2))
  with(tv.series, lines(year,lower, type="l", lty=3, lwd=2))
  if(any(is.na(tv.series$abundance)))
  {
    with(isolated_years, points(year,abundance, pch=16))
    with(isolated_years, arrows(x0=year,x1=year,y0=lower,y1=upper,length=0))
  }
  axis(1, at=tv.series$year[seq(1,length(tv.series$year),by=5)])  
  axis(2)
  title(ylab="Abundance (millions)")
  title("Stock size index - abundance", cex.main=0.9,line=0.5)
  abline(h=trigger, lty=3, col="#EB6B14" ,lwd=3)
  box()
  dev.off()
  
  
  ##########################################################
  #Harvest rates
  ##########################################################     
  png(paste0(wk.dir, f.u, "_advice_HR.png"), height=300, width=300/0.55, pointsize=12)
  par(mar=c(3, 4, 2, 1))  
  HRs <- read.csv(paste0(wk.dir, Exploitation_summary), header = TRUE)
  
  # Format South Minch dataset
  if(f.u %in% c("south minch", "sm", "FU12", "FU 12")){
    HRs <- HRs[HRs$year >= 2007,]
    names(HRs) <- gsub(".krige", "", names(HRs))
  }
  
  # Format North Minch dataset
  if(f.u %in% c("north minch", "nm", "FU11", "FU 11")){
    names(HRs) <- gsub(".VMS","",names(HRs))
  }
  
  # Ensure that if there is a discontinuity in the abundance series (e.g. FU9 2020)
  # isolated points are plotted
  c1 <- which(is.na(HRs$harvest.ratio))
  if(any(is.na(HRs$harvest.ratio)))
  {
    c2<- c1[which(c1[2:length(c1)]-c1[1:(length(c1)-1)]==2)]+1
    isolated_years<- HRs[c2,"year"]
    if( is.na(HRs[which.max(HRs$year)-1,"harvest.ratio"]) ) { isolated_years<- c(isolated_years,max(HRs$year)) }
    isolated_years<- HRs[HRs$year %in% isolated_years,]
  }
  
  with(HRs, plot(harvest.ratio~year, type="l",cex=1,bty="l", 
                 ylim=c(0, max(harvest.ratio,MSY.hr+1,na.rm=T)), lwd=1.5,axes=F,xlab="",ylab=""))
  if(any(is.na(HRs$harvest.ratio)))
  {
    with(HRs[HRs$year %in% isolated_years$year,], points(year,harvest.ratio, pch=16, cex=0.5))
  }
  axis(1, at=HRs$year[seq(1,length(HRs$year),by=5)])  
  axis(2)
  title(ylab="Harvest Rate (%)")
  title("Harvest Rate", cex.main=0.9,line=0.6)
  abline(h=MSY.hr,lty=3,col="#F7A487",lwd=3)
  box()
  dev.off()
}
