`nephup.long.term.plots_kw` <-
function(wk.dir, stock, effort.data.days=NULL, effort.data.kwdays=NULL, international = F, international.landings = NULL)
{

  rescale<- function(v,reference)  #v is vector to scale
  {
    out<- rep(NA,length(v))
    for(i in 1:length(v))
    {
      out[i]<- (max(reference)-min(reference)) / (max(v)-min(v))*(v[i]-min(v)) + min(reference)
    }
    return(out)
  }
    
# Landings UK (tonnes)

 landings.UK <- rowSums(colSums(stock@landings[c("OTB_CRU", "OTT_CRU", "OTHER", "FPO"),,,,,]))
 
  
#Landings trawl
  
  landings.st.nt <- rowSums(colSums(stock@landings[c("OTB_CRU", "OTT_CRU"),,,,,]))
  years.land<- stock@range["minyear"]:stock@range["maxyear"]
  creel <- rowSums(colSums(stock@landings["FPO",,,,,]  ))

# Effort Scot NT in days & CPUE/LPUE SCOT NT kg/hour
  if(is.null(effort.data.kwdays))
  {
    tmp <- colSums(effort.data.days[c("OTB_CRU", "OTT_CRU"),,,,,])
    effort.total <- rowSums(tmp)
    years.eff<- as.numeric(names(effort.total))
    years.match<- years.land[match(years.eff, years.land)]
    LPUE.tot <- landings.st.nt[names(landings.st.nt) %in% years.match]*1000/effort.total  
    ylabEff= "Effort (days absent)"
    ylabLPUE= "LPUE (kg/day trawling)"
  }

  if(is.null(effort.data.days))
  {
    tmp <- colSums(effort.data.kwdays[c("OTB_CRU", "OTT_CRU"),,,,,])
    effort.total <- rowSums(tmp)
    years.eff<- as.numeric(names(effort.total))
    years.match<- years.land[match(years.eff, years.land)]
    LPUE.tot <- landings.st.nt[names(landings.st.nt) %in% years.match]*1000/effort.total
    ylabEff= "Effort (kW days)"
    ylabLPUE= "LPUE (kg/kW days)"
  }
  
  if(!is.null(effort.data.days) & !is.null(effort.data.kwdays))
  {
    tmp<- list(days=effort.data.days,kwdays=effort.data.kwdays)
    plotLabels<- list(days=list(ylabEff= "Effort (days absent)", ylabLPUE= "LPUE (kg/day trawling)"),
                      kwdays=list(ylabEff= "Effort (kW days)", ylabLPUE= "LPUE (kg/kW days)"))
    effort.total<- lapply(tmp, function(x) {rowSums(colSums(x[c("OTB_CRU", "OTT_CRU"),,,,,]))})
    effort.total$kwdays.rescaled<- round(rescale(effort.total$kwdays, effort.total$days))
    years.eff<- as.numeric(names(effort.total[[1]]))
    years.match<- years.land[match(years.eff, years.land)]
    LPUE.tot <- lapply(effort.total, function(x) { landings.st.nt[names(landings.st.nt) %in% years.match]*1000/x } )
    LPUE.tot$kwdays.rescaled<- round(rescale(LPUE.tot$kwdays, LPUE.tot$days))
  }

#  Mean sizes

  means <- mean.sizes(stock)
  

########PLOTS##################

  get.fname <-
  function (base.name)
  {
    fname <- function(i) paste(base.name, i, ".png", sep="")
    out <- fname(i <- 1)
 #   while (file.exists(out))
 #      out <- fname(i <- i + 1)

    return ( out )
  }
 
  png(get.fname(paste(wk.dir, "long term trends", sep = "")), width = 2100, height = 3000, pointsize = 50)

  par(mfrow=c(4,1))

  #legend position
  leg.x <- stock@range["maxyear"]-8
  leg.y.lands <- range(landings.st.nt, na.rm=T)[2] - range(landings.st.nt, na.rm=T)[1]
  leg.y.size <- min(means$means.female.high, na.rm=TRUE)

  year <- stock@range["minyear"]:stock@range["maxyear"]

  # Long term landings
  plot(year, landings.st.nt, type="o", xlab="", ylab="", bty="l", lwd=2, ylim=c(0, max(landings.UK, na.rm=T)+max(landings.UK*0.05, na.rm=T)))
  points(year, landings.st.nt, pch=21, bg="white")
  lines(year, creel, type="l", lwd=2)
  points(year, creel, pch=4, lwd=2)
  lines(year, landings.UK, type="l", lwd=2)
  points(year, landings.UK, pch=22, lwd=2, bg="white")

  if(international == T)
  {
	  temp<- read.csv(paste(wk.dir, international.landings, sep=""), head = T)
	  temp<- temp[, c("Year", "Total")]
	  lines(temp$Year, temp$Total, type="l", lwd=2)
	  points(temp$Year, temp$Total, pch=23, lwd=2, bg="black")
	  legend("topleft", legend=c("UK Scotland - All gears", "UK Scotland - Nephrops trawlers", "Creel", "International"), pch=c(22,21,4,23), pt.bg = c("white", "white", "black", "black"), lty= 1, bty="n", cex=0.7)
  } else
  
  {
  legend("topleft", legend=c("UK Scotland - All gears", "UK Scotland - Nephrops trawlers", "Creel"), pch=c(22,21,4), pt.bg = c("white", "white", "black"), lty= 1, bty="n", cex=0.6)
  }
  
  title(main="Landings - International", xlab="", ylab="Landings (tonnes)")
 
  #If only one effort type is available
  if(sum(c(is.null(effort.data.days), is.null(effort.data.kwdays)))==1)
  {
    # Effort - Scottish  Nephrops trawlers
    plot(as.numeric(names(effort.total)), effort.total, type="o", xlab="", ylab="", bty="l", col="black", lwd=2, ylim=c(0, max(effort.total, na.rm=T)+max(effort.total*0.05, na.rm=T)))
    points(as.numeric(names(effort.total)), effort.total, pch=21, bg="black")
    title(main="Effort - Scottish Nephrops trawlers", xlab="", ylab= ylabEff)

    # LPUE - Scottish Nephrops trawlers
    plot(as.numeric(names(LPUE.tot)), LPUE.tot, type="o", xlab="", ylab="", bty="l", lwd=2, ylim=c(0, max(LPUE.tot, na.rm=T)+max(LPUE.tot*0.05, na.rm=T)))
    points(as.numeric(names(LPUE.tot)), LPUE.tot, , pch=24, bg="black")
    title(main="LPUE - Scottish Nephrops trawlers", xlab="", ylab= ylabLPUE)
  }

  #If two effort sources (days & kwdays) are available
  if(!is.null(effort.data.days) & !is.null(effort.data.kwdays))
  {
    # Effort - Days
    par(mar=c(5, 4, 4, 4)+0.1)
    yrange<- c(0, max(effort.total$days, na.rm=T)+max(effort.total$days*0.05, na.rm=T))
    plot(as.numeric(names(effort.total$days)), effort.total$days, type="o", xlab="", ylab=plotLabels$days$ylabEff, bty="u", col="black", lwd=2, ylim=yrange)
    points(as.numeric(names(effort.total$days)), effort.total$days, pch=21, bg="black",cex=1.5)
    # Effort - kWDays
    lines(as.numeric(names(effort.total$kwdays)), effort.total$kwdays.rescaled, lwd=2,lty=2)
    points(as.numeric(names(effort.total$kwdays)), effort.total$kwdays.rescaled, pch=21, bg="white",cex=1.2)
    new.scale<- c(0,round(seq(min(effort.total$kwdays),max(effort.total$kwdays),length=length(pretty(effort.total$days))),-2))
    axis(4, at=c(0,pretty(effort.total$days)), labels=new.scale)
    mtext(plotLabels$kwdays$ylabEff, side=4, line=3, cex=0.7)
    title(main="Effort - Scottish Nephrops trawlers", xlab="")
    legend("topright", legend=c("Days absent", "kW days"), pch=c(21,21), pt.bg = c("black", "white"), lty= c(1,2), lwd=c(2,2), bty="n")
    
    # LPUE - Days
    par(mar=c(5, 4, 4, 4)+0.1)
    yrange<- c(0, max(LPUE.tot$days, na.rm=T)+max(LPUE.tot$days*0.05, na.rm=T))
    plot(as.numeric(names(LPUE.tot$days)), LPUE.tot$days, type="o", xlab="", ylab=plotLabels$days$ylabLPUE, bty="u", col="black", lwd=2, ylim=yrange)
    points(as.numeric(names(LPUE.tot$days)), LPUE.tot$days, pch=21, bg="black",cex=1.5)
    # LPUE - kWDays
    lines(as.numeric(names(LPUE.tot$kwdays)), LPUE.tot$kwdays.rescaled, lwd=2,lty=2)
    points(as.numeric(names(LPUE.tot$kwdays)), LPUE.tot$kwdays.rescaled, pch=21, bg="white",cex=1.2)
    new.scale<- c(0,round(seq(min(LPUE.tot$kwdays),max(LPUE.tot$kwdays),length=length(pretty(LPUE.tot$days))),1))
    axis(4, at=c(0,pretty(LPUE.tot$days)), labels=new.scale)
    mtext(plotLabels$kwdays$ylabLPUE, side=4, line=3, cex=0.7)
    title(main="LPUE - Scottish Nephrops trawlers", xlab="")
    legend("topleft", legend=c("Days absent", "kW days"), pch=c(21,21), pt.bg = c("black", "white"), lty= c(1,2), lwd=c(2,2), bty="n")
  }
  

# Mean sizes Scottish Nephrops trawlers
  y.min <-min(means$means.male.low.c,means$means.female.low.c,means$means.male.low,means$means.female.low,na.rm=T)
 
  plot(year, means$means.male.low, type="o", pch=23, lwd=2, xlab="", ylab="", bty="l", ylim=c(y.min-2, max(means$means.male.high, na.rm=T)))
  points(year, means$means.male.low, pch=23, bg="white")
  lines(year, means$means.female.low, type="o", pch=24, lwd=2)
  points(year, means$means.female.low, pch=24, bg="white")
  lines(year, means$means.male.high, type="o", pch=23, lwd=2)
  points(year, means$means.male.high, pch=23, bg="grey50")
  lines(year, means$means.female.high, type="o", pch=24, lwd=2)
  points(year, means$means.female.high, pch=24, bg="grey50")
if(regexpr("Noup", stock@name)[1] == (-1))
{ 
  lines(year, means$means.male.low.c, type="o", pch=1, lwd=2)
  points(year, means$means.male.low.c, pch=23, bg="black")
  lines(year, means$means.female.low.c, type="o", pch=2, lwd=2)
  points(year, means$means.female.low.c, pch=24, bg="black")
} 
  
  title(main="Mean sizes", xlab = "year", ylab="Mean size (mm carapace length)")

  legend("left", legend=c("Landings Mal >35", "Landings Fem >35", "Landings Mal <35", "Landings Fem <35", "Catch Mal <35mm CL", "Catch Fem <35"),
          pch=c(23,24,23,24,23,24), pt.bg = c("grey50", "grey50", "white", "white", "black", "black"), lty=1, bty="n", ncol=3, cex=0.7, inset=0.05)

  dev.off()
}

