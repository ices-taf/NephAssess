`nephup.long.term.plots` <-
function(wk.dir, stock, effort.data, international = F, international.landings = NULL)
{

# Landings UK (tonnes)

 landings.UK <- rowSums(colSums(stock@landings[c("OTB_CRU", "OTT_CRU", "OTHER", "FPO"),,,,,]))
 
  
#Landings trawl
  
  landings.st.nt <- rowSums(colSums(stock@landings[c("OTB_CRU", "OTT_CRU"),,,,,]))
  
# Effort Scot NT in days 
  
  tmp <- colSums(effort.data[c("OTB_CRU", "OTT_CRU"),,,,,])
  effort.total <- rowSums(tmp)

#  CPUE/LPUE SCOT NT kg/hour

  years.land<- stock@range["minyear"]:stock@range["maxyear"]
  years.eff<- as.numeric(dimnames(effort.data[,,,,,])$year)
  years.match<- years.land[match(years.eff, years.land)]

  LPUE.tot <- landings.st.nt[names(landings.st.nt) %in% years.match]*1000/effort.total        

  creel <- rowSums(colSums(stock@landings["FPO",,,,,]  ))

  #  Mean sizes

  means <- mean.sizes(stock)
  

########PLOTS##################

  get.fname <-
  function (base.name)
  {
    fname <- function(i) paste(base.name, i, ".png", sep="")
    out <- fname(i <- 1)
    while (file.exists(out))
        out <- fname(i <- i + 1)

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
	  legend("topleft", legend=c("UK Scotland - All gears", "UK Scotland - Nephrops trawlers", "Creel", "International"), pch=c(22,21,4,23), pt.bg = c("white", "white", "black", "black"), lty= 1, bty="n", cex=0.6)
  } else
  
  {
  legend("topleft", legend=c("UK Scotland - All gears", "UK Scotland - Nephrops trawlers", "Creel"), pch=c(22,21,4), pt.bg = c("white", "white", "black"), lty= 1, bty="n", cex=0.6)
  }
  
  title(main="Landings - International", xlab="", ylab="landings (tonnes)")
 

  
  # Effort - Scottish  Nephrops trawlers

  plot(as.numeric(names(effort.total)), effort.total, type="o", xlab="", ylab="", bty="l", col="black", lwd=2, ylim=c(0, max(effort.total, na.rm=T)+max(effort.total*0.05, na.rm=T)))
  points(as.numeric(names(effort.total)), effort.total, pch=21, bg="black")
  title(main="Effort - Scottish Nephrops trawlers", xlab="", ylab= "Effort (days absent)")

  # LPUE - Scottish Nephrops trawlers

  plot(as.numeric(names(LPUE.tot)), LPUE.tot, type="o", xlab="", ylab="", bty="l", lwd=2, ylim=c(0, max(LPUE.tot, na.rm=T)+max(LPUE.tot*0.05, na.rm=T)))
  points(as.numeric(names(LPUE.tot)), LPUE.tot, , pch=24, bg="black")
  title(main="LPUE - Scottish Nephrops trawlers", xlab="", ylab= "LPUE (kg/day trawling")

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
  
  title(main="Mean sizes", xlab = "year", ylab="mean size (mm carapace length)")

  legend("left", legend=c("Landings Mal >35", "Landings Fem >35", "Landings Mal <35", "Landings Fem <35", "Catch Mal <35mm CL", "Catch Fem <35"),
          pch=c(23,24,23,24,23,24), pt.bg = c("grey50", "grey50", "white", "white", "black", "black"), lty=1, bty="n", ncol=3, cex=0.6, inset=0.05)

  dev.off()
  
}

