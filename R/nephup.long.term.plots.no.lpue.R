`nephup.long.term.plots.no.lpue` <-
function(wk.dir, stock, effort.data, international = F, international.landings = NULL)
{

# Landings UK (tonnes)

 landings.UK <- rowSums(colSums(stock@landings[1,,,,,] + stock@landings[4,,,,,]))
  
# Landings Scotland Nephrops Trawl

  landings.st.nt <- rowSums(colSums(stock@landings[2:3,,,,] ))
 
# Effort Scot NT in '000 hours (this is due to change to effort in days)
  
  tmp <- colSums(effort.data)
  effort.total <- rowSums(tmp)

#  CPUE/LPUE SCOT NT kg/hour

  LPUE.tot <- landings.st.nt*1000/effort.total        # should I be multiplying by 1000 to get kg?

  creel <- rowSums(colSums(stock@landings[4,,,,,]  ))


#  Mean sizes

  means <- mean.sizes(stock, effort.data)
  

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

  par(mfrow=c(2,1))

  #legend position
  leg.x <- stock@range["maxyear"]-8
  leg.y.lands <- range(landings.st.nt)[2] - range(landings.st.nt)[1]
  leg.y.size <- min(means$means.female.high, na.rm=TRUE)

  year <- stock@range["minyear"]:stock@range["maxyear"]

  # Long term landings

  plot(year, landings.st.nt, type="o", xlab="", ylab="", bty="l", lwd=2, ylim=c(0, max(landings.UK)+max(landings.UK*0.05)))
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
  }
  
  title(main="Landings - International", xlab="", ylab="landings (tonnes)")
 
  legend("topleft", legend=c("UK Scotland - All gears", "UK Scotland - Nephrops trawlers", "Creel", "International"), pch=c(22,21,4,23), pt.bg = c("white", "white", "black", "black"), lty= 1, bty="n", cex=0.8)

  
  # Mean sizes Scottish Nephrops trawlers

  plot(year, means$means.male.low.c, type="o", pch=1, xlab="", ylab="", bty="l", lwd=2, ylim=c(min(means$means.male.low.c, na.rm=T)-2, max(means$means.male.high, na.rm=T)))
  points(year, means$means.male.low.c, pch=23, bg="black")
  lines(year, means$means.female.low.c, type="o", pch=2, lwd=2)
  points(year, means$means.female.low.c, pch=24, bg="black")
  lines(year, means$means.male.low, type="o", pch=23, lwd=2)
  points(year, means$means.male.low, pch=23, bg="white")
  lines(year, means$means.female.low, type="o", pch=24, lwd=2)
  points(year, means$means.female.low, pch=24, bg="white")
  lines(year, means$means.male.high, type="o", pch=23, lwd=2)
  points(year, means$means.male.high, pch=23, bg="grey50")
  lines(year, means$means.female.high, type="o", pch=24, lwd=2)
  points(year, means$means.female.high, pch=24, bg="grey50")
  
  title(main="Mean sizes - Scottish Nephrops trawlers", xlab = "year", ylab="mean size (mm carapace length)")

#  legend("left", legend=c("Landings Mal >35", "Landings Fem >35", "Landings Mal <35", "Landings Fem <35", "Catch Mal <35mm CL", "Catch Fem <35"),
  legend(x=year[1],y=37, legend=c("Landings Mal >35", "Landings Fem >35", "Landings Mal <35", "Landings Fem <35", "Catch Mal <35mm CL", "Catch Fem <35"),
          pch=c(23,24,23,24,23,24), pt.bg = c("grey50", "grey50", "white", "white", "black", "black"), lty=1, bty="n", ncol=3, cex=0.8, inset=0.05)


  dev.off()
  
}

