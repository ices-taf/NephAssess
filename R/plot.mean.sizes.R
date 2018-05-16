  `plot.mean.sizes` <-

  function(wk.dir,stock,means){
  
  get.fname <-
  function (base.name)
  {
    fname <- function(i) paste(base.name, i, ".png", sep="")
    out <- fname(i <- 1)
    while (file.exists(out))
        out <- fname(i <- i + 1)

    return ( out )
   }

   year <- stock@range["minyear"]:stock@range["maxyear"]
   
  png(get.fname(paste(wk.dir, "mean sizes", sep = "")), width = 2100, height = 1500, pointsize = 50)

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

  title(main="Mean sizes - Scottish trawlers", xlab = "year", ylab="mean size (mm carapace length)")

#  legend("left", legend=c("Landings Mal >35", "Landings Fem >35", "Landings Mal <35", "Landings Fem <35", "Catch Mal <35mm CL", "Catch Fem <35"),
  legend(x=year[1],y=37, legend=c("Landings Mal >35", "Landings Fem >35", "Landings Mal <35", "Landings Fem <35", "Catch Mal <35mm CL", "Catch Fem <35"),
          pch=c(23,24,23,24,23,24), pt.bg = c("grey50", "grey50", "white", "white", "black", "black"), lty=1, bty="n", ncol=3, cex=0.8, inset=0.05)


  dev.off()

  }