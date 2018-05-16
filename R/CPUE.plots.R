`CPUE.plots` <-
function(neph.object, effort.obj, start.year) {

  time.series <- as.character(seq(start.year,neph.object@range["maxyear"]))

  ##  landings weight split into above and below 35mm
  landings.weight <- neph.object@catch.n*neph.object@stock.wt
  tmp <- as.numeric(rownames(landings.weight))
  small.lands <- landings.weight[tmp<=35,time.series,,]
  large.lands <- landings.weight[tmp>35,time.series,,]
  small.lands.total <- colSums(small.lands)
  large.lands.total <- colSums(large.lands)

  ## annual landings weights
  
  annual.land.sums.small <- small.lands[,,,1]+small.lands[,,,2]+small.lands[,,,3]+small.lands[,,,4]
  male.small.annual <- colSums(annual.land.sums.small[,,1,])
  female.small.annual <- colSums(annual.land.sums.small[,,2,])
  
  annual.land.sums.large <- large.lands[,,,1]+large.lands[,,,2]+large.lands[,,,3]+large.lands[,,,4]
  male.large.annual <- colSums(annual.land.sums.large[,,1,])
  female.large.annual <- colSums(annual.land.sums.large[,,2,])

  ##  effort
  effort.sums <-  colSums(effort.obj)
  effort.total <- effort.sums[,,1,]+effort.sums[,,2,]+effort.sums[,,3,]+effort.sums[,,4,]

  ##  CPUE
  tmp.male.small <- small.lands.total[,1,,]
  tmp.male.large <-  large.lands.total[,1,,]

  tmp.female.small <- small.lands.total[,2,,]
  tmp.female.large <- large.lands.total[,2,,]


  CPUE.male.small <- t(tmp.male.small*1e3/effort.sums[time.series,1,,])
  CPUE.female.small <- t(tmp.female.small*1e3/effort.sums[time.series,1,,])
  
  CPUE.male.large <- t(tmp.male.large*1e3/effort.sums[time.series,1,,])
  CPUE.female.large <- t(tmp.female.large*1e3/effort.sums[time.series,1,,])
  
  annual.cpue.small.m <- (male.small.annual*1e3)/effort.total[time.series]
  annual.cpue.small.f <- (female.small.annual*1e3)/effort.total[time.series]
  annual.cpue.large.m <- (male.large.annual*1e3)/effort.total[time.series]
  annual.cpue.large.f <- (female.large.annual*1e3)/effort.total[time.series]


  ##  PLOTS  ##

  col.scale <- gray(c(0.1,0.3,0.6,0.9))
  bar.yrs <- seq(along=time.series)*5-2

  par(mfrow=c(2,2), xpd=TRUE)


  # Male CPUE small
  barplot(CPUE.male.small, beside=T, col=col.scale, ylim=c(0,max(CPUE.male.small)+max(CPUE.male.small*0.2)))
  lines(x=bar.yrs, y=annual.cpue.small.m, type="b", col="red")
  title(main="CPUE - Males < 35mm CL", xlab="", ylab="CPUE (kg / hour trawling)")
  legend(x=-1, y=-25, legend=c("Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4", "Annual"), cex=0.8, pt.cex=0.8,
    pch=c(15,15,15,15,21,NA), lty=c(NA,NA,NA,NA,1), col=c(col.scale, "red"), bty="n", ncol=5, x.intersp =0.5, text.width=7)

  # Female CPUE small
  barplot(CPUE.female.small, beside=T, col=col.scale, ylim=c(0,max(CPUE.female.small)+max(CPUE.female.small*0.2)))
  lines(x=bar.yrs, y=annual.cpue.small.f, type="b", col="red")
  title(main="CPUE - Females < 35mm CL", xlab="", ylab="CPUE (kg / hour trawling)")
  legend(x=-1, y=-35, legend=c("Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4", "Annual"), cex=0.8, pt.cex=0.8,
    pch=c(15,15,15,15,21,NA), lty=c(NA,NA,NA,NA,1), col=c(col.scale, "red"), bty="n", ncol=5, x.intersp =0.5, text.width=7)

  # Male CPUE large
  barplot(CPUE.male.large, beside=T, col=col.scale, ylim=c(0,max(CPUE.male.large)+max(CPUE.male.large*0.2)))
  lines(x=bar.yrs, y=annual.cpue.large.m, type="b", col="red")
  title(main="CPUE - Males > 35mm CL", xlab="", ylab="CPUE (kg / hour trawling)")
  legend(x=-1, y=-25, legend=c("Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4", "Annual"), cex=0.8, pt.cex=0.8,
    pch=c(15,15,15,15,21,NA), lty=c(NA,NA,NA,NA,1), col=c(col.scale, "red"), bty="n", ncol=5, x.intersp =0.5, text.width=7)

  # Female CPUE large
  barplot(CPUE.female.large, beside=T, col=col.scale, ylim=c(0,max(CPUE.female.large)+max(CPUE.female.large*0.2)))
  lines(x=bar.yrs, y=annual.cpue.large.f, type="b", col="red")
  title(main="CPUE - Females > 35mm CL", xlab="", ylab="CPUE (kg / hour trawling)")
  legend(x=-1, y=-20, legend=c("Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4", "Annual"), cex=0.8, pt.cex=0.8,
    pch=c(15,15,15,15,21,NA), lty=c(NA,NA,NA,NA,1), col=c(col.scale, "red"), bty="n", ncol=5, x.intersp =0.5, text.width=7)

}

