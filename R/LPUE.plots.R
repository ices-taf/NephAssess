`LPUE.plots` <-
function(neph.object, effort.obj, start.year) {

  ##  landings weight
  landings.weight <- neph.object@landings.n*neph.object@stock.wt
  land.sums <- colSums(landings.weight)
  annual.land.sums <- land.sums[,,1,]+land.sums[,,2,]+land.sums[,,3,]+land.sums[,,4,]
  annual.land.sums.all <- rowSums(annual.land.sums)

  ##  effort
  effort.sums <-  colSums(effort.obj)
  effort.total <- effort.sums[,,1,]+effort.sums[,,2,]+effort.sums[,,3,]+effort.sums[,,4,]

  ##  LPUE
  tmp.male <- land.sums[,1,,]
  total.male.landing <- rowSums(tmp.male)
  tmp.female <- land.sums[,2,,]
  total.female.landing <- rowSums(tmp.female)
  total.landings.all <- t(cbind(total.male.landing, total.female.landing))
  LPUE.male <- t(tmp.male*1e3/effort.sums[,1,,])
  LPUE.female <- t(tmp.female*1e3/effort.sums[,1,,])
  annual.lpue <- (annual.land.sums*1e3)/effort.total
  #average.lpue <- annual.LPUE/4

  #curtail data series
  time.series <- as.character(seq(start.year,neph.object@range["maxyear"]))
  LPUE.male <- LPUE.male[,time.series]
  LPUE.female <- LPUE.female[,time.series]

  ##  PLOTS  ##

  col.scale <- gray(c(0.1,0.3,0.6,0.9))
  col.scale.2 <- gray(c(0.1,0.9))
  bar.yrs <- seq(along=time.series)*5-2
  bar.yrs.2 <- seq(along=time.series)*3-2
  par(mfrow=c(2,2), xpd=TRUE)
  
  # Landings
  barplot(total.landings.all[,time.series], beside=T, ylim=c(0, max(annual.land.sums.all[time.series])+max(annual.land.sums.all[time.series]*0.2)))
  lines(x=bar.yrs.2, y=annual.land.sums.all[time.series], col="red", type="b")
  title(main="Landings", xlab="", ylab="Landings (tonnes)")
  legend(x=5, y=-3000, legend=c("Males", "Females", "Total"), cex=0.8, pt.cex=0.8,
    pch=c(15,15,21,NA), lty=c(NA,NA,1), col=c(col.scale.2, "red"), bty="n", ncol=3, x.intersp =0.5, text.width=7)

  # Effort
  effort.plotting <- t(effort.sums[time.series,1,,]/1e3)
  barplot(effort.plotting, beside=T, col=col.scale, ylim=c(0, max(effort.total[time.series]/1e3)+max(effort.total[time.series]/1e3*0.3)))
  lines(x=bar.yrs, y=effort.total[time.series]/1e3, col="red", type="b")
  title(main="Effort", xlab="", ylab="Effort ('000 hours)")
  legend(x=-1, y=-25, legend=c("Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4", "Annual"), cex=0.8, pt.cex=0.8,
    pch=c(15,15,15,15,21,NA), lty=c(NA,NA,NA,NA,1), col=c(col.scale, "red"), bty="n", ncol=5, x.intersp =0.5, text.width=7)

  # Male LPUE
  barplot(LPUE.male, beside=T, col=col.scale, ylim=c(0,max(LPUE.male)+max(LPUE.male*0.1)))
  lines(x=bar.yrs, y=annual.lpue[time.series,1], type="b", col="red")
  title(main="LPUE - Males", xlab="", ylab="LPUE(kg/hour trawling)")
  legend(x=-1, y=-30, legend=c("Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4", "Annual"), cex=0.8, pt.cex=0.8,
    pch=c(15,15,15,15,21,NA), lty=c(NA,NA,NA,NA,1), col=c(col.scale, "red"), bty="n", ncol=5, x.intersp =0.5, text.width=7)

  # Female LPUE
  barplot(LPUE.female, beside=T, col=col.scale, ylim=c(0,max(LPUE.female)+max(LPUE.female*0.1)))
  lines(x=bar.yrs, y=annual.lpue[time.series,2], type="b", col="red")
  title(main="LPUE - Females", xlab="", ylab="LPUE(kg/hour trawling)")
  legend(x=-1, y=-30, legend=c("Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4", "Annual"), cex=0.8, pt.cex=0.8,
    pch=c(15,15,15,15,21,NA), lty=c(NA,NA,NA,NA,1), col=c(col.scale, "red"), bty="n", ncol=5, x.intersp =0.5, text.width=7)

}

