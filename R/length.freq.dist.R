`length.freq.dist` <-
function(neph.object, av.years) {


av.years <- as.character(av.years)

# landings summed by year and averaged over last 3 years
tmp <- annual.quant(neph.object@landings.n)
tmp.male <- tmp[,,1,,]
landings.sum.m <- (rowSums(tmp.male[,av.years,,,])) /3
tmp.female <- tmp[,,2,,]
landings.sum.f <- (rowSums(tmp.female[,av.years,,,]))/3

# discards summed by years and averaged over last 3 years

discs <- annual.quant(neph.object@discards.n)
discs.male <- discs[,,1,,]
discs.sum.m <- (rowSums(discs.male[,av.years,,,])) /3
discs.female <- discs[,,2,,]
discs.sum.f <- (rowSums(discs.female[,av.years,,,]))/3

axis.col <- grey(0.5)
x.axis <- as.numeric(rownames(tmp))
x.limit <- c(0, max(x.axis)+max(x.axis*0.1))
y.limit.m <- c(0, max(landings.sum.m)+max(landings.sum.m*0.1))
y.limit.f <- c(0, max(landings.sum.f)+max(landings.sum.f*0.1))

## PLOTS ##

par(mfrow=c(1,2))

# Males
plot(x.axis, landings.sum.m, type="n", axes=F, xlab="", ylab="", xlim=x.limit, ylim=y.limit.m)
lines(x.axis,landings.sum.m, type="l", xlab="Carapace length", ylab="Numbers")
lines(x.axis, discs.sum.m, type="l", lty=4, col="red")
title(main="Males", xlab="Carapace length", ylab="Numbers")
axis(1, col=axis.col)
axis(2, col=axis.col)
box(bty="l", col=axis.col)

legend("topright", legend=c("Landings", "Discards"), lty=c(1,4),
  col=c("black", "red"), bty="n")


# Females
plot(x.axis, landings.sum.f, type="n", axes=F, xlab="", ylab="", xlim=x.limit, ylim=y.limit.f)
lines(x.axis,landings.sum.f, type="l", xlab="Carapace length", ylab="Numbers")
lines(x.axis, discs.sum.f, type="l", lty=4, col="red")
title(main="Females", xlab="Carapace length", ylab="")
axis(1, col=axis.col)
axis(2, col=axis.col)
box(bty="l", col=axis.col)

legend("topright", legend=c("Landings", "Discards"), lty=c(1,4),
  col=c("black", "red"), bty="n")
}

