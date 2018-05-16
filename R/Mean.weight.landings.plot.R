plot.mean.weights<- function(wk.dir, st1=NULL, st2=NULL, st3=NULL, st4=NULL, fu.legend)
{
	#Plot mean weights

	setwd(wk.dir)

	range.lim<- 1990

	cols<- c("blue", "green", "red", "orange")
	pchs<- c(22,23,24,21)

	sts<- list(st1,st2,st3,st4)
	n.sts<- length(sts)
	tmp<- numeric(n.sts)
	for(i in 1:n.sts) { tmp[i]<- !is.null(sts[[i]]) }
	cols<- cols[which(tmp==1)]
	pchs<- pchs[which(tmp==1)]

	png(filename="mean.wts.png", width =1000, height = 400, 
		 bg = "white", res = NA)
		 
	plot(0,0, xlim=c(min(st1[st1$Year >= range.lim,1], na.rm=T), max(st1[st1$Year >= range.lim,1], na.rm=T)), ylim=c(0,50), 
		main="Mean weight in landings", xlab="Year", ylab="Mean weight (g)", axes = FALSE, cex.main=1.8, cex.lab=1.5)

	axis(1, at=seq(min(	st1[st1$Year >= range.lim,1], na.rm=T), max(st1[st1$Year >= range.lim,1], na.rm=T), by=2), cex.axis=1.2)
	axis(2, cex.axis=1.2)
	box( bty = "L") 

	lines(st1[st1$Year >= range.lim,1], st1[st1$Year >= range.lim,2], lwd=2, col=cols[1])
	points(st1[st1$Year >= range.lim,1], st1[st1$Year >= range.lim,2], pch=pchs[1], col=cols[1], bg=cols[1], cex=1.3)

	lines(st2[st2$Year >= range.lim,1], st2[st2$Year >= range.lim,2], lwd=2, col=cols[2])
	points(st2[st2$Year >= range.lim,1], st2[st2$Year >= range.lim,2], pch=pchs[2], col=cols[2], bg=cols[2], cex=1.3)

	lines(st3[st3$Year >= range.lim,1], st3[st3$Year >= range.lim,2], pch=0, lwd=2, col=cols[3])
	points(st3[st3$Year >= range.lim,1], st3[st3$Year >= range.lim,2], pch=pchs[3], col=cols[3], bg=cols[3], cex=1.3)

	lines(st4[st4$Year >= range.lim,1], st4[st4$Year >= range.lim,2], pch=0, lwd=2, col=cols[4])
	points(st4[st4$Year >= range.lim,1], st4[st4$Year >= range.lim,2], pch=pchs[4], col=cols[4], bg=cols[4], cex=1.3)

	legend("topleft", legend=fu.legend, pch=pchs, 
		pt.bg = cols, col = cols, lty=1, bty="n", ncol=3, cex=1.3, inset=0.05)

	dev.off()
}
