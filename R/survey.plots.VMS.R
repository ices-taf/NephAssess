survey.plot.VMS <-

function(wk.dir, survey.year, coast.file, fu.dens, fu.dens.vms, f.u = c("fladen", "moray", "forth", "north minch", "south minch", "clyde"), jura = F, jura.dens = NULL, bubble.scale = 1)
{
	old.dir <- setwd(wk.dir)
	on.exit(setwd(old.dir), add = TRUE)

	uk.coast <- read.table("scottish_coast.txt", header=T)

	zero.dens<- subset(fu.dens, average.density == 0)
	
	
	get.fname <-
	function (base.name)
	{
		fname <- function(i) paste(base.name, i, ".jpg", sep="")
		out <- fname(i <- 1)
		while (file.exists(out))
		out <- fname(i <- i + 1)
		return ( out )
	}
	
	jpeg(get.fname(paste(wk.dir, f.u, "_survey.plot.VMS", sep = "")), width = 640, height = 640, 
		pointsize = 12, bg = "white", res = NA, quality=100)
	
	if (f.u== "fladen")  
	{
		
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-2,2), ylim=c(57.5, 60.5), cex.axis=1.5, cex.lab=1.5)
		#, main="Fladen")
		#mtext(side=3, text="FU 07 Management Unit G", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")
  
		lines(x=c(-1,2), y=c(57.5,57.5), lwd=2, lty=2)
		lines(x=c(2,2), y=c(57.5,60.5), lwd=2, lty=2)
		lines(x=c(-1,2), y=c(60.5,60.5), lwd=2, lty=2)
		lines(x=c(-1,-1), y=c(59,60.5), lwd=2, lty=2)
		lines(x=c(-2,-1), y=c(59,59), lwd=2, lty=2)
		lines(x=c(-2,-2), y=c(59,58), lwd=2, lty=2)
		lines(x=c(-2,-1), y=c(58,58), lwd=2, lty=2)
		lines(x=c(-1,-1), y=c(58,57.5), lwd=2, lty=2)
		
		lapply(fladen.poly[6:36],polygon,col="olivedrab",border=NA)
		lapply(fladen.poly[39:67],polygon,col="skyblue",border=NA)
		lapply(fladen.poly[86:102],polygon,col="green",border=NA)
		lapply(fladen.poly[75:85],polygon,col="skyblue",border=NA)
		lapply(fladen.poly[c(15,21)],polygon,col="olivedrab",border=NA)
		lapply(fladen.poly[1:5],polygon,col="darkgreen",border=NA)
		lapply(fladen.poly[37:38],polygon,col="skyblue",border=NA)
		lapply(fladen.poly[86],polygon,col="green",border=NA)
		lapply(fladen.poly[93],polygon,col="green",border=NA)
		lapply(fladen.poly[16],polygon,col="olivedrab",border=NA)
		
		polygon(uk.coast, col="grey45")
		points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=2)
		points(fu.dens.vms$lons, fu.dens.vms$lats, cex=bubble.scale*fu.dens.vms$average.density, lwd=2, col=4)
		points(zero.dens$lons, zero.dens$lats, pch=4, lwd=1, col=2, cex=1)
		title(survey.year, cex.main=3.5)
		box()
	
	
	} else
	
	
	if (f.u== "moray")  
	{
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-4.2, -0.9), ylim=c(57.45, 58.55), cex.axis=1.5, cex.lab=1.5)
		#, main="Moray Firth")
		#mtext(side=3, text="FU 09 Management Unit F", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")
		
		lines(x=c(-4,-2), y=c(58.5,58.5), lwd=2, lty=2)
		lines(x=c(-2,-2), y=c(58.5,58), lwd=2, lty=2)
		lines(x=c(-2,-1), y=c(58,58), lwd=2, lty=2)
		lines(x=c(-1,-1), y=c(58,57.5), lwd=2, lty=2)
		lines(x=c(-1,-2), y=c(57.5,57.5), lwd=2, lty=2)
		
		lapply(moray.poly[2:8],polygon,col="olivedrab",border=NA)
		lapply(moray.poly[9:20],polygon,col="skyblue",border=NA)
		lapply(moray.poly[21:25],polygon,col="green",border=NA)
		lapply(moray.poly[1],polygon,col="darkgreen",border=NA)

		polygon(uk.coast, col="grey45")
		points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=2)
		points(fu.dens.vms$lons, fu.dens.vms$lats, cex=bubble.scale*fu.dens.vms$average.density, lwd=2, col=4)
		points(zero.dens$lons, zero.dens$lats, pch=4, lwd=1, col=2, cex=1)
		title(survey.year, cex.main=3.5)
		box()

	} else

	
	if (f.u== "forth")
	{
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-3.5,-2.0), ylim=c(55.9,56.8), cex.axis=1.5, cex.lab=1.5)
		#, main="Firth of Forth")
		#mtext(side=3, text="FU 08 Management Unit I", cex=0.7, line=0.5)
		polygon(x=c(-5, 0,0,-5), y=c(55,55,60,60), col="skyblue")

		lines(x=c(-2,-2), y=c(56.5,55.5), lwd=2, lty=2)
		lines(x=c(-3,-2), y=c(56.5,56.5), lwd=2, lty=2)
		
		lapply(forth.poly[5:16],polygon,col="olivedrab",border=NA)
		lapply(forth.poly[23:53],polygon,col="skyblue",border=NA)
		lapply(forth.poly[54:66],polygon,col="green",border=NA)
		lapply(forth.poly[17:22],polygon,col="skyblue",border=NA)
		lapply(forth.poly[1:4],polygon,col="darkgreen",border=NA)
  
		polygon(uk.coast, col="grey45")
		points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=2)
		points(fu.dens.vms$lons, fu.dens.vms$lats, cex=bubble.scale*fu.dens.vms$average.density, lwd=2, col=4)
		points(zero.dens$lons, zero.dens$lats, pch=4, lwd=1, col=2, cex=1)
		title(survey.year, cex.main=3.5)
		box()
  
	} else
	
	
	if (f.u== "clyde")  
	{
		
		if(jura == F)
		{
			plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-6.1,-4.5), ylim=c(54.9,56.1), cex.axis=1.5, cex.lab=1.5)
			#, main="Firth of Clyde & Sound of Jura")
			#mtext(side=3, text="FU 13 Management Unit C", cex=0.7, line=0.5)
			polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")

			lines(x=c(-6,-6), y=c(56,55), lwd=2, lty=2)
			lines(x=c(-6,-5), y=c(55,55), lwd=2, lty=2)
			lines(x=c(-6,-5.5), y=c(56,56), lwd=2, lty=2)
			
			#Clyde
			lapply(clyde.poly[26:35],polygon,col="olivedrab",border=NA)
			lapply(clyde.poly[1:16],polygon,col="skyblue",border=NA)
			lapply(clyde.poly[44:49],polygon,col="green",border=NA)
			lapply(clyde.poly[17:25],polygon,col="darkgreen",border=NA)

			polygon(uk.coast, col="grey45")
			points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=2)
			points(fu.dens.vms$lons, fu.dens.vms$lats, cex=bubble.scale*fu.dens.vms$average.density, lwd=2, col=4)
			points(zero.dens$lons, zero.dens$lats, pch=4, lwd=1, col=2, cex=2)
			title(survey.year, cex.main=3.5)
			box()
		}

		if(jura == T)
		{
		
			jura.dens<- subset(jura.dens, strata_type != FALSE)
			jura.zero.dens<- subset(jura.dens, average.density == 0)
			
			plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-6.1,-4.5), ylim=c(54.9,56.1), cex.axis=1.5, cex.lab=1.5)
			#, main="Firth of Clyde & Sound of Jura")
			#mtext(side=3, text="FU 13 Management Unit C", cex=0.7, line=0.5)
			polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")

			lines(x=c(-6,-6), y=c(56,55), lwd=2, lty=2)
			lines(x=c(-6,-5), y=c(55,55), lwd=2, lty=2)
			lines(x=c(-6,-5.5), y=c(56,56), lwd=2, lty=2)
			
			#Clyde
			lapply(clyde.poly[26:35],polygon,col="olivedrab",border=NA)
			lapply(clyde.poly[1:16],polygon,col="skyblue",border=NA)
			lapply(clyde.poly[44:49],polygon,col="green",border=NA)
			lapply(clyde.poly[17:25],polygon,col="darkgreen",border=NA)

			#Sound of Jura
			lapply(jura.poly[3:4],polygon,col="olivedrab",border=NA)
			lapply(jura.poly[7],polygon,col="green",border=NA)
			lapply(jura.poly[5:6],polygon,col="olivedrab",border=NA)
			lapply(jura.poly[1],polygon,col="skyblue",border=NA)
			lapply(jura.poly[2],polygon,col="darkgreen",border=NA)
			
			polygon(uk.coast, col="grey45")
			points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=2)
			points(fu.dens.vms$lons, fu.dens.vms$lats, cex=bubble.scale*fu.dens.vms$average.density, lwd=2, col=4)
			points(zero.dens$lons, zero.dens$lats, pch=4, lwd=1, col=2, cex=1)
			points(jura.dens$lons, jura.dens$lats, cex=bubble.scale*jura.dens$average.density, lwd=2)
			points(jura.zero.dens$lons, jura.zero.dens$lats, pch=4, lwd=1, col=2, cex=1)
			title(survey.year, cex.main=3.5)
			box()	
		}	
	
	} else
	
	
	if (f.u== "north minch")  
	{
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-7.2, -4.8), ylim=c(57.3, 59), cex.axis=1.5, cex.lab=1.5)
		#, main="North Minch")
		#mtext(side=3, text="FU 11 Management Unit C", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")

		lines(x=c(-7,-5), y=c(57.5,57.5), lwd=2, lty=2)
		lines(x=c(-7,-5), y=c(59,59), lwd=2, lty=2)
		lines(x=c(-7,-7), y=c(57.5,59), lwd=2, lty=2)
		lines(x=c(-5,-5), y=c(57.5,59), lwd=2, lty=2)
		
		lapply(north.minch.poly[18:35],polygon,col="olivedrab",border=NA)
		lapply(north.minch.poly[1:7],polygon,col="skyblue",border=NA)
		lapply(north.minch.poly[36:56],polygon,col="green",border=NA)
		lapply(north.minch.poly[8:17],polygon,col="darkgreen",border=NA)
		
		polygon(uk.coast, col="grey45")
		points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=2)
		points(fu.dens.vms$lons, fu.dens.vms$lats, cex=bubble.scale*fu.dens.vms$average.density, lwd=2, col=4)
		points(zero.dens$lons, zero.dens$lats, pch=4, lwd=1, col=2, cex=1)
		title(survey.year, cex.main=3.5)
		box()
				
	
	} else
		
		
	if (f.u== "south minch")  
	{
	
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-8,-5), ylim=c(56,57.5), cex.axis=1.5, cex.lab=1.5)
		#, main="Fladen")
		#mtext(side=3, text="FU 12 Management Unit C", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")

		lines(x=c(-8,-5), y=c(57.5,57.5), lwd=2, lty=2)
		lines(x=c(-8,-5.5), y=c(56,56), lwd=2, lty=2)
		lines(x=c(-8,-8), y=c(57.5,56), lwd=2, lty=2)
		
		lapply(south.minch.poly[17:50],polygon,col="olivedrab",border=NA)
		lapply(south.minch.poly[51],polygon,col="skyblue",border=NA)
		lapply(south.minch.poly[53:72],polygon,col="green",border=NA)
		lapply(south.minch.poly[52],polygon,col="skyblue",border=NA)
		lapply(south.minch.poly[1:16],polygon,col="darkgreen",border=NA)

		polygon(uk.coast, col="grey45")
		points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=2)
		points(fu.dens.vms$lons, fu.dens.vms$lats, cex=bubble.scale*fu.dens.vms$average.density, lwd=2, col=4)
		points(zero.dens$lons, zero.dens$lats, pch=4, lwd=1, col=2, cex=1)
		title(survey.year, cex.main=3.5)
		box()
	
	}
	
	dev.off()

}
	
	
	