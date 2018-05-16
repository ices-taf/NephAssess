survey.plot <-

function(wk.dir, survey.year, coast.file, fu.dens, f.u = c("fladen", "moray", "forth", "north minch", "south minch", "clyde", "devils hole", "noup"), jura = F, jura.dens = NULL, bubble.scale = 1, legend = NULL)
{
	old.dir <- setwd(wk.dir)
	on.exit(setwd(old.dir), add = TRUE)

	uk.coast <- uk.coast

	get.fname <-
	function (base.name)
	{
		fname <- function(i) paste(base.name, i, ".jpg", sep="")
		out <- fname(i <- 1)
		while (file.exists(out))
		out <- fname(i <- i + 1)
		return ( out )
	}
	
	jpeg(get.fname(paste(wk.dir, f.u, "_survey.plot_",survey.year,".", sep = "")), width = 640, height = 640, 
		pointsize = 12, bg = "white", res = NA, quality=100)
	
	if (f.u== "fladen")  
	{
		
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-2,2), ylim=c(57.5, 60.5))
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
		
#		points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=2)
    points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0], 
        cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=2)
    points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
		polygon(uk.coast, col="grey45")
		title(survey.year, cex.main=3.5)
		box()
	
	
	} else
	
	
	if (f.u== "moray")  
	{
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-4.2, -0.9), ylim=c(57.45, 58.55))
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

		points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0],
         cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=2)
		points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
    polygon(uk.coast, col="grey45")
		title(survey.year, cex.main=3.5)
		box()

	} else

	
	if (f.u== "forth")
	{
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-3.5,-2.0), ylim=c(55.9,56.8))
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
  
		points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0],
           cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=2)
		points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
    polygon(uk.coast, col="grey45")
		title(survey.year, cex.main=3.5)
		box()
  
	} else
	
	
	if (f.u== "clyde")  
	{
		
		if(jura == F)
		{
			plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-6.1,-4.5), ylim=c(54.9,56.1))
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

			points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0], 
            cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=2)
			points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
      polygon(uk.coast, col="grey45")
			title(survey.year, cex.main=3.5)
			box()
		}

		if(jura == T)
		{
			plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-6.1,-4.5), ylim=c(54.9,56.1))
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
			
			points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0], 
              cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=2)
			points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
      points(jura.dens$lons[jura.dens$average.density>0], jura.dens$lats[jura.dens$average.density>0], 
              cex=bubble.scale*jura.dens$average.density[jura.dens$average.density>0], lwd=2)
			points(jura.dens$lons[jura.dens$average.density==0], jura.dens$lats[jura.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
      polygon(uk.coast, col="grey45")
			title(survey.year, cex.main=3.5)
			box()	
		}	
	
		if(legend == T)
		{
				x=rep(-4.65, 5)
				y=seq(54.9, 55.4, length=5)
				ds<- bubble.scale*fu.dens$average.density[fu.dens$average.density>0]
				z= c(0.1*max(ds),0.25*max(ds),0.5*max(ds),0.75*max(ds),max(ds) )
				z.txt<- round(z/bubble.scale,1)
				points(x, y, cex=z, lwd=2)
				text(x+0.15, y, z.txt, font=2)
		}
	
	
	} else
	
	
	if (f.u== "north minch")  
	{
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-7.2, -4.8), ylim=c(57.3, 59))
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
		
		points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0], 
          cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=2)
		points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
    polygon(uk.coast, col="grey45")
		title(survey.year, cex.main=3.5)
		box()
	
		
	} else
		
		
	if (f.u== "south minch")  
	{
	
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-8,-5), ylim=c(56,57.5))
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

		points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0], 
        cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=2)
		points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
    polygon(uk.coast, col="grey45")
		title(survey.year, cex.main=3.5)
		box()
	
		if(legend == T)
		{
			x=rep(-5.3, 5)
			y=seq(56.0, 56.7, length=5)
			ds<- bubble.scale*fu.dens$average.density[fu.dens$average.density>0]
			z= c(0.1*max(ds),0.25*max(ds),0.5*max(ds),0.75*max(ds),max(ds) )
			z.txt<- round(z/bubble.scale,1)
			points(x, y, cex=z, lwd=2)
			text(x+0.3, y, z.txt, font=2)
		}
	
	
	}  else
		
		
	if (f.u== "devils hole")  
	{
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-0.5,2.5), ylim=c(56,57.5))
		#, main="Fladen")
		#mtext(side=3, text="FU 12 Management Unit C", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")

		lines(x=c(0,2), y=c(56,56), lwd=2, lty=2)
		lines(x=c(2,2), y=c(56,57.5), lwd=2, lty=2)
		lines(x=c(0,0), y=c(56,57.5), lwd=2, lty=2)
		lines(x=c(0,2), y=c(57.5,57.5), lwd=2, lty=2)
		
		lapply(devils.hole.poly[1:12],polygon,col="olivedrab",border=NA)
		lapply(devils.hole.poly[13:22],polygon,col="skyblue",border=NA)

		points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0], 
        cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=2)
		points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
    polygon(uk.coast, col="grey45")
		title(survey.year, cex.main=3.5)
		box()
	
	}
	
	else
		
		
	if (f.u== "noup")  
	{
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-4.5,-2.5), ylim=c(59,59.6))
		#, main="Fladen")
		#mtext(side=3, text="FU 12 Management Unit C", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")

		lines(x=c(-4,-3), y=c(59,59), lwd=2, lty=2)
		lines(x=c(-3,-3), y=c(59,59.5), lwd=2, lty=2)
		lines(x=c(-4,-3), y=c(59.5,59.5), lwd=2, lty=2)
		lines(x=c(-4,-4), y=c(59,59.5), lwd=2, lty=2)
		
		lapply(noup.poly[1:2],polygon,col="olivedrab",border=NA)
		lapply(noup.poly[3:5],polygon,col="skyblue",border=NA)

		points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0], 
        cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=2)
		points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
    polygon(uk.coast, col="grey45")
		title(survey.year, cex.main=3.5)
		box()
	
	}
	
	dev.off()

}
	
	
	
	
	
	