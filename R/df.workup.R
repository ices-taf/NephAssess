df.workup<-
function(wk.dir, dat.dir, counts.file, index, verbose = FALSE, na.rm = TRUE, field.w)
{
  
  require(sp)
  options(digits = 10)
  
  file.list <- readLines(paste(dat.dir, index, sep=""))

  
  no.stations<- as.integer(file.list[1])
  
  # strip off header info
  
  file.list <- file.list[-(1)]
  file.list <- file.list[file.list != ""]

  
  
#  if (any(is.na(c(front.height, back.height, rangefinder.height, camera.length,
#                  horizontal.angle, vertical.angle, camera.angle,
#                  height.differential, no.stations))))
#      stop("The values for sledge parameters have not been correctly formatted, check your file and try again")
  
  if (length(file.list) != no.stations) 
      stop("INCORRECT NUMBER OF FILES IN INDEX")

  file.list <- matrix(file.list, ncol = 1, byrow = TRUE) 
  # a possibility to check file pairing consistency here ...

  nsets <- nrow(file.list)
  seq.nsets <- seq.int(nsets)
  
  ## reads in each count file and corresponding DAT file
  ## some (at least one) files are space delimited, most are comma, some have extra commas
  ## read.csv is more robust than read.table ...
  
     
    
   pos.files <- 
    lapply(file.list,
      function (x, verbose)
      {
	      pos.file <- read.csv(paste(dat.dir, x, ".dat", sep=""), skip=2, header=F)
		  nav.file<- read.csv(paste(dat.dir, x, ".nav", sep=""), skip=2, header=F)
	      
		# To account to the fact that at times the dropframe DAT files so not have any info on depth, rangefinder and distance
		pos.file[,c("V5","V6","V7")]<- 0
		  
		  if (verbose) print(x)
          # if any columns are all NAs, then extra commas present
          which.ok <- which(sapply(pos.file, function(x) !all(is.na(x))))
          if (!all(which.ok[1] : which.ok[length(which.ok)] == which.ok)) 
              stop (gettextf("column of NAs in middle of data in: %s", file.list[(i*2)-1]))
          pos.file <- pos.file[which.ok]
      
          if (dim(pos.file)[2] > 7) stop (gettextf("more than 7 columns of data in: %s", file.list[i,1]))
      
          if (dim(pos.file)[2] != 7L )
          {
            # then file not comma separated so try tab or space... 
            pos.file <- read.csv(x, skip=2, header=F, sep = "")
          }
      
          names(pos.file) <- c("date","time","lat","lon","depth","range","distance")
		  pos.file$date<- as.character(pos.file$date)
		  suppressWarnings (pos.file $ range<- as.numeric(as.character(pos.file $ range)))
		  suppressWarnings (pos.file $ distance<- as.numeric(as.character(pos.file $ distance)))
		  suppressWarnings (pos.file $ depth<- as.numeric(as.character(pos.file $ depth)))
		  pos.file $ distance[is.na(pos.file $ distance)]<- 0		
		  
           nav.file$lat<- as.numeric(substr(nav.file$V3, 1, 2)) + as.numeric(substr(nav.file$V3, 4, 10))/60
		   nav.file$lon<- as.numeric(substr(nav.file$V4, 1, 3)) + as.numeric(substr(nav.file$V4, 5, 11))/60
		   nav.file$lon<- ifelse(substr(nav.file$V4, 13, 13) == "W", nav.file$lon,
				ifelse(substr(nav.file$V4, 13, 13) == "E", nav.file$lon*(-1), NA))
		   
		   nav.file$V3<- NULL
		   nav.file$V4<- NULL
		   names(nav.file) <- c("date","time","lat","lon")
		   nav.file$time<- as.character(nav.file$time)
		  
		   pos.file<- pos.file[pos.file$date %in% nav.file$time, ]
		  
		  
		  for(i in 1:dim(pos.file)[1])
		  {
				if(i==1) {pos.file[i,"distance"]<- 0}
				
				if(i>1)
				{		min.pos1<- pos.file[i,"date"]
						min.pos0<- pos.file[i-1,"date"]
				
						nav1<- subset(nav.file, time==min.pos1)
						nav0<- subset(nav.file, time==min.pos0)
						
						lat.nav1<- mean(nav1$lat)
						lat.nav0<- mean(nav0$lat)
						lon.nav1<- mean(nav1$lon)
						lon.nav0<- mean(nav0$lon)
						
						dist.m<- 1000*spDistsN1(matrix(c(lon.nav1, lat.nav1), ncol=2), c(lon.nav0, lat.nav0), longlat=TRUE)
						pos.file[i,"distance"]<- dist.m + pos.file[i-1,"distance"]
				}
		  }
		 
          pos.file
      }, verbose = verbose)

	  
	  
	
	counts<- read.csv(counts.file, header=T)
  # read count files 
  count.files <- 
    lapply(file.list, 
      function (x) 
      {
        count.file<- subset(counts, Station==x, select=c("Min", "Counter_ID", "Count", "Sec_not_counted"))
		count.file$Sec_not_counted[is.na(count.file$Sec_not_counted)]=0 
		count.file$Count<- round(60*count.file$Count/(60-count.file$Sec_not_counted), 1)
		count.file$Count<- ifelse(count.file$Sec_not_counted == 60, NA, count.file$Count)
		count.file$Sec_not_counted<- NULL
		count.file<- reshape(count.file, direction="wide", idvar=c("Min"), timevar="Counter_ID")
		count.file<- count.file[order(count.file$Min),]
		count.file<- count.file[!is.na(count.file$Min)==T,]
		
		add.n.cols<- 5-dim(count.file)[2]
		if(add.n.cols>=0)
		{
			mat<- matrix(NA, nrow=dim(count.file)[1], ncol=add.n.cols)
			count.file<- cbind(count.file, mat)
		} else
		
		{
		count.file<- count.file[,1:5]
		}
		names(count.file) <- c("Mins", "C1", "C2", "C3", "C4")
		dat.file<- read.csv(paste(dat.dir, x, ".dat", sep=""), skip=2, header=F)
		count.file<- subset(count.file, Mins<=floor(max(dat.file$V2,na.rm=na.rm)/60))	
		count.file
      })
  
  
  #Replace "xxx" by NAs to make columns C1 and C2 numeric
  for(i in seq.nsets)
	{
		for(j in 2:5)
		{
			suppressWarnings(count.files[[i]][,j] <- as.numeric(as.character(count.files[[i]][,j])))
		}
	}
  
  

  
  ## calculate info for count records

  minutes <- sapply(count.files, nrow)
  
  pos.files <- lapply(pos.files, function(x) {if (x $ distance[1] >= 30) x $ distance[1] <- 0; x} )
  start.dist <- sapply(pos.files, function(x) x $ distance[1])
  
  distance.covered <- vector("list", nsets)

  
    for (i in seq.nsets)
    {
      for (j in seq.int(minutes[i]))
      {
        temp.mat <- pos.files[[i]][ pos.files [[i]] $ time <=  count.files [[i]] $ Min[j]      * 60  & 
                                    pos.files [[i]] $ time >= (count.files [[i]] $ Min[j] - 1) * 60, ]
        distance.covered[[i]] [j] <- temp.mat $ distance[nrow(temp.mat)] - start.dist[i]
        start.dist[i] <- temp.mat $ distance[nrow(temp.mat)]
      }
    }

	
	distance.covered.2<- vector("list", nsets)
	for (i in seq.nsets)
    {
      for (j in seq.int(minutes[i]))
      {
		d.i<- distance.covered[[i]]
		
		if (distance.covered[[i]][j] <= 0 | distance.covered[[i]][j] > 50) 
		{
			distance.covered.2[[i]][j]<- round(mean(d.i[d.i >= 0 & d.i <= 50], na.rm = na.rm), 0)
		} else
		{
			distance.covered.2[[i]][j]<- distance.covered[[i]][j]
		}
      }
    }
	distance.covered<- distance.covered.2
	
	 view.width.station<- rep(field.w, nsets)
	 view.width<- vector("list", nsets)
	 view.width<- lapply(view.width.station, function(x) x)
	 area <- lapply(seq.nsets, function(i) view.width[[i]] * distance.covered[[i]])

	
	##This ensures that if there are no counts at all for a given minute in a given station (eg no visibility) then, the calculated area for that minute/station is set to NA
	for (i in seq.nsets)
    {
      for (j in seq.int(minutes[i]))
      {
		if (all(is.na(count.files[[i]][j, c("C1", "C2", "C3", "C4")])) == T)
		{
			area[[i]][j]<- NA
		} 
      }
    }
		
    lats <- sapply(pos.files, function(x) x $ lat[1])
    lons <- sapply(pos.files, function(x) x $ lon[1])

    viewed.area <- sapply(area, sum, na.rm = na.rm)
    run.length <- sapply(distance.covered, sum)

    # was previously done this way - but this method ignores area weighting and results are slightly biased as a result.
    counter.1.density <- sapply(lapply(seq_along(pos.files), function(i) count.files[[i]] $ C1 / area[[i]]), mean, na.rm = na.rm)
    counter.2.density <- sapply(lapply(seq_along(pos.files), function(i) count.files[[i]] $ C2 / area[[i]]), mean, na.rm = na.rm)
    counter.3.density <- sapply(lapply(seq_along(pos.files), function(i) count.files[[i]] $ C3 / area[[i]]), mean, na.rm = na.rm)
    counter.4.density <- sapply(lapply(seq_along(pos.files), function(i) count.files[[i]] $ C4 / area[[i]]), mean, na.rm = na.rm)
    
	
	##Calculates an average of the counts per minute 
	for (i in seq.nsets)
	{
		count.files [[i]]$average<- numeric(minutes[i])
		
		for (j in seq.int(minutes[i]))
		{
			count.files[[i]][j, "average"]<- round(mean(c(count.files[[i]][j, "C1"], count.files[[i]][j, "C2"], count.files[[i]][j, "C3"], count.files[[i]][j, "C4"]), na.rm = na.rm), 1)
		}
	}

	##Sums the average count and divides by the total viewed area for each station
	average.density <- sapply(count.files, function(x) sum(x$average, na.rm = na.rm)) / viewed.area

    ## make output list
    out <-
      list(
        lats = lats,
        lons = lons,
        viewed.area = viewed.area,
        distance.covered = round(run.length),
        average.density  = round(average.density, 2),
        count.1 = round(counter.1.density, 2),
        count.2 = round(counter.2.density, 2)
      )
   
  
  as.data.frame(out)
}
