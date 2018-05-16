#02/08/2013 added 5th counter burrow densities in the output data frame
#29/07/2014 added an argument for removing counters/stations (over/undercounting) from workup)

tv.workup.angle <-
function(wk.dir, database, index = "index.txt", do.plots = TRUE, verbose = FALSE, na.rm = TRUE, old = TRUE, subset = FALSE, counters.removed=NULL, wg=FALSE, station.names=FALSE)
{
  old.dir <- setwd(wk.dir)
  on.exit(setwd(old.dir), add = TRUE)

  # read index file  

  file.list <- readLines(index)

  # header information
  cruise <- file.list[1]

  functional.unit <- file.list[2]

  # sledge parameters
  
  tmp <-  as.numeric(strsplit(file.list[3], ",")[[1]])
  front.height         <- tmp[1]
  camera.angle          <- tmp[2]
  rangefinder.height   <- tmp[3]
  horizontal.angle     <- tmp[4]
  vertical.angle       <- tmp[5]
  height.differential  <- (rangefinder.height-front.height) / 100
  lower.edge.view      <- camera.angle - 0.5 * vertical.angle
  no.stations          <- as.integer(file.list[4])

  # strip off header info
  
  file.list <- file.list[-(1:4)]
  file.list <- file.list[file.list != ""]

  if (is.na(cruise))
      stop("There are problems with your index file - please check it is of standard format and try again") 
  
  if (any(is.na(c(front.height, camera.angle, rangefinder.height,
                  horizontal.angle, vertical.angle,
                  height.differential, no.stations))))
      stop("The values for sledge parameters have not been correctly formatted, check your file and try again")
  
  if (length(file.list) != no.stations) 
      stop("INCORRECT NUMBER OF FILES IN INDEX")

  file.list <- matrix(file.list, ncol = 1, byrow = TRUE) 
  # a possibility to check file pairing consistency here ...

  nsets <- nrow(file.list)
  seq.nsets <- seq.int(nsets)
  
  ## reads in each count file and corresponding DAT file
  ## some (at least one) files are space delimited, most are comma, some have extra commas
  ## read.csv is more robust than read.table ...
  
  #Nephrops database connection
  db<- odbcConnect(database)
  #db<- odbcConnectAccess(database) 
  db.tables<- sqlTables(db)
  db.tables<- subset(db.tables, TABLE_NAME %in% c("Dot dat files", "Raw_burrow_counts"))
  DAT<- sqlQuery(db, paste("SELECT * FROM [",db.tables$TABLE_NAME[db.tables$TABLE_NAME=="Dot dat files"],"]",sep="") )
  counts<- sqlQuery(db, paste("SELECT * FROM [",db.tables$TABLE_NAME[db.tables$TABLE_NAME=="Raw_burrow_counts"],"]",sep="") )
  odbcClose(db)
  
  if(subset == TRUE)		#Remove some counters with evidence from 3rd counts 
  {
	remove<- read.table(counters.removed, sep="\t", head=T)
	counts<- subset(counts, !paste(counts$Station, counts$Counter_ID) %in% paste(remove$Station, remove$Counter_ID))
  }
  
    pos.files <- 
    lapply(file.list,
      function (x, verbose)
      {
	      pos.file<- subset(DAT, Station %in% x)
	      pos.file$Station<- NULL
	      pos.file$Date<- NULL
		  
		  if (verbose) print(x)
          # if any columns are all NAs, then extra commas present
          which.ok <- which(sapply(pos.file, function(x) !all(is.na(x))))
          if (!all(which.ok[1] : which.ok[length(which.ok)] == which.ok)) 
              stop (gettextf("column of NAs in middle of data in: %s", x))
          pos.file <- pos.file[which.ok]
      
          if (dim(pos.file)[2] > 7) stop (gettextf("more than 7 columns of data in: %s", x))
      
          if (dim(pos.file)[2] != 7L )
          {
            # then file not comma separated so try tab or space... 
            pos.file <- read.csv(x, skip=2, header=F, sep = "")
          }
      
          names(pos.file) <- c("date","time","lat","lon","depth","range","distance")
		  suppressWarnings (pos.file $ range<- as.numeric(as.character(pos.file $ range)))
		  suppressWarnings (pos.file $ distance<- as.numeric(as.character(pos.file $ distance)))
		  suppressWarnings (pos.file $ depth<- as.numeric(as.character(pos.file $ depth)))
		  pos.file $ distance[is.na(pos.file $ distance)]<- 0		
		  
          if (any( tmp <- is.na(pos.file $ range ))) pos.file $ range [ tmp ] <- rangefinder.height/100
		  
		  if (any(tmp <- pos.file $ range < 0)) pos.file $ range [ tmp ] <- rangefinder.height/100
          
          if (any(tmp <- pos.file $ range < 3))
          {
            pos.file $ range [ !tmp ] <- mean(pos.file $ range [ tmp ])
          } else
          {
            pos.file $ range <- rangefinder.height/100
          }
             
          pos.file
      }, verbose = verbose)

	  
	  
	  
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
		
		add.n.cols<- 6-dim(count.file)[2]
		if(add.n.cols>=0)
		{
			mat<- matrix(NA, nrow=dim(count.file)[1], ncol=add.n.cols)
			count.file<- cbind(count.file, mat)
		} else
		
		{
		count.file<- count.file[,1:6]
		}
		names(count.file) <- c("Mins", "C1", "C2", "C3", "C4", "C5")
        dat.file<- subset(DAT, Station %in% x)
		count.file<- subset(count.file, Mins<=floor(max(dat.file$SecondsElapsed,na.rm=na.rm)/60))	
		count.file
      })
  
  
  #Replace "xxx" by NAs to make columns C1-C5 numeric
  for(i in seq.nsets)
	{
		for(j in 2:5)
		{
			suppressWarnings(count.files[[i]][,j] <- as.numeric(as.character(count.files[[i]][,j])))
		}
	}
  
  # diagnostic plots
  if (do.plots)
  {
   if (!file.exists("Diagnostic Plots")) dir.create("Diagnostic Plots")
    fnames <- paste("diagnostic plots/", file.list, ".png", sep="")
    main <- file.list
    for (i in seq.nsets)
    {
      png( fnames[i] )
        evalq({
          plot( time, distance / max(distance, na.rm = TRUE), 
               ylim = c(0,1), lwd = 2, type = "l", yaxt = "n", bty = "n",
               xlab= "Time (s)", ylab = "", main = gsub(".dat", "", file.list[i,1]) )
          lines( time, range / max(range, na.rm = TRUE), col = 2)
          lines( time, depth / max(depth, na.rm = TRUE), col = 3)
		  }, envir = pos.files[[i]])
        evalq({
          points(Mins * 60 - 9, C1 / max(C1, C2, C3, C4, C5, na.rm = T), type = "h", lwd = 3, col = 4)
          points(Mins * 60 + -3, C2 / max(C1, C2, C3, C4, C5, na.rm = T), type = "h", lwd = 3, col = 5)
		  points(Mins * 60 + 3, C3 / max(C1, C2, C3, C4, C5, na.rm = T), type = "h", lwd = 3, col = 6)
		  points(Mins * 60 + 9, C4 / max(C1, C2, C3, C4, C5, na.rm = T), type = "h", lwd = 3, col = 7)
		  points(Mins * 60 + 15, C5 / max(C1, C2, C3, C4, C5, na.rm = T), type = "h", lwd = 3, col = "brown")
        }, envir = count.files[[i]])
        legend(0, 1, legend=c("Distance", "Range", "Depth", "Count 1", "Count 2", "Count 3", "Count 4"), 
               col = c(1:7), lwd = 2, cex = 0.6)
		max.y<- max(count.files[[i]][, c("C1","C2","C3","C4","C5")], na.rm = T)
		new.scale<- pretty(seq(0, max.y))
		old.scale<- new.scale/max.y
		if(max.y>0) { axis(2, at=old.scale, lab=new.scale) }
      dev.off()
    }
  }

  
  ## calculate info for count records

  minutes <- sapply(count.files, nrow)
  
  pos.files <- lapply(pos.files, function(x) {if (x $ distance[1] >= 30) x $ distance[1] <- 0; x} )
  start.dist <- sapply(pos.files, function(x) x $ distance[1])
  
  distance.covered <- average.height <- vector("list", nsets)

  if (old)
  {
    for (i in seq.nsets)
    {
      for (j in seq.int(minutes[i]))
      {
        temp.mat <- pos.files[[i]][ pos.files [[i]] $ time <=  count.files [[i]] $ Min[j]      * 60  & 
                                    pos.files [[i]] $ time >= (count.files [[i]] $ Min[j] - 1) * 60, ]
        distance.covered[[i]] [j] <- temp.mat $ distance[nrow(temp.mat)] - start.dist[i]
        average.height[[i]] [j] <- mean(temp.mat $ range - height.differential)
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
	
	
    
    view.width <- lapply(average.height, function(x) 2 * x / cos( lower.edge.view  * pi / 180)  * tan( horizontal.angle * pi / 360))
    area <- lapply(seq.nsets, function(i) view.width[[i]] * distance.covered[[i]])

	
	##This ensures that if there are no counts at all for a given minute in a given station (eg no visibility) then, the calculated area for that minute/station is set to NA
	for (i in seq.nsets)
    {
      for (j in seq.int(minutes[i]))
      {
		if (all(is.na(count.files[[i]][j, c("C1", "C2", "C3", "C4", "C5")])) == T)
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
	counter.5.density <- sapply(lapply(seq_along(pos.files), function(i) count.files[[i]] $ C5 / area[[i]]), mean, na.rm = na.rm)
    
	
	##Calculates an average of the counts per minute 
	for (i in seq.nsets)
	{
		count.files [[i]]$average<- numeric(minutes[i])
		
		for (j in seq.int(minutes[i]))
		{
			count.files[[i]][j, "average"]<- round(rowMeans(count.files[[i]][j,c("C1", "C2", "C3", "C4", "C5")], na.rm=T),1)
		}
	}

	##Sums the average count and divides by the total viewed area for each station
	average.density <- sapply(count.files, function(x) sum(x$average, na.rm = na.rm)) / viewed.area
	
	total.counts <-sapply(count.files, function(x) sum(x$average, na.rm = na.rm)) 

    ## make output list
	 if(wg==T){
	   out <-
	     list(
	       survey = rep(as.character(subset(counts,Station==file.list[1])$Cruise[1]),length(file.list)),
	       year = 2000 + as.numeric(substr(file.list,3,4)),
	       station = file.list,
	       long = lons,
	       lat = lats,
	       density = round(average.density, 2),
	       distanceoverground = run.length,
	       area = viewed.area,
	       count = total.counts,
	       ground = substr(file.list,1,2))
	 } else if(station.names==T)
	{
		out <-
		  list(
			lats = lats,
			lons = lons,
			viewed.area = viewed.area,
			distance.covered = run.length,
			average.density  = round(average.density, 2),
			count.1 = round(counter.1.density, 2),
			count.2 = round(counter.2.density, 2),
			count.3 = round(counter.3.density, 2),
			count.4 = round(counter.4.density, 2),
			count.5 = round(counter.5.density, 2),
			Station = file.list)
	} else
		{
			out <-
		  list(
			lats = lats,
			lons = lons,
			viewed.area = viewed.area,
			distance.covered = run.length,
			average.density  = round(average.density, 2),
			count.1 = round(counter.1.density, 2),
			count.2 = round(counter.2.density, 2),
			count.3 = round(counter.3.density, 2),
			count.4 = round(counter.4.density, 2),
			count.5 = round(counter.5.density, 2))
		}
  }
  
  as.data.frame(out)
}

check.strata <-
function (strata.object)
{
  # we don;t want NaN average density
  if (any( is.nan(strata.object $ average.density) ) )
  {
    warning("NaNs in average densisty - samples removed.")
    strata.object <- strata.object[ !is.nan(strata.object $ average.density), ]
  }
  # warn about non-fintite in count
  if (any( !sapply(strata.object[c("count.1","count.2")], is.finite) ) )
  {
    warning("non-finite (NaNs or Inf) in counts.")
  }
   #remove samples not in named strata
  if (any( strata.object $ strata_type == "FALSE" ) )
  {
    warning("FALSE in strata type - samples removed")
    strata.object <- strata.object[ strata.object $ strata_type != "FALSE", ]
  }
  
  strata.object
}

variance.calcs <-
function (x)
{
  x $ xvar <- x $ sample.variance  / x $ no.stations
  x $ standard.error <- sqrt(x $ xvar)
  x $ total.n <- x $ area * x $ sample.mean
  x $ mean.var <- (x $ area^2) * x $ xvar
  x $ st.error <- sqrt(x $ mean.var)
  
  return(x)
}
