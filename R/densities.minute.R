
densities.minute <-
function(wk.dir, database, index = "index.txt", verbose = FALSE, na.rm = TRUE)
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
  back.height          <- tmp[2]
  rangefinder.height   <- tmp[3]
  camera.length        <- tmp[4]
  horizontal.angle     <- tmp[5]
  vertical.angle       <- tmp[6]
  camera.angle         <- acos( (back.height - front.height) / camera.length ) * 180 / pi
  height.differential  <- (rangefinder.height-front.height) / 100
  lower.edge.view      <- camera.angle - 0.5 * vertical.angle
  no.stations          <- as.integer(file.list[4])

  # strip off header info
  
  file.list <- file.list[-(1:4)]
  file.list <- file.list[file.list != ""]

  if (is.na(cruise))
      stop("There are problems with your index file - please check it is of standard format and try again") 
  
  if (any(is.na(c(front.height, back.height, rangefinder.height, camera.length,
                  horizontal.angle, vertical.angle, camera.angle,
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
  db<- odbcConnectAccess(database)
  db.tables<- sqlTables(db)
  db.tables<- subset(db.tables, TABLE_NAME %in% c("Dot dat files", "Raw_burrow_counts"))
  DAT<- sqlQuery(db, paste("SELECT * FROM [",db.tables$TABLE_NAME[db.tables$TABLE_NAME=="Dot dat files"],"]",sep="") )
  counts<- sqlQuery(db, paste("SELECT * FROM [",db.tables$TABLE_NAME[db.tables$TABLE_NAME=="Raw_burrow_counts"],"]",sep="") )
  odbcClose(db)
  
  
    
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
              stop (gettextf("column of NAs in middle of data in: %s", file.list[(i*2)-1]))
          pos.file <- pos.file[which.ok]
      
          if (dim(pos.file)[2] > 7) stop (gettextf("more than 7 columns of data in: %s", file.list[i,1]))
      
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
		  
          if (any( tmp <- is.na(pos.file $ range ))) pos.file $ range [ tmp ] <- 0.88
		  
		  if (any(tmp <- pos.file $ range < 0)) pos.file $ range [ tmp ] <- 0.88
          
          if (any(tmp <- pos.file $ range < 3))
          {
            pos.file $ range [ !tmp ] <- mean(pos.file $ range [ tmp ])
          } else
          {
            pos.file $ range <- 0.88
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
        dat.file<- subset(DAT, Station %in% x)
		count.file<- subset(count.file, Mins<=floor(max(dat.file$SecondsElapsed)/60))	
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
  
  distance.covered <- average.height <- vector("list", nsets)


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

	average.density.minute<- lapply(seq.nsets, function(i) 
	{
	temp<- count.files[[i]]
	temp<- temp[,c("Mins", "average")]
	temp$area<- area[[i]]
	temp$dens<- temp$average/temp$area
	temp$station<- rep(file.list[i], dim(temp)[1])
	temp$fu<- substr(temp$station, 1, 2)
	names(temp)<- c("Mins", "Average_counts", "Area", "Density", "Sation", "FU")
	temp
	}
	)

	return(do.call(rbind,average.density.minute))

}