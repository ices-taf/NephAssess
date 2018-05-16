


area_2009_for_Ade <-
function(wk.dir, index = "index.txt", do.plots = TRUE, verbose = FALSE, na.rm = TRUE, old = TRUE)
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
  height.differential  <- (front.height-rangefinder.height) / 100
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
  
  if (length(file.list) != 2L * no.stations) 
      stop("INCORRECT NUMBER OF FILES IN INDEX")

  file.list <- matrix(file.list, ncol = 2, byrow = TRUE) 
  # a possibility to check file pairing consistency here ...

  nsets <- nrow(file.list)
  seq.nsets <- seq.int(nsets)
  
  ## reads in each count file and corresponding DAT file
  ## some (at least one) files are space delimited, most are comma, some have extra commas
  ## read.csv is more robust than read.table ...
  
  pos.files <- 
    lapply(file.list[,1],
      function (x, verbose)
      {
          pos.file <- read.csv(x, skip=2, header=F)
    
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
    lapply(file.list[,2], 
      function (x) 
      {
        count.file <- read.table(x, skip=2)
        names(count.file) <- c("Mins", "C1", "C2")
        count.file
      })
    
	
  #Replace "xxx" by NAs to make columns C1 and C2 numeric
  for(i in seq.nsets)
	{
		for(j in 2:3)
		{
			suppressWarnings(count.files[[i]][,j] <- as.numeric(as.character(count.files[[i]][,j])))
		}
	}
  
  
  # diagnostic plots
  if (do.plots)
  {
   if (!file.exists("Diagnostic Plots")) dir.create("Diagnostic Plots")
    fnames <- paste("diagnostic plots/", gsub(".dat", ".png", file.list[,1]), sep="")
    main <- gsub(".dat", "", file.list[,1])
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
          points(Mins * 60 - 5, C1 / max(C1, C2, na.rm = T), type = "h", lwd = 4, col = 4)
          points(Mins * 60 + 5, C2 / max(C1, C2, na.rm = T), type = "h", lwd = 4, col = 5)
        }, envir = count.files[[i]])
        legend(0, 1, legend=c("Distance", "Range", "Depth", "Count 1", "Count 2"), 
               col = c(1:5), lwd = 2, cex = 0.6)  
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
		if (all(is.na(count.files[[i]][j, c("C1", "C2")])) == T)
		{
			area[[i]][j]<- NA
		} 
      }
    }
		
    lats <- sapply(pos.files, function(x) x $ lat[1])
    lons <- sapply(pos.files, function(x) x $ lon[1])

    viewed.area <- sapply(area, sum, na.rm = na.rm)
    run.length <- sapply(distance.covered, sum)
    
   
	  
	  
	  
	  
  } 


sum(viewed.area)


}







































area_2010_for_Ade <-
function(wk.dir, index = "index.txt", do.plots = TRUE, verbose = FALSE, na.rm = TRUE, old = TRUE)
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
  height.differential  <- (front.height-rangefinder.height) / 100
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
  
  if (length(file.list) != 2L * no.stations) 
      stop("INCORRECT NUMBER OF FILES IN INDEX")

  file.list <- matrix(file.list, ncol = 2, byrow = TRUE) 
  # a possibility to check file pairing consistency here ...

  nsets <- nrow(file.list)
  seq.nsets <- seq.int(nsets)
  
  ## reads in each count file and corresponding DAT file
  ## some (at least one) files are space delimited, most are comma, some have extra commas
  ## read.csv is more robust than read.table ...
  
  pos.files <- 
    lapply(file.list[,1],
      function (x, verbose)
      {
          pos.file <- read.csv(x, skip=2, header=F)
    
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
    lapply(file.list[,2], 
      function (x) 
      {
        count.file <- read.table(x, skip=2)
        names(count.file) <- c("Mins", "C1", "C2", "C3", "C4")
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
  
  # diagnostic plots
  if (do.plots)
  {
   if (!file.exists("Diagnostic Plots")) dir.create("Diagnostic Plots")
    fnames <- paste("diagnostic plots/", gsub(".dat", ".png", file.list[,1]), sep="")
    main <- gsub(".dat", "", file.list[,1])
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
          points(Mins * 60 - 9, C1 / max(C1, C2, C3, C4, na.rm = T), type = "h", lwd = 3, col = 4)
          points(Mins * 60 + -3, C2 / max(C1, C2, C3, C4, na.rm = T), type = "h", lwd = 3, col = 5)
		  points(Mins * 60 + 3, C3 / max(C1, C2, C3, C4, na.rm = T), type = "h", lwd = 3, col = 6)
		  points(Mins * 60 + 9, C4 / max(C1, C2, C3, C4, na.rm = T), type = "h", lwd = 3, col = 7)
        }, envir = count.files[[i]])
        legend(0, 1, legend=c("Distance", "Range", "Depth", "Count 1", "Count 2", "Count 3", "Count 4"), 
               col = c(1:7), lwd = 2, cex = 0.6)  
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

      
  }

sum(viewed.area)
  
 }

 
 
 