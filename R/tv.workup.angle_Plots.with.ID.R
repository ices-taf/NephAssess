#02/08/2013 added 5th counter burrow densities in the output data frame
#29/07/2014 added an argument for removing counters/stations (over/undercounting) from workup)

tv.workup.angle_Plots.with.ID <-
function(wk.dir, database, index = "index.txt", do.plots = TRUE, verbose = FALSE, na.rm = TRUE, old = TRUE, subset = FALSE, counters.removed=NULL, station.names=FALSE)
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
#		names(count.file) <- c("Mins", "C1", "C2", "C3", "C4", "C5")
		names(count.file)<- gsub("Count.", "", names(count.file))
        dat.file<- subset(DAT, Station %in% x)
		count.file<- subset(count.file, Min<=floor(max(dat.file$SecondsElapsed,na.rm=na.rm)/60))	
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
          points(Min * 60 - 9, count.files[[i]][,2] / max(count.files[[i]][,2:dim(count.files[[i]])[2]], na.rm = T), type = "h", lwd = 3, col = 4)
          points(Min * 60 + -3, count.files[[i]][,3] / max(count.files[[i]][,2:dim(count.files[[i]])[2]], na.rm = T), type = "h", lwd = 3, col = 5)
		  points(Min * 60 + 3, count.files[[i]][,4] / max(count.files[[i]][,2:dim(count.files[[i]])[2]], na.rm = T), type = "h", lwd = 3, col = 6)
		  points(Min * 60 + 9, count.files[[i]][,5] / max(count.files[[i]][,2:dim(count.files[[i]])[2]], na.rm = T), type = "h", lwd = 3, col = 7)
		  points(Min * 60 + 15, count.files[[i]][,6] / max(count.files[[i]][,2:dim(count.files[[i]])[2]], na.rm = T), type = "h", lwd = 3, col = "brown")
        }, envir = count.files[[i]])
        leg<- c(c("Distance", "Range", "Depth"), c(names(count.files[[i]])[2:dim(count.files[[i]])[2]]))
		leg<- leg[suppressWarnings(which(is.na(as.numeric(leg))))]
		leg<- leg[!leg %in% "mat"]
		legend(-20, 1, legend=leg, 
               col = c(1:7), lwd = 2, cex = 0.7)
		max.y<- max(count.files[[i]][,2:dim(count.files[[i]])[2]], na.rm = T)
		new.scale<- pretty(seq(0, max.y))
		old.scale<- new.scale/max.y
		if(max.y>0) { axis(2, at=old.scale, lab=new.scale) }
      dev.off()
    }
  }
}
