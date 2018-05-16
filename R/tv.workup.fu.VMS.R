# TV workup wrapper - generic functional unit


tv.workup.fu.VMS <-
function(wk.dir, strata.object, survey.data,  f.u, vms.area=NULL)
{

  old.dir <- setwd(wk.dir)
  on.exit( setwd(old.dir) )

  if (missing(survey.data) || missing(f.u) || missing(strata.object)) stop("strata.object, survey.data and f.u must be specified")

  f.u <- check.fu (f.u)
  
  inputs <- 
  switch(f.u,
    "clyde" = list( 
      areas = c(MUD = 716.8, "MUDDY SAND" = 664.6, "SANDY MUD" = 698.6)),

    "south minch" = list(
      areas = c(MUD = 303.1, "MUDDY SAND" = 2027.6, "SANDY MUD" = 2740.6)),

    "moray firth" = list(
      areas = c("MUDDY SAND E" = 646, "MUDDY SAND C" = 698.44, "MUDDY SAND W" = 681.68, "SANDY MUD" = 169)),

    "firth forth" = list( 
      areas = c("MUDDY SAND E" = 395.019, "MUDDY SAND C" = 210.596, "MUDDY SAND W" = 138.90, "SANDY MUD" = 170.48)),
      
    "north minch" = list( 
      areas = c("VMS" = vms.area)),
     
	"jura" = list( 
      areas = c(MUD = 90, "MUDDY SAND" = 142, "SANDY MUD" = 150)),
	 
    "fladen" = list( 
      areas = c(C = 15634 , MC = 4303.5 , MF = 4966.6, "F" = 3248.1))
    )
    
  if (f.u == "fladen")
  { 
    names(strata.object) [dim(strata.object)[2]-1] <- "strata_type_old"
    names(strata.object) [dim(strata.object)[2]] <- "strata_type"
  }
  
  bias <- 
  switch(f.u,
    "clyde" = 1.19,
	"south minch" = 1.32,
	"moray firth" = 1.21,
	"firth forth" = 1.18,
	"north minch" = 1.33,
	"jura" = 1.19,
	"fladen" = 1.35)
  
  strata.object <- check.strata (strata.object)

  total.area <- sum(inputs $ areas)
  
  strata.object <- strata.object[order(strata.object$strata_type),]

  stratum.names <- unique( strata.object $ strata_type )
  if (!all(stratum.names %in% names( inputs $ areas))) stop("stratum names not consistent in inputs")

  strata.list <- vector("list", length(stratum.names))
  names(strata.list) <- stratum.names

  for (i in stratum.names)
  {
    tmp <- list()
    tmp $ strata_type <- strata.object[strata.object $ strata_type == i, ]
    av.dens <- tmp $ strata_type $ average.density
    tmp $ area <- unname(inputs $ areas [i])
    tmp $ no.stations <- length(av.dens)
    tmp $ sample.mean <- mean(av.dens)
    tmp $ sample.variance <- var(av.dens)
    tmp <- variance.calcs (tmp)
    #tmp $ biomass <- tmp $ total.n * inputs $ mean.wt

    strata.list[[i]] <- tmp
  }

  ## if any strata have only 1 station fill in the variance by assuming constant CV
  which.fill <- sapply(strata.list, "[[", "no.stations") == 1
  fill.cv <- mean(sapply(strata.list, function(x) x $ sample.mean / sqrt(x $ sample.variance)), na.rm = TRUE)
  strata.list[which.fill] <- lapply( strata.list[which.fill], function (x) { x $ sample.variance <- (x $ sample.mean / fill.cv)^2; x} )
  strata.list[which.fill] <- lapply( strata.list[which.fill], variance.calcs)
  
  
  ## Proportions of the mean

  sum.var.mean <- sum( sapply(strata.list, "[[", "mean.var") )
  strata.list <- lapply(strata.list, function(x) {x $ prop <- x $ mean.var / sum.var.mean; x})
 

  #################################################
  #####  Final table   - TV Survey results  #######
  #################################################

  abundance.VMS <- sum( sapply( strata.list, "[[", "total.n") )
  mean.density <- abundance.VMS / total.area
  confidence.interval.VMS <-  2 * sqrt(sum.var.mean)
  
  
  recent.year <- cbind(rev(survey.data $ year)[1] + 1, mean.density, NA, NA, abundance.VMS, NA, NA, confidence.interval.VMS)
  colnames(recent.year) <- c("year", "mean.density", "abundance.sediment", "abundance.VMS.1", "abundance.VMS.2", "confidence.interval.sediment", "confidence.interval.VMS.1", "confidence.interval.VMS.2")
  final.table <- round(rbind(survey.data, recent.year), 2)
  final.table.bias.corrected<- cbind( final.table[,c("year", "mean.density")], round(final.table[,c("abundance.sediment", "abundance.VMS.1", "abundance.VMS.2", "confidence.interval.sediment", "confidence.interval.VMS.1", "confidence.interval.VMS.2")] / bias)) 

  
  # if no fishstats folder create one
  if (! file.exists("fishstats")) dir.create("fishstats")

  write.table(final.table, paste(getwd(), "/", "fishstats/", f.u, "_TV_results.csv", sep = ""), row.names = FALSE, sep = ",") 
  write.table(final.table.bias.corrected, paste(getwd(), "/", "fishstats/", f.u, "_TV_results_bias_corrected.csv", sep = ""), row.names = FALSE, sep = ",") 

  
  ####################
  ######  PLOT #######
  ####################

  #Not corrected for bias
  
  png(paste(getwd(), "/", "fishstats/", f.u, "_TV_results.png", sep = ""), width=2200, height=1500, pointsize=50)
  par(las = 1, bty = "l", ann = FALSE, pch = 16, mfrow = c(1,1))

 
    plot(final.table$year, final.table$abundance.VMS.2, cex = 0.8, type = "o", lwd = 3, lty = 1, pch=16,
         ylim = c(0, max(final.table$abundance.VMS.2 + final.table$confidence.interval.VMS.2, na.rm = TRUE)))
    
	yr.int<- subset(final.table, year >= 2010)
	arrows(yr.int$year, yr.int$abundance.VMS.2 - yr.int$confidence.interval.VMS.2, yr.int$year, yr.int$abundance.VMS.2 + yr.int$confidence.interval.VMS.2, angle = 90, code = 3, length = 0.2,lwd=3,lty=1)
 
    lines(final.table$year, final.table$abundance.sediment, cex = 0.8, type = "o", lwd = 3, pch=1, lty=2,
         ylim = c(0, max(final.table$abundance.sediment + final.table$confidence.interval.sediment, na.rm = TRUE)))
    arrows(final.table$year, final.table$abundance.sediment - final.table$confidence.interval.sediment, final.table$year, final.table$abundance.sediment + final.table$confidence.interval.sediment, angle = 90, code = 3, length = 0.2,lwd=3, lty=1)

  legend("topleft", legend=c("Sediment abundance series", "VMS abundance series"), pch=c(1,16), lty=c(2,1), bty="n", cex=0.6)
  title(main = f.u, ylab = "Nephrops abundance (millions)", xlab = "Year")

  dev.off()
  
  
  # corrected for bias
  
  png(paste(getwd(), "/", "fishstats/", f.u, "_TV_results_bias_corrected.png", sep = ""), width=2200, height=1500, pointsize=50)
  par(las = 1, bty = "l", ann = FALSE, pch = 16, mfrow = c(1,1))

 
    plot(final.table.bias.corrected$year, final.table.bias.corrected$abundance.VMS.2, cex = 0.8, type = "o", lwd = 3, lty = 1, pch=16,
         ylim = c(0, max(final.table.bias.corrected$abundance.VMS.2 + final.table.bias.corrected$confidence.interval.VMS.2, na.rm = TRUE)))
    
	yr.int<- subset(final.table.bias.corrected, year >= 2010)
	arrows(yr.int$year, yr.int$abundance.VMS.2 - yr.int$confidence.interval.VMS.2, yr.int$year, yr.int$abundance.VMS.2 + yr.int$confidence.interval.VMS.2, angle = 90, code = 3, length = 0.2,lwd=3,lty=1)
 
    lines(final.table.bias.corrected$year, final.table.bias.corrected$abundance.sediment, cex = 0.8, type = "o", lwd = 3, pch=1, lty=2,
         ylim = c(0, max(final.table.bias.corrected$abundance.sediment + final.table.bias.corrected$confidence.interval.sediment, na.rm = TRUE)))
    arrows(final.table.bias.corrected$year, final.table.bias.corrected$abundance.sediment - final.table.bias.corrected$confidence.interval.sediment, final.table.bias.corrected$year, final.table.bias.corrected$abundance.sediment + final.table.bias.corrected$confidence.interval.sediment, angle = 90, code = 3, length = 0.2,lwd=3, lty=1)

  legend("topleft", legend=c("Sediment abundance series", "VMS abundance series"), pch=c(1,16), lty=c(2,1), bty="n", cex=0.6)
  title(main = f.u, ylab = "Nephrops abundance (millions)", xlab = "Year")

  dev.off()

  #########################################################
  ##### Final table - Results by stratum last 3 years #####
  #########################################################

  results.by.stratum <-
    data.frame(
      stratum.names,
      sapply(strata.list, "[[", "area" ),
      sapply(strata.list, "[[", "no.stations"),
      round( sapply(strata.list, "[[", "sample.mean"), 3),
      round( sapply(strata.list, "[[", "sample.variance"), 3),
      round( sapply(strata.list, "[[", "total.n"), 0),
      round( sapply(strata.list, "[[", "mean.var"), 0),
      round( sapply(strata.list, "[[", "prop"), 3),
      stringsAsFactors = FALSE
    )

  # calculate totals for each column and bind to previous table
  sums <- c( "Total", lapply( results.by.stratum[-1], sum ) )
  sums[4:5] <- NA
  names(sums) <- names(results.by.stratum)
  results.by.stratum <- rbind(results.by.stratum, sums)

  names(results.by.stratum) <- c("Stratum", "Area (km2)", "Number of stations", "Mean burrow density (no./m2)", "Observed variance", "Abundance (millions)",
                                          "Stratum variance", "Proportion of total variance")
  # convert to character...
  results.by.stratum[] <- lapply( results.by.stratum, as.character )
  results.by.stratum [ is.na(results.by.stratum) ] <- ""
  
  #Correct for bias
  results.by.stratum.bias.corrected<- results.by.stratum
  results.by.stratum.bias.corrected[,"Abundance (millions)"]<- round(as.numeric(results.by.stratum.bias.corrected[,"Abundance (millions)"])/bias, 1)
 

  # Write table to a csv
  write.table(results.by.stratum, paste(getwd(), "/", "fishstats/", f.u, "_TV results by stratum.csv", sep = ""), sep = ",", row.names=FALSE)
  write.table(results.by.stratum.bias.corrected, paste(getwd(), "/", "fishstats/", f.u, "_TV results by stratum_bias_corrected.csv", sep = ""), sep = ",", row.names=FALSE)

  
  strata_list <- lapply(strata.list, function(x) {class(x) <- "tvworkup"; x})
  class(strata.list) <- "tvworkup"

  invisible( strata.list )

}
