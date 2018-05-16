# TV workup wrapper - generic functional unit


tv.workup.fu <-
function(wk.dir, strata.object, survey.data,  f.u)
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
      areas = c(U = 655.6, V = 425, W = 563, X = 131)),
     
	"jura" = list( 
      areas = c(MUD = 90, "MUDDY SAND" = 142, "SANDY MUD" = 150)),
	  
	  "noup" = list( 
      areas = c("MUDDY SAND" = 409)),
	 
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
	"noup"=1.35,
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

  abundance <- sum( sapply( strata.list, "[[", "total.n") )
  mean.density <- abundance / total.area
  confidence.interval <-  2 * sqrt(sum.var.mean)
  
  
  recent.year <- cbind(rev(survey.data $ year)[1] + 1, abundance, mean.density, confidence.interval)
  colnames(recent.year) <- c("year", "abundance", "mean.density", "confidence.interval")
  final.table <- round(rbind(survey.data, recent.year), 3)
  final.table.bias.corrected<- cbind( year=final.table$year, abundance=round(final.table$abundance/bias), mean.density=round(final.table$mean.density/bias, 3), confidence.interval=round(final.table$confidence.interval/bias)) 
  
  
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

  with( final.table,
  {
    plot(year, abundance, cex = 0.8, type = "o", lwd = 3,
         ylim = c(0, max(abundance + confidence.interval, na.rm = TRUE)))
    arrows(year, abundance - confidence.interval, year, abundance + confidence.interval, angle = 90, code = 3, length = 0.2,lwd=3)
  })
  title(main = f.u, ylab = "Nephrops abundance (millions)", xlab = "Year")

  dev.off()
  
  
  #Corrected for bias
  
  png(paste(getwd(), "/", "fishstats/", f.u, "_TV_results_bias_corrected.png", sep = ""), width=2200, height=1500, pointsize=50)
  par(las = 1, bty = "l", ann = FALSE, pch = 16, mfrow = c(1,1))

  with( final.table,
  {
    plot(year, abundance/bias, cex = 0.8, type = "o", lwd = 3,
         ylim = c(0, max(abundance/bias + confidence.interval/bias, na.rm = TRUE)))
    arrows(year, abundance/bias - confidence.interval/bias, year, abundance/bias + confidence.interval/bias, angle = 90, code = 3, length = 0.2,lwd=3)
  })
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
      round( sapply(strata.list, "[[", "total.n"), 1),
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
  results.by.stratum.bias.corrected[,"Mean burrow density (no./m2)"]<- round(as.numeric(results.by.stratum.bias.corrected[,"Mean burrow density (no./m2)"])/bias, 3)
  results.by.stratum.bias.corrected[,"Observed variance"]<- round(as.numeric(results.by.stratum.bias.corrected[,"Observed variance"])/bias^2, 3)
  results.by.stratum.bias.corrected[,"Stratum variance"]<- round(as.numeric(results.by.stratum.bias.corrected[,"Stratum variance"])/bias^2, 0)
  results.by.stratum.bias.corrected [ is.na(results.by.stratum.bias.corrected) ] <- ""
  
  # Write table to a csv
  write.table(results.by.stratum, paste(getwd(), "/", "fishstats/", f.u, "_TV results by stratum.csv", sep = ""), sep = ",", row.names=FALSE)
  write.table( results.by.stratum.bias.corrected, paste(getwd(), "/", "fishstats/", f.u, "_TV results by stratum_bias_corrected.csv", sep = ""), sep = ",", row.names=FALSE)

  strata_list <- lapply(strata.list, function(x) {class(x) <- "tvworkup"; x})
  class(strata.list) <- "tvworkup"

  invisible( strata.list )

}
