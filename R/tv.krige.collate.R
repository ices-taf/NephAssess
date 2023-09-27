tv_kriging_collate <- function(wk.dir = Wkdir, 
                               strata.object = NULL, 
                               f.u = NULL,
                               survey.data = NULL,
                               krige.object = NULL,
                               survey.year = year){
  
  old.dir <- setwd(wk.dir)
  on.exit(setwd(old.dir))
  
  # if(missing(survey.data) || missing(f.u) || missing(strata.object || missing(krige.object))){
  #   stop("strata.object, survey.data, krige.object, and f.u must all be specified")}
  
  f.u <- check.fu(f.u)
  
  # Get bias correction factor
  bias <- switch(f.u, clyde = 1.19, `south minch` = 1.32, 
                 `moray firth` = 1.21, `firth forth` = 1.18, 
                 `north minch` = 1.33, jura = 1.19, noup = 1.35, 
                 fladen = 1.35)
  
  ## Small patch abundance to be calculated as a scalar to kriged
  smallAreas <- tapply(FU12_smallPatches$AREA, FU12_smallPatches$ROCK_D, sum)/1e6
  names(smallAreas) <- c("MUD", "MUDDY SAND", "SANDY MUD")
  inputs <- list(areas = smallAreas)
  # strata.object <- sm.dens
  total.area <- sum(tapply(FU12_smallPatches$AREA, FU12_smallPatches$ROCK_D, sum)/1e6)
  strata.object <- strata.object[order(strata.object$strata_type),]
  strata.object <- strata.object[strata.object$strata_type != "FALSE",]
  stratum.names <- unique(strata.object$strata_type)
  strata.list <- vector("list", length(stratum.names))
  names(strata.list) <- stratum.names
  for (i in stratum.names){
    tmp <- list()
    tmp$strata_type <- strata.object[strata.object$strata_type == i, ]
    av.dens <- tmp$strata_type$average.density
    tmp$area <- unname(inputs$areas[i])
    tmp$no.stations <- length(av.dens)
    tmp$sample.mean <- mean(av.dens)
    tmp$sample.variance <- var(av.dens)
    tmp <- variance.calcs(tmp)
    strata.list[[i]] <- tmp
  }
  which.fill <- sapply(strata.list, "[[", "no.stations") == 1
  fill.cv <- mean(sapply(strata.list, function(x) x$sample.mean/sqrt(x$sample.variance)), na.rm = TRUE)
  strata.list[which.fill] <- lapply(strata.list[which.fill], 
                                    function(x) {
                                      x$sample.variance <- (x$sample.mean/fill.cv)^2
                                      x
                                    })
  strata.list[which.fill] <- lapply(strata.list[which.fill], 
                                    variance.calcs)
  sum.var.mean <- sum(sapply(strata.list, "[[", "mean.var"))
  strata.list <- lapply(strata.list, function(x) {
    x$prop <- x$mean.var/sum.var.mean
    x
  })
  small.patch.abund <- sum(sapply(strata.list, "[[", "total.n"))
  
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
  results.by.stratum.bias.corrected[ is.na(results.by.stratum.bias.corrected) ] <- ""
  
  strata_list <- lapply(strata.list, function(x) {class(x) <- "tvworkup"; x})
  class(strata.list) <- "tvworkup"
  
  
  ## CVs - Standard relative error calculation
  n.station<- as.numeric(results.by.stratum.bias.corrected[,"Number of stations"])
  var.station<- as.numeric(results.by.stratum.bias.corrected[,"Observed variance"])
  mean.station<- as.numeric(results.by.stratum.bias.corrected[,"Mean burrow density (no./m2)"])
  
  n.station<- n.station[1:(length(n.station)-1)]
  var.station<- var.station[1:(length(var.station)-1)]
  mean.station<- mean.station[1:(length(mean.station)-1)]
  
  var.total<- sum(var.station*n.station)/sum(n.station)
  mean.total<- sum(mean.station*n.station)/sum(n.station)
  
  sd.total<- sqrt(var.total)/sqrt(sum(n.station))
  cv.total<- round(sd.total/mean.total,3)
  
  # Get new values
  krig.mean.density <- krige.object$zest
  krig.coefficient.of.variation <- krige.object$cv
  main.patch.abundance <- krig.mean.density * 4107.165
  krig.CI <- main.patch.abundance*krig.coefficient.of.variation*1.96 + small.patch.abund*cv.total*1.96
  # Get total FU abundance
  krig.total.abundance <- main.patch.abundance + small.patch.abund
  # Add results to dataframe
  final.table <- survey.data
  final.table[final.table$year == survey.year, c("krig.mean",
                                                 "krig.cv",
                                                 "krig.abundance",
                                                 "krig.confidence.interval")] <- c(krig.mean.density,
                                                                                   krig.coefficient.of.variation,
                                                                                   krig.total.abundance,
                                                                                   krig.CI)
  final.table.bias.corrected <- cbind(year = final.table$year, 
                                      mean.density = round(final.table$mean.density/bias, 3), 
                                      abundance = round(final.table$abundance/bias), 
                                      confidence.interval = round(final.table$confidence.interval/bias),
                                      krig.mean = round(final.table$krig.mean/bias, 3),
                                      krig.abundance = round(final.table$krig.abundance/bias),
                                      krig.confidence.interval = round(final.table$krig.confidence.interval/bias)) 
  
  
  # if no fishstats folder create one
  if(!file.exists("fishstats")){dir.create("fishstats")}
  
  write.table(final.table, paste(getwd(), "/", "fishstats/", f.u, "_TV_results.csv", sep = ""), row.names = FALSE, sep = ",") 
  write.table(final.table.bias.corrected, paste(getwd(), "/", "fishstats/", f.u, "_TV_results_bias_corrected.csv", sep = ""), row.names = FALSE, sep = ",") 
  invisible(strata.list)
  
}

