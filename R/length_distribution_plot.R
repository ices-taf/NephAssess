length_distribution_plot <- function(flneph.object, 
                                     years = c(flneph.object@range["maxyear"]-20, flneph.object@range["maxyear"]), 
                                     extra.space = 2, 
                                     MLS = c(20,35),
                                     wk.dir,
                                     fade = TRUE){
    
    # Set up exit commands
    oldPar <- par()
    on.exit(par(oldPar))
    on.exit(dev.off())
    
    # Set arguments for test
    # flneph.object <- nephup.fu13
    # years <- c(flneph.object@range["maxyear"]-20, flneph.object@range["maxyear"])
    # extra.space <- 3
    
    if (missing(extra.space)){
        extra.space <- 2
    }
    
    start.year <- years[1]
    end.year <- years[2]
    year.range <- c(start.year:end.year)
    
    flneph.object <- FLCore::trim(flneph.object, year = years[1]:years[2])
    
    min.length <- as.numeric(rownames(flneph.object@catch.n)[1])
    max.length <- as.numeric(rownames(flneph.object@catch.n)[dim(flneph.object@catch.n)[1]])
    
    # Extract and format plotting data
    disc <- as.data.frame(seasonSums(flneph.object@discards.n))
    catch <- as.data.frame(seasonSums(flneph.object@catch.n))
    land <- as.data.frame(seasonSums(flneph.object@landings.n))
    df <- data.frame(Year = disc$year,
                     Sex = disc$unit,
                     Length = disc$lengths,
                     Landings = land$data,
                     Discards = disc$data,
                     Catch = catch$data)
    bin <- 2
    df$length2 <- df$Length + (ifelse(floor(df$Length/bin)-df$Length/bin==0,0,-1))
    df2 <- tapply.ID(df, "Landings", c("Year", "Sex", "length2"), "sum", "Landings")
    df2$Discards <- tapply.ID(df, "Discards", c("Year", "Sex", "length2"), "sum", "temp")[,"temp"]
    df2$Catch <- tapply.ID(df, "Catch", c("Year", "Sex", "length2"), "sum", "temp")[,"temp"]
    df2$Length <- df2$length2+1
    df <- df2
    tab <- with(df, tapply(Catch, list(Year, Sex), max))
    vec <- vectorise.names(tab, c("max", "Year", "Sex"))
    
    ld2 <- merge(df, vec)
    ld2$scaled.catch <- ld2$Catch/ld2$max
    ld2$scaled.landing <- ld2$Landings/ld2$max
    ld2$time.catch <- ld2$Year+(.9999*ld2$scaled.catch)
    ld2$time.landing <- ld2$Year+(.9999*ld2$scaled.landing)
    
    # stack the catch and landings together, to use cbind the columns need to be the same name, 
    # so make a temp data frame and rename time.landing time.catch
    ld2 <- ld2[ld2$scaled.catch > 0,]
    ld2 <- sort.data.frame(~ Year + Sex + Length, ld2)
    length(ld2$Year)
    temp <- as.data.frame(ld2$Year)
    names(temp)[1] <- "Year"
    temp$Sex <- ld2$Sex
    temp$Length <- ld2$Length
    temp$time.catch <- ld2$time.landing
    temp$type <- rep("land", length(temp$Year))
    
    ld2$type <- rep("catch", length(ld2$Year))
    ld3 <- rbind(ld2[,c("Year", "Sex", "Length","type","time.catch")],temp)
    ld3 <- sort.data.frame(~Sex +type + Year+ Length, ld3)
    length(ld2[,1])
    length(ld3[,1])
    
    # Split dataframes by sex/catch component
    ldfc <- ld3[ld3$Sex == "Female" & ld3$type == "catch",]
    ldfl <- ld3[ld3$Sex == "Female" & ld3$type == "land",]
    ldmc <- ld3[ld3$Sex == "Male" & ld3$type == "catch",]
    ldml <- ld3[ld3$Sex == "Male" & ld3$type == "land",]
    
    # Get mean lengths by year for each dataset
    fastmean <- function(dat){
        with(dat, sum(data*lengths)/sum(data))
    }
    
    lcmeans <- unlist(by(catch, list(catch$year, catch$unit), fastmean, simplify = FALSE))
    llmeans <- unlist(by(land, list(land$year, land$unit), fastmean, simplify = FALSE))
    # Females
    lfm <- data.frame(year = c(years[1]:years[2]),
                      lcmean = lcmeans[1:(length(lcmeans)/2)],
                      llmean = llmeans[1:(length(llmeans)/2)],
                      time.catch = NA,
                      time.land = NA)
    
    for(i in unique(ldfc$Year)){
        xx <- ldfc[ldfc$Year == i,]
        f <- approxfun(x = xx$Length, y = xx$time.catch)
        lfm$time.catch[lfm$year == i] <- f(lfm$lcmean[lfm$year == i])
        
        yy <- ldfl[ldfl$Year == i,]
        f <- approxfun(x = yy$Length, y = yy$time.catch)
        lfm$time.land[lfm$year == i] <- f(lfm$llmean[lfm$year == i])
    }
    
    # lfm$time.catch <- ldfc$time.catch[match(paste(lfm$year, 2*floor(lfm$lcmean/2)+1), paste(ldfc$Year, ldfc$Length))]
    # lfm$time.land <- ldfl$time.catch[match(paste(lfm$year, 2*floor(lfm$llmean/2)+1), paste(ldfl$Year, ldfl$Length))]
    
    # Males
    lmm <- data.frame(year = c(years[1]:years[2]),
                      lcmean = lcmeans[(1+(length(lcmeans)/2)):length(lcmeans)],
                      llmean = llmeans[(1+(length(llmeans)/2)):length(llmeans)],
                      time.catch = NA,
                      time.land = NA)
    
    for(i in unique(ldmc$Year)){
        xx <- ldmc[ldmc$Year == i,]
        f <- approxfun(x = xx$Length, y = xx$time.catch)
        lmm$time.catch[lmm$year == i] <- f(lmm$lcmean[lmm$year == i])
        
        yy <- ldml[ldml$Year == i,]
        f <- approxfun(x = yy$Length, y = yy$time.catch)
        lmm$time.land[lmm$year == i] <- f(lmm$llmean[lmm$year == i])
    }
    
    # lmm$time.catch <- ldmc$time.catch[match(paste(lmm$year, 2*floor(lmm$lcmean/2)+1), paste(ldmc$Year, ldmc$Length))]
    # lmm$time.land <- ldml$time.catch[match(paste(lmm$year, 2*floor(lmm$llmean/2)+1), paste(ldml$Year, ldml$Length))]
    
    # Create colour ramp for the lines
    if(fade == TRUE){
        lcols <- colorRampPalette(c("#b3b3b3","#000000"))(length(years[1]:years[2]))
    } else {lcols <- rep("#000000", times = length(years[1]:years[2]))}
    
    # Function to avoid overwriting other plots
    get.fname <- function(base.name){
        fname <- function(i){paste(base.name, i, ".jpeg", sep="")}
        out <- fname(i <- 1)
        while(file.exists(out)){out <- fname(i <- i + 1)}
        return (out)
    }
    
    # Open .jpeg file connection
    jpeg(get.fname(paste(wk.dir, "length_distribution_plot", sep="")), 
         width = 210, height = 262, units = "mm", res = 350)
    
    # Get old plotting parameters and set parameters
    par(mfrow=c(1,2), mar = c(5.1, 0, 2.5, 0), oma = c(0, 4.1, 0, 1.1))
    
    
    # Females
    plot(1, 1, 
         xlim = c(10,70),
         type = "n", 
         ylim = c(start.year, end.year+extra.space), 
         ylab = "", 
         xlab = "", 
         main = "" )
    abline(v=MLS[1], col="#b3b3b3", lwd=1.5, lty=3)
    abline(v=MLS[2], col="#b3b3b3", lwd=1.5, lty=3)
    box()
    legend("topright", legend = "Female", bty = "n", text.font = 2, cex = 1.5)
    
    for(i in unique(ldfc$Year)){
        lines(ldfc$Length[ldfc$Year==i], ldfc$time.catch[ldfc$Year==i], 
              lty = 3, lwd = 1.5,
              col = lcols[match(i,unique(ldfc$Year))])
        lines(ldfl$Length[ldfc$Year==i], ldfl$time.catch[ldfc$Year==i], 
              lty = 1, lwd = 1.5,
              col = lcols[match(i,unique(ldfl$Year))])
    }
    
    lines(lfm$lcmean, lfm$time.catch, col = "#D55E00", lwd = 2, lty = 2)
    lines(lfm$llmean, lfm$time.land, col = "#D55E00", lwd = 2, lty = 1)
    
    # Males
    plot(1, 1, 
         xlim = c(10,70),
         type = "n", 
         ylim = c(start.year, end.year+extra.space), 
         ylab = "", 
         xlab = "", 
         main = "",
         axes = FALSE)
    abline(v=MLS[1], col="#b3b3b3", lwd=1.5, lty=3)
    abline(v=MLS[2], col="#b3b3b3", lwd=1.5, lty=3)
    axis(3)
    box()
    legend("topright", legend = "Male", bty = "n", text.font = 2, cex = 1.5)
    
    for(i in unique(ldmc$Year)){
        lines(ldmc$Length[ldmc$Year==i], ldmc$time.catch[ldmc$Year==i], 
              lty = 3, lwd = 1.5,
              col = lcols[match(i,unique(ldmc$Year))])
        lines(ldml$Length[ldml$Year==i], ldml$time.catch[ldml$Year==i], 
              lty = 1, lwd = 1.5, 
              col = lcols[match(i,unique(ldml$Year))])
    }
    
    lines(lmm$lcmean, lmm$time.catch, col = "#D55E00", lwd = 2, lty = 2)
    lines(lmm$llmean, lmm$time.land, col = "#D55E00", lwd = 2, lty = 1)
    
    mtext("Carapace Length (mm)", side=1, line=-2, outer=TRUE)    
    mtext("Year", side=2, line=2.5, outer=TRUE)  
    
    # oldw <- getOption("warn")
    # options(warn = -1)
    # suppressWarnings(par(oldPar))
    # options(warn = oldw)
}

