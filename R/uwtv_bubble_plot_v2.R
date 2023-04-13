uwtv_bubble_plot <- function(
    dat = NULL,
    wk.dir,
    year = NA,
    orientation = c("Landscape", "Portrait"),
    style = c("new", "old"),
    stratum_legend = FALSE,
    elevation = FALSE,
    jura = NULL){
  
  
  
  # Coefficient of variation function
  CV <- function(x){
    (sd(x)/mean(x))
  }
  
  # Function to create new plot names
  get.fname <- function(base.name){
    fname <- function(i){paste(base.name, i, ".jpeg", sep = "")}
    out <- fname(i <- 1)
    while(file.exists(out))
      out <- fname(i <- i + 1)
    return(out)
  }
  
  
  # Function to capitalize first letter of each word
  simpleCap <- function(x){
    x <- tolower(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }
  
  ## evaluate orientation
  # orientation <- orientation(members)
  orientation <- match.arg(orientation)
  style <- match.arg(style)
  
  
  ## Plotting colour palettes
  
  # 3 sediment types
  sedimentCol <- c("#009E73", "#E69F00", "#56B4E9")
  bubbleFill <- c(rgb(0, 158, 115, 50, maxColorValue = 255),
                  rgb(230, 159, 0, 50, maxColorValue = 255),
                  rgb(86, 180, 233, 50, maxColorValue = 255))
  
  # 2 sediment types
  sedimentCol2 <- sedimentCol[c(2,3)]
  bubbleFill2 <- bubbleFill[c(2,3)]
  
  # Fladen
  fladenCol <- c("#009E73", "#E69F00", "#56B4E9", "#CC79A7")
  fladenFill <- c(rgb(0, 158, 115, 50, maxColorValue = 255),
                  rgb(230, 159, 0, 50, maxColorValue = 255),
                  rgb(86, 180, 233, 50, maxColorValue = 255),
                  rgb(204, 121, 167, 50, maxColorValue = 255))
  
  
  
  ### FU7
  if(unique(dat$FU) == "Fladen"){
    
    # Get years
    mfys <- unique(dat$year)
    dat <- dat[dat$year %in% c(mfys[(match(year, mfys)-11):match(year, mfys)]),]
    
    # Calculate bubble radii
    dat$radii <- sqrt(dat$average_density/pi)
    
    # SymbolScaling
    symbolScaling <- 0.15
    
    # Strata as factor
    dat$strata_type <- factor(dat$strata_type, levels = c("C", "MC", "MF", "F"))
    
    ## Landscape Orientation  
    if(orientation == "Landscape"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU7_bubble_plot_ls.")), 
           width = 380, height = 230, units = "mm", res = 350)
      par(mfrow = c(3, 4), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-2.1, 2),
             ylim = c(57.4, 60.6),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r7, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r7, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU7 strata & sediments
          segments(-1, 60.5, 2, 60.5, lty = 2, col = "#dadada")
          segments(-1, 60.5, -1, 59, lty = 2, col = "#dadada")
          segments(-2, 59, -1, 59, lty = 2, col = "#dadada")
          segments(-2, 59, -2, 58, lty = 2, col = "#dadada")
          segments(-1, 58, -2, 58, lty = 2, col = "#dadada")
          segments(-1, 58, -1, 57.5, lty = 2, col = "#dadada")
          segments(2, 57.5, -1, 57.5, lty = 2, col = "#dadada")
          segments(2, 57.5, 2, 60.5, lty = 2, col = "#dadada")
          lapply(fladenSedimentsList, polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = fladenCol[as.factor(nz$strata_type)], 
                  bg = fladenFill[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10,10,10,-10), y = c(50,50,70,70), col = "#c9e9f6")
          
          # FU7 strata & sediments
          segments(-1, 60.5, 2, 60.5, lty = 2, col = "#000000")
          segments(-1, 60.5, -1, 59, lty = 2, col = "#000000")
          segments(-2, 59, -1, 59, lty = 2, col = "#000000")
          segments(-2, 59, -2, 58, lty = 2, col = "#000000")
          segments(-1, 58, -2, 58, lty = 2, col = "#000000")
          segments(-1, 58, -1, 57.5, lty = 2, col = "#000000")
          segments(2, 57.5, -1, 57.5, lty = 2, col = "#000000")
          segments(2, 57.5, 2, 60.5, lty = 2, col = "#000000")
          
          lapply(fladen.surfer.poly[c(17:20,24,25)], polygon, col = "olivedrab", border = NA)
          lapply(fladen.surfer.poly[c(1,2,3,4,23)], polygon, col = "green", border = NA)
          lapply(fladen.surfer.poly[c(5,7,9,14)], polygon, col = "lightgreen", border = NA)
          lapply(fladen.surfer.poly[10], polygon, col = "darkgreen", border = NA)
          lapply(fladen.surfer.poly[12], polygon, col = "lightgreen", border = NA)
          lapply(fladen.surfer.poly[c(13,22)], polygon, col = "green", border = NA)
          lapply(fladen.surfer.poly[c(6,8,11,15,16,21)], polygon, col = "#c9e9f6", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r7, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(1, 60.2, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-3):match(year, mfys)])){
          axis(1,
               cex.axis = 1.5,
               at = c(-1.5, 0, 1.5), 
               labels = c(expression("1.5"^o*W),
                          expression("0"^o),
                          expression("1.5"^o*E)))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-7)], 
                    mfys[(match(year, mfys)-3)])){
          axis(2, at = seq(58, 60, by = 1), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(58, 60, by = 1)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-3)]){
          legdens <- c(1.2, 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("1.2", cex = 0.5)
          stW <- strwidth(c("1.2","0.5","0.1"), cex=0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-1.25, 3), rep(59.4, 3) + 2*heightAdj, 
                   x1 = rep(-1.5, 3), y1 = rep(59.4, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-1.25, 3), rep(59.4, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-1.6, 3) - 0.5*stWdiff, rep(59.4, 3) + 2*heightAdj, 
               c("1.2", "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-1.25, 59.15, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-7.8, 57,
               legend = levels(as.factor(dat$strata_type)),
               col = fladenCol,
               pt.bg = fladenFill,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
      ## Portrait Orientation
    } else if(orientation == "Portrait"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU7_bubble_plot_po.")), 
           width = 300, height = 320, units = "mm", res = 350)
      par(mfrow = c(4, 3), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-2.1, 2),
             ylim = c(57.4, 60.6),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r7, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r7, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU7 strata & sediments
          segments(-1, 60.5, 2, 60.5, lty = 2, col = "#dadada")
          segments(-1, 60.5, -1, 59, lty = 2, col = "#dadada")
          segments(-2, 59, -1, 59, lty = 2, col = "#dadada")
          segments(-2, 59, -2, 58, lty = 2, col = "#dadada")
          segments(-1, 58, -2, 58, lty = 2, col = "#dadada")
          segments(-1, 58, -1, 57.5, lty = 2, col = "#dadada")
          segments(2, 57.5, -1, 57.5, lty = 2, col = "#dadada")
          segments(2, 57.5, 2, 60.5, lty = 2, col = "#dadada")
          lapply(fladenSedimentsList, polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = fladenCol[as.factor(nz$strata_type)], 
                  bg = fladenFill[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10,10,10,-10), y = c(50,50,70,70), col = "#c9e9f6")
          
          # FU7 strata & sediments
          segments(-1, 60.5, 2, 60.5, lty = 2, col = "#000000")
          segments(-1, 60.5, -1, 59, lty = 2, col = "#000000")
          segments(-2, 59, -1, 59, lty = 2, col = "#000000")
          segments(-2, 59, -2, 58, lty = 2, col = "#000000")
          segments(-1, 58, -2, 58, lty = 2, col = "#000000")
          segments(-1, 58, -1, 57.5, lty = 2, col = "#000000")
          segments(2, 57.5, -1, 57.5, lty = 2, col = "#000000")
          segments(2, 57.5, 2, 60.5, lty = 2, col = "#000000")
          
          lapply(fladen.surfer.poly[c(17:20,24,25)], polygon, col = "olivedrab", border = NA)
          lapply(fladen.surfer.poly[c(1,2,3,4,23)], polygon, col = "green", border = NA)
          lapply(fladen.surfer.poly[c(5,7,9,14)], polygon, col = "lightgreen", border = NA)
          lapply(fladen.surfer.poly[10], polygon, col = "darkgreen", border = NA)
          lapply(fladen.surfer.poly[12], polygon, col = "lightgreen", border = NA)
          lapply(fladen.surfer.poly[c(13,22)], polygon, col = "green", border = NA)
          lapply(fladen.surfer.poly[c(6,8,11,15,16,21)], polygon, col = "#c9e9f6", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r7, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(1, 60.2, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-2):match(year, mfys)])){
          axis(1,
               cex.axis = 1.5,
               at = c(-1.5, 0, 1.5), 
               labels = c(expression("1.5"^o*W),
                          expression("0"^o),
                          expression("1.5"^o*E)))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-8)], 
                    mfys[(match(year, mfys)-5)], 
                    mfys[(match(year, mfys)-2)])){
          axis(2, at = seq(58, 60, by = 1), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(58, 60, by = 1)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-2)]){
          legdens <- c(1.2, 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("1.2", cex = 0.5)
          stW <- strwidth(c("1.2","0.5","0.1"), cex=0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-1.25, 3), rep(59.4, 3) + 2*heightAdj, 
                   x1 = rep(-1.5, 3), y1 = rep(59.4, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-1.25, 3), rep(59.4, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-1.6, 3) - 0.5*stWdiff, rep(59.4, 3) + 2*heightAdj, 
               c("1.2", "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-1.25, 59.15, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-5.7, 57,
               legend = levels(as.factor(dat$strata_type)),
               col = fladenCol,
               pt.bg = fladenFill,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
    }
  }
  
  
  
  ### FU8
  if(unique(dat$FU) == "Firth of Forth"){
    
    # Get years
    mfys <- unique(dat$year)
    dat <- dat[dat$year %in% c(mfys[(match(year, mfys)-11):match(year, mfys)]),]
    
    # Calculate bubble radii
    dat$radii <- sqrt(dat$average_density/pi)
    
    # SymbolScaling
    symbolScaling <- 0.15
    
    # Remove FALSE strata
    dat <- dat[dat$strata_type != "FALSE",]
    
    
    ## Landscape Orientation  
    if(orientation == "Landscape"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU8_bubble_plot_ls.")), 
           width = 380, height = 200, units = "mm", res = 350)
      par(mfrow = c(3, 4), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-3.5, -1.95),
             ylim = c(55.85, 56.35),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        if(style == "new"){
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r8, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r8, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU8 strata
          segments(-4, 56.5, -2, 56.5, lty = 2, col = "#dadada")
          segments(-2, 56.5, -2, 55.5, lty = 2, col = "#dadada")
          
          # FU8 strata
          lapply(forth.poly[5:16], polygon, col = NA, border = "#dadada")
          lapply(forth.poly[23:53], polygon, col = NA, border = "#dadada")
          lapply(forth.poly[54:66], polygon, col = NA, border = "#dadada")
          lapply(forth.poly[17:22], polygon, col = NA, border = "#dadada")
          lapply(forth.poly[1:4], polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol2[as.factor(nz$strata_type)], 
                  bg = bubbleFill2[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-5, 0, 0,-5), y = c(55, 55, 60, 60), col = "#c9e9f6")
          
          lines(x = c(-2, -2), y = c(56.5, 55.5), lty = 2)
          lines(x = c(-3, -2), y = c(56.5, 56.5), lty = 2)
          
          lapply(forth.poly[5:16], polygon, col = "olivedrab", border = NA)
          lapply(forth.poly[23:53], polygon, col = "#c9e9f6", border = NA)
          lapply(forth.poly[54:66], polygon, col = "green", border = NA)
          lapply(forth.poly[17:22], polygon, col = "#c9e9f6", border = NA)
          lapply(forth.poly[1:4], polygon, col = "darkgreen", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r8, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(-3.45, 55.9, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-3):match(year, mfys)])){
          axis(1, at = seq(-3.2, -2.2, by = 0.5), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-3.2, -2.2, by = 0.5)), "W")))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-7)], 
                    mfys[(match(year, mfys)-3)])){
          axis(2, at = seq(56, 56.2, by = 0.2), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(56, 56.2, by = 0.2)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-3)]){
          legdens <- c(3.3, 1, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("3.3", cex = 0.5)
          stW <- strwidth(c("3.3","1","0.1"), cex=0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-3.2, 3), rep(56.25, 3) + 2*heightAdj, 
                   x1 = rep(-3.3, 3), y1 = rep(56.25, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-3.2, 3), rep(56.25, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-3.33, 3) - 0.5*stWdiff, rep(56.25, 3) + 2*heightAdj, 
               c("3.3", "1", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-3.2, 56.2, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-6, 55.8,
               legend = sapply(levels(as.factor(dat$strata_type)), simpleCap),
               col = sedimentCol2,
               pt.bg = bubbleFill2,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
      
      ## Portrait Orientation
    } else if(orientation == "Portrait"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU8_bubble_plot_po.")), 
           width = 360, height = 260, units = "mm", res = 350)
      par(mfrow = c(4, 3), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-3.5, -1.95),
             ylim = c(55.85, 56.35),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        if(style == "new"){
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r8, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r8, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU8 strata
          segments(-4, 56.5, -2, 56.5, lty = 2, col = "#dadada")
          segments(-2, 56.5, -2, 55.5, lty = 2, col = "#dadada")
          
          # FU8 strata
          lapply(forth.poly[5:16], polygon, col = NA, border = "#dadada")
          lapply(forth.poly[23:53], polygon, col = NA, border = "#dadada")
          lapply(forth.poly[54:66], polygon, col = NA, border = "#dadada")
          lapply(forth.poly[17:22], polygon, col = NA, border = "#dadada")
          lapply(forth.poly[1:4], polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol2[as.factor(nz$strata_type)], 
                  bg = bubbleFill2[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-5, 0, 0,-5), y = c(55, 55, 60, 60), col = "#c9e9f6")
          
          lines(x = c(-2, -2), y = c(56.5, 55.5), lty = 2)
          lines(x = c(-3, -2), y = c(56.5, 56.5), lty = 2)
          
          lapply(forth.poly[5:16], polygon, col = "olivedrab", border = NA)
          lapply(forth.poly[23:53], polygon, col = "#c9e9f6", border = NA)
          lapply(forth.poly[54:66], polygon, col = "green", border = NA)
          lapply(forth.poly[17:22], polygon, col = "#c9e9f6", border = NA)
          lapply(forth.poly[1:4], polygon, col = "darkgreen", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r8, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(-3.45, 55.9, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-2):match(year, mfys)])){
          axis(1, at = seq(-3.2, -2.3, by = 0.3), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-3.2, -2.3, by = 0.3)), "W")))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-8)], 
                    mfys[(match(year, mfys)-5)], 
                    mfys[(match(year, mfys)-2)])){
          axis(2, at = seq(56, 56.2, by = 0.2), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(56, 56.2, by = 0.2)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-2)]){
          legdens <- c(3.3, 1, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("3.3", cex = 0.5)
          stW <- strwidth(c("3.3","1","0.1"), cex=0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-3.2, 3), rep(56.25, 3) + 2*heightAdj, 
                   x1 = rep(-3.3, 3), y1 = rep(56.25, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-3.2, 3), rep(56.25, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-3.33, 3) - 0.5*stWdiff, rep(56.25, 3) + 2*heightAdj, 
               c("3.3", "1", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-3.2, 56.2, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-5, 55.8,
               legend = sapply(levels(as.factor(dat$strata_type)), simpleCap),
               col = sedimentCol2,
               pt.bg = bubbleFill2,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
    }
  }
  
  
  
  ### FU9
  if(unique(dat$FU) == "Moray Firth"){
    
    # Get years
    mfys <- unique(dat$year)
    dat <- dat[dat$year %in% c(mfys[(match(year, mfys)-11):match(year, mfys)]),]
    dat <- dat[!is.na(dat$average_density),]
    
    # Calculate bubble radii
    dat$radii <- sqrt(dat$average_density/pi)
    
    # SymbolScaling
    symbolScaling <- 0.15
    
    # Remove FALSE strata
    dat <- dat[dat$strata_type != "FALSE",]
    
    # Set as factor
    dat$strata_type <- as.factor(dat$strata_type)
    
    ## Landscape Orientation  
    if(orientation == "Landscape"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU9_bubble_plot_ls.")), 
           width = 380, height = 200, units = "mm", res = 350)
      par(mfrow = c(3, 4), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-4.1, -1.6),
             ylim = c(57.6, 58.2),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r9, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r9, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU9 strata
          segments(-4, 56.5, -2, 56.5, lty = 2, col = "#dadada")
          # FU9 strata
          lapply(moray.poly[2:8], polygon, col = NA, border = "#dadada")
          segments(-2, 57.96422, -2, 57.80697, lty = 1, col = "#ffffff")
          lapply(moray.poly[9:20], polygon, col = NA, border = "#dadada")
          lapply(moray.poly[21:25], polygon, col = NA, border = "#dadada")
          lapply(moray.poly[1], polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol[as.factor(nz$strata_type)], 
                  bg = bubbleFill[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-4, -2), y = c(58.5, 58.5), lwd = 1, lty = 2)
          lines(x = c(-2, -2), y = c(58.5, 58), lwd = 1, lty = 2)
          lines(x = c(-2, -1), y = c(58, 58), lwd = 1, lty = 2)
          lines(x = c(-1, -1), y = c(58, 57.5), lwd = 1, lty = 2)
          lines(x = c(-1, -2), y = c(57.5, 57.5), lwd = 2, lty = 2)
          
          lapply(moray.poly[2:8], polygon, col = "olivedrab", border = NA)
          lapply(moray.poly[9:20], polygon, col = "#c9e9f6", border = NA)
          lapply(moray.poly[21:25], polygon, col = "green", border = NA)
          lapply(moray.poly[1], polygon, col = "darkgreen", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r9, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(-4, 58.1, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-3):match(year, mfys)])){
          axis(1, at = seq(-3.6, -2, by = 0.8), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-3.6, -2, by = 0.8)), "W")))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-7)], 
                    mfys[(match(year, mfys)-3)])){
          axis(2, at = seq(57.7, 58.1, by = 0.2), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(57.7, 58.1, by = 0.2)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-8)]){
          legdens <- c(1.37, 0.75, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("1.35", cex = 0.5)
          stW <- strwidth(c("1.35","0.75","0.1"), cex=0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-2.1, 3), rep(58.05, 3) + 2*heightAdj, 
                   x1 = rep(-2.3, 3), y1 = rep(58.05, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-2.1, 3), rep(58.05, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-2.375, 3) - 0.5*stWdiff, rep(58.05, 3) + 2*heightAdj, 
               c("1.35","0.75","0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-2.1, 58, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-8.5, 57.55,
               legend = sapply(levels(as.factor(dat$strata_type)), simpleCap),
               col = sedimentCol,
               pt.bg = bubbleFill,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
      
      ## Portrait Orientation
    } else if(orientation == "Portrait"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU9_bubble_plot_po.")), 
           width = 300, height = 260, units = "mm", res = 350)
      par(mfrow = c(4, 3), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-4.1, -1.6),
             ylim = c(57.6, 58.2),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r9, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r9, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU9 strata
          segments(-4, 56.5, -2, 56.5, lty = 2, col = "#dadada")
          # FU9 strata
          lapply(moray.poly[2:8], polygon, col = NA, border = "#dadada")
          segments(-2, 57.96422, -2, 57.80697, lty = 1, col = "#ffffff")
          lapply(moray.poly[9:20], polygon, col = NA, border = "#dadada")
          lapply(moray.poly[21:25], polygon, col = NA, border = "#dadada")
          lapply(moray.poly[1], polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol[as.factor(nz$strata_type)], 
                  bg = bubbleFill[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-4, -2), y = c(58.5, 58.5), lwd = 1, lty = 2)
          lines(x = c(-2, -2), y = c(58.5, 58), lwd = 1, lty = 2)
          lines(x = c(-2, -1), y = c(58, 58), lwd = 1, lty = 2)
          lines(x = c(-1, -1), y = c(58, 57.5), lwd = 1, lty = 2)
          lines(x = c(-1, -2), y = c(57.5, 57.5), lwd = 2, lty = 2)
          
          lapply(moray.poly[2:8], polygon, col = "olivedrab", border = NA)
          lapply(moray.poly[9:20], polygon, col = "#c9e9f6", border = NA)
          lapply(moray.poly[21:25], polygon, col = "green", border = NA)
          lapply(moray.poly[1], polygon, col = "darkgreen", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r9, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(-4, 58.1, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-2):match(year, mfys)])){
          axis(1, at = seq(-3.6, -2, by = 0.8), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-3.6, -2, by = 0.8)), "W")))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-8)], 
                    mfys[(match(year, mfys)-5)], 
                    mfys[(match(year, mfys)-2)])){
          axis(2, at = seq(57.7, 58.1, by = 0.2), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(57.7, 58.1, by = 0.2)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-9)]){
          legdens <- c(1.37, 0.75, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("1.35", cex = 0.5)
          stW <- strwidth(c("1.35","0.75","0.1"), cex = 0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-2.1, 3), rep(58.05, 3) + 2*heightAdj, 
                   x1 = rep(-2.3, 3), y1 = rep(58.05, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-2.1, 3), rep(58.05, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-2.375, 3) - 0.5*stWdiff, rep(58.05, 3) + 2*heightAdj, 
               c("1.35","0.75","0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-2.1, 58, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-7, 57.55,
               legend = sapply(levels(as.factor(dat$strata_type)), simpleCap),
               col = sedimentCol,
               pt.bg = bubbleFill,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      
      # Close File Connection
      dev.off()
      
    }
  }
  
  
  
  ### FU10
  if(unique(dat$FU) == "Noup"){
    
    # Calculate bubble radii
    dat$radii <- sqrt(dat$average_density/pi)
    
    # SymbolScaling
    symbolScaling <- 0.2
    
    # Remove FALSE strata
    dat <- dat[dat$strata_type != "FALSE",]
    
    
    ## Landscape Orientation  
    if(orientation == "Landscape"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU10_bubble_plot.")), 
           width = 280, height = 190, units = "mm", res = 350)
      par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in unique(FU10_tvdata$year)){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-4, -3),
             ylim = c(59, 59.7),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r10, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r10, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU10 strata
          lapply(noup.poly[1:2], polygon, col = NA, border = "#dadada")
          lapply(noup.poly[3:5], polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol[2], 
                  bg = bubbleFill[2], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-4, -3), y = c(59, 59), lwd = 1, lty = 2)
          lines(x = c(-3, -3), y = c(59, 59.5), lwd = 1, lty = 2)
          lines(x = c(-4, -3), y = c(59.5, 59.5), lwd = 1, lty = 2)
          lines(x =c(-4, -4), y = c(59, 59.5), lwd = 1, lty = 2)
          
          lapply(noup.poly[1:2], polygon, col = "olivedrab", border = NA)
          lapply(noup.poly[3:5], polygon, col = "#c9e9f6", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r10, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        
        # Box and label
        box()
        text(-3.3, 59.6, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(2007, 2014, 2019)){
          axis(1, at = seq(-3.8, -3.2, by = 0.3), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-3.8, -3.2, by = 0.3)), "W")))
        }
        # y-axes
        if(i %in% c(1994, 2007)){
          axis(2, at = c(59.2, 59.5), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(c(59.2, 59.5)), "N")))
        }
        # Bubble Legend
        if(i == (2007)){
          legdens <- c(0.9, 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("1", cex = 0.5)
          stW <- strwidth(c("1","0.5","0.1"), cex=0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-3.7, 3), rep(59.05, 3) + 2*heightAdj, 
                   x1 = rep(-3.78, 3), y1 = rep(59.05, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-3.7, 3), rep(59.05, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-3.8, 3) - 0.5*stWdiff, rep(59.05, 3) + 2*heightAdj, 
               c("1", "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-3.7, 59, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      dev.off()
      
      
      ## Portrait Orientation
    } else if(orientation == "Portrait"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU10_bubble_plot.")), 
           width = 200, height = 280, units = "mm", res = 350)
      par(mfrow = c(3, 2), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in unique(FU10_tvdata$year)){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-4, -3),
             ylim = c(59, 59.7),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r10, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r10, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU10 strata
          lapply(noup.poly[1:2], polygon, col = NA, border = "#dadada")
          lapply(noup.poly[3:5], polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol[2], 
                  bg = bubbleFill[2], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-4, -3), y = c(59, 59), lwd = 1, lty = 2)
          lines(x = c(-3, -3), y = c(59, 59.5), lwd = 1, lty = 2)
          lines(x = c(-4, -3), y = c(59.5, 59.5), lwd = 1, lty = 2)
          lines(x =c(-4, -4), y = c(59, 59.5), lwd = 1, lty = 2)
          
          lapply(noup.poly[1:2], polygon, col = "olivedrab", border = NA)
          lapply(noup.poly[3:5], polygon, col = "#c9e9f6", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r10, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        
        # Box and label
        box()
        text(-3.3, 59.6, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(2014, 2019)){
          axis(1, at = seq(-3.8, -3.2, by = 0.3), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-3.8, -3.2, by = 0.3)), "W")))
        }
        # y-axes
        if(i %in% c(1994, 2006, 2014)){
          axis(2, at = c(59.2, 59.5), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(c(59.2, 59.5)), "N")))
        }
        # Bubble Legend
        if(i == (2014)){
          legdens <- c(0.9, 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("1", cex = 0.5)
          stW <- strwidth(c("1","0.5","0.1"), cex=0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-3.7, 3), rep(59.05, 3) + 2*heightAdj, 
                   x1 = rep(-3.78, 3), y1 = rep(59.05, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-3.7, 3), rep(59.05, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-3.8, 3) - 0.5*stWdiff, rep(59.05, 3) + 2*heightAdj, 
               c("1", "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-3.7, 59, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      dev.off()
      
    }
  }
  
  
  
  ### FU11 Bubble Plots
  if(unique(dat$FU) == "North Minch"){
    
    # Get years
    mfys <- unique(dat$year)
    dat <- dat[dat$year %in% c(mfys[(match(year, mfys)-11):match(year, mfys)]),]
    
    # Calculate bubble radii
    dat$radii <- sqrt(dat$average_density/pi)
    
    # SymbolScaling
    symbolScaling <- 0.15
    
    # Remove FALSE strata
    dat <- dat[dat$strata_type != "FALSE",]
    
    
    ## Landscape Orientation  
    if(orientation == "Landscape"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU11_bubble_plot_ls.")), 
           width = 350, height = 250, units = "mm", res = 350)
      par(mfrow = c(3, 4), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-7.1, -5.3),
             ylim = c(57.4, 58.7),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # FU11 boundaries
          segments(-7, 57.5, -5, 57.5, lty = 2, col = "#dadada")
          segments(-7, 57.5, -7, 59, lty = 2, col = "#dadada")
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r11, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r11, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU11 strata
          plot(poly_union_2007_2011_nm.rda, add = TRUE, 
               poly.args = list(col = NA, border = "#dadada"))
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#CC79A7", 
                  bg = rgb(204, 121, 167, 50, maxColorValue = 255), 
                  lwd = 2,
                  add = TRUE)
          
          # Zeroes
          points(zDat$lons, zDat$lats, 
                 pch = "x", 
                 cex = 2, 
                 lwd = 2, 
                 col = "#ff0000")
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-7, -5), y = c(57.5, 57.5), lwd = 1, lty = 2)
          lines(x = c(-7, -5), y = c(59, 59), lwd = 1, lty = 2)
          lines(x = c(-7, -7), y = c(57.5, 59), lwd = 1, lty = 2)
          lines(x = c(-5, -5), y = c(57.5, 59), lwd = 1, lty = 2)
          
          plot(poly_union_2007_2011_nm.rda, add = TRUE, 
               poly.args = list(col = 2, border = NA))
          # plot holes
          pts <- get.pts(poly_union_2007_2011_nm.rda)
          logic.pts.hole <- lapply(1:length(pts), function(x){pts[[x]]$hole == TRUE})
          pts.hole <- pts[logic.pts.hole == TRUE]
          pts.hole <- new("gpc.poly", pts=pts.hole)
          plot(pts.hole, add = TRUE, poly.args = list(col = "#c9e9f6", border = NA))
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r11, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
          # Zeroes
          points(zDat$lons, zDat$lats, 
                 pch = "x", 
                 cex = 2, 
                 lwd = 2, 
                 col = "#000000")
          
        }
        
        # Box and label
        box()
        text(-5.75, 57.64, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c((year-3):year)){
          axis(1, at = seq(-6.5, -5.75, by = 0.75), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-6.5, -5.75, by = 0.75)), "W")))
        }
        # y-axes
        if(i %in% c((year-11), (year-7), (year-3))){
          axis(2, at = seq(57.6, 58.4, by = 0.4), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(57.6, 58.4, by = 0.4)), "N")))
        }
        # Bubble Legend
        if(i == (year-3)){
          legdens <- c(2.8, 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("2.8", cex = 0.5)
          stW <- strwidth(c("2.8","0.5","0.1"), cex = 0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-6.73, 3), rep(58.45, 3) + 2*heightAdj, 
                   x1 = rep(-6.83, 3), y1 = rep(58.45, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-6.73, 3), rep(58.45, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-6.87, 3) - 0.5*stWdiff, rep(58.45, 3) + 2*heightAdj, 
               c("2.8", "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-6.73, 58.35, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-9.25, 57.3,
               legend = "VMS",
               col = "#CC79A7",
               pt.bg = rgb(204, 121, 167, 50, maxColorValue = 255),
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
      
      ## Portrait Orientation
    } else if(orientation == "Portrait"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU11_bubble_plot_po.")), 
           width = 280, height = 320, units = "mm", res = 350)
      par(mfrow = c(4, 3), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-7.1, -5.3),
             ylim = c(57.4, 58.7),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # FU11 boundaries
          segments(-7, 57.5, -5, 57.5, lty = 2, col = "#dadada")
          segments(-7, 57.5, -7, 59, lty = 2, col = "#dadada")
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r11, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r11, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU11 strata
          plot(poly_union_2007_2011_nm.rda, add = TRUE, 
               poly.args = list(col = NA, border = "#dadada"))
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#CC79A7", 
                  bg = rgb(204, 121, 167, 50, maxColorValue = 255), 
                  lwd = 2,
                  add = TRUE)
          
          # Zeroes
          points(zDat$lons, zDat$lats, 
                 pch = "x", 
                 cex = 2, 
                 lwd = 2, 
                 col = "#ff0000")
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-7, -5), y = c(57.5, 57.5), lwd = 1, lty = 2)
          lines(x = c(-7, -5), y = c(59, 59), lwd = 1, lty = 2)
          lines(x = c(-7, -7), y = c(57.5, 59), lwd = 1, lty = 2)
          lines(x = c(-5, -5), y = c(57.5, 59), lwd = 1, lty = 2)
          
          plot(poly_union_2007_2011_nm.rda, add = TRUE, 
               poly.args = list(col = 2, border = NA))
          # plot holes
          pts <- get.pts(poly_union_2007_2011_nm.rda)
          logic.pts.hole <- lapply(1:length(pts), function(x){pts[[x]]$hole == TRUE})
          pts.hole <- pts[logic.pts.hole == TRUE]
          pts.hole <- new("gpc.poly", pts=pts.hole)
          plot(pts.hole, add = TRUE, poly.args = list(col = "#c9e9f6", border = NA))
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r11, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
          # Zeroes
          points(zDat$lons, zDat$lats, 
                 pch = "x", 
                 cex = 2, 
                 lwd = 2, 
                 col = "#000000")
          
        }
        
        # Box and label
        box()
        text(-5.75, 57.64, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c((year-2):year)){
          axis(1, at = seq(-6.5, -5.75, by = 0.75), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-6.5, -5.75, by = 0.75)), "W")))
        }
        # y-axes
        if(i %in% c((year-11), (year-8), (year-5), (year-2))){
          axis(2, at = seq(57.6, 58.4, by = 0.4), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(57.6, 58.4, by = 0.4)), "N")))
        }
        # Bubble Legend
        if(i == (year-2)){
          legdens <- c(2.8, 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("2.8", cex = 0.5)
          stW <- strwidth(c("2.8","0.5","0.1"), cex = 0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-6.73, 3), rep(58.45, 3) + 2*heightAdj, 
                   x1 = rep(-6.83, 3), y1 = rep(58.45, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-6.73, 3), rep(58.45, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-6.87, 3) - 0.5*stWdiff, rep(58.45, 3) + 2*heightAdj, 
               c("2.8", "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-6.73, 58.35, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-8.25, 57.3,
               legend = "VMS",
               col = "#CC79A7",
               pt.bg = rgb(204, 121, 167, 50, maxColorValue = 255),
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
    }
  }
  
  
  
  ### FU12 Bubble Plots
  if(unique(dat$FU) == "South Minch"){
    
    # Get years
    mfys <- unique(dat$year)
    dat <- dat[dat$year %in% c(mfys[(match(year, mfys)-11):match(year, mfys)]),]
    
    # Calculate bubble radii
    dat$radii <- sqrt(dat$average_density/pi)
    
    # SymbolScaling
    symbolScaling <- 0.15
    
    # Remove FALSE strata
    dat <- dat[dat$strata_type != "FALSE",]
    
    
    ## Landscape Orientation  
    if(orientation == "Landscape"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU12_bubble_plot_ls.")), 
           width = 380, height = 230, units = "mm", res = 350)
      par(mfrow = c(3, 4), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-8, -5.5),
             ylim = c(56, 57.5),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # FU12 area strata
          segments(-6.75, 56.6, -6.75, 57.16, 
                   lty = 2, 
                   col = "#dadada")
          segments(-6.75, 57.16, -6.51, 57.33, 
                   lty = 2, 
                   col = "#dadada")      
          segments(-6.75, 56.6, -8, 56.4, lty = 2, col = "#dadada")
          segments(-6.75, 56.6, -5, 56.6, lty = 2, col = "#dadada")
          # FU12 Boundary
          segments(-8, 57.5, -8, 56, lty = 2, col = "#dadada")
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r12, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r12, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol[as.factor(nz$strata_type)], 
                  bg = bubbleFill[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-8, -5), y = c(57.5, 57.5), lwd = 1, lty = 2)
          lines(x = c(-8, -5.5), y = c(56, 56), lwd = 1, lty = 2)
          lines(x = c(-8, -8), y = c(57.5, 56), lwd = 1, lty = 2)
          
          lapply(south.minch.poly[17:50], polygon, col = "olivedrab", border = NA)
          lapply(south.minch.poly[51], polygon, col = "#c9e9f6", border = NA)
          lapply(south.minch.poly[53:72], polygon, col = "green", border = NA)
          lapply(south.minch.poly[52], polygon, col = "#c9e9f6", border = NA)
          lapply(south.minch.poly[1:16], polygon, col = "darkgreen", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r12, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(-8, 57.3, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-3):match(year, mfys)])){
          axis(1, at = seq(-7.5, -6, by = 0.5), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-7.5, -6, by = 0.5)), "W")))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-7)], 
                    mfys[(match(year, mfys)-3)])){
          axis(2, at = seq(56.5, 57, by=0.5), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(56.5, 57, by=0.5)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-3)]){
          legdens <- c(2.5, 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("2.5", cex = 0.5)
          stW <- strwidth(c("2.5","0.5","0.1"), cex=0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-7.5, 3), rep(56.1, 3) + 2*heightAdj, 
                   x1 = rep(-7.75, 3), y1 = rep(56.1, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-7.5, 3), rep(56.1, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-7.8, 3) - 0.5*stWdiff, rep(56.1, 3) + 2*heightAdj, 
               c("2.5", "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-7.5, 56, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-12.8, 55.85,
               legend = sapply(levels(as.factor(dat$strata_type)), simpleCap),
               col = sedimentCol,
               pt.bg = bubbleFill,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
      
      ## Portrait Orientation
    } else if(orientation == "Portrait"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU12_bubble_plot_po.")), 
           width = 300, height = 320, units = "mm", res = 350)
      par(mfrow = c(4, 3), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-8, -5.5),
             ylim = c(56, 57.5),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # FU12 area strata
          segments(-6.75, 56.6, -6.75, 57.16, 
                   lty = 2, 
                   col = "#dadada")
          segments(-6.75, 57.16, -6.51, 57.33, 
                   lty = 2, 
                   col = "#dadada")      
          segments(-6.75, 56.6, -8, 56.4, lty = 2, col = "#dadada")
          segments(-6.75, 56.6, -5, 56.6, lty = 2, col = "#dadada")
          # FU12 Boundary
          segments(-8, 57.5, -8, 56, lty = 2, col = "#dadada")
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r12, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r12, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol[as.factor(nz$strata_type)], 
                  bg = bubbleFill[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-8, -5), y = c(57.5, 57.5), lwd = 1, lty = 2)
          lines(x = c(-8, -5.5), y = c(56, 56), lwd = 1, lty = 2)
          lines(x = c(-8, -8), y = c(57.5, 56), lwd = 1, lty = 2)
          
          lapply(south.minch.poly[17:50], polygon, col = "olivedrab", border = NA)
          lapply(south.minch.poly[51], polygon, col = "#c9e9f6", border = NA)
          lapply(south.minch.poly[53:72], polygon, col = "green", border = NA)
          lapply(south.minch.poly[52], polygon, col = "#c9e9f6", border = NA)
          lapply(south.minch.poly[1:16], polygon, col = "darkgreen", border = NA)
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r12, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(-8, 57.3, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-2):match(year, mfys)])){
          axis(1, at = seq(-7.5, -6, by = 0.5), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-7.5, -6, by = 0.5)), "W")))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-8)], 
                    mfys[(match(year, mfys)-5)], 
                    mfys[(match(year, mfys)-2)])){
          axis(2, at = seq(56.5, 57, by=0.5), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(56.5, 57, by=0.5)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-2)]){
          legdens <- c(2.5, 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("2.5", cex = 0.5)
          stW <- strwidth(c("2.5","0.5","0.1"), cex = 0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-7.5, 3), rep(56.1, 3) + 2*heightAdj, 
                   x1 = rep(-7.75, 3), y1 = rep(56.1, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-7.5, 3), rep(56.1, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-7.8, 3) - 0.5*stWdiff, rep(56.1, 3) + 2*heightAdj, 
               c("2.5", "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-7.5, 56, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-10.8, 55.85,
               legend = sapply(levels(as.factor(dat$strata_type)), simpleCap),
               col = sedimentCol,
               pt.bg = bubbleFill,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
    }
  }
  
  
  
  ### FU13
  if(unique(dat$FU) == "Clyde"){
    
    # Is Jura provided?
    if(is.null(jura) == FALSE){
      dat13 <- rbind(dat, jura)
    } else {dat13 <- dat}
    
    # Get years
    mfys <- unique(dat13$year)
    dat13 <- dat13[dat13$year %in% c(mfys[(match(year, mfys)-11):match(year, mfys)]),]
    
    # Calculate bubble radii
    dat13$radii <- sqrt(dat13$average_density/pi)
    
    # SymbolScaling
    symbolScaling <- 0.15
    
    # Remove FALSE strata
    dat13 <- dat13[dat13$strata_type != "FALSE",]
    
    
    ## Landscape Orientation  
    if(orientation == "Landscape"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU13_bubble_plot_ls.")), 
           width = 380, height = 230, units = "mm", res = 350)
      par(mfrow = c(3, 4), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat13[dat13$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat13$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-6.2, -4.6),
             ylim = c(55, 56),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # FU13 Boundary
          segments(-6, 55, -6, 56, lty = 2, col = "#dadada")
          segments(-6, 56, -4, 56, lty = 2, col = "#dadada")
          segments(-6.75, 56.6, -5, 56.6, lty = 2, col = "#dadada")
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r13, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r13, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU13 strata
          lapply(clyde.poly[26:35], polygon, col = NA, border = "#dadada")
          #lapply(clyde.poly[1:16], polygon, col = NA, border = "#dadada")
          lapply(clyde.poly[44:49], polygon, col = NA, border = "#dadada")
          #lapply(clyde.poly[17:25], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[3:4], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[7], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[5:6], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[1], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[2], polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol[as.factor(nz$strata_type)], 
                  bg = bubbleFill[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-6, -6), y = c(56, 55), lwd = 1, lty = 2)
          lines(x = c(-6, -5), y = c(55, 55), lwd = 1, lty = 2)
          lines(x = c(-6, -5.5), y = c(56, 56), lwd = 1, lty = 2)
          
          lapply(clyde.poly[26:35], polygon, col = "olivedrab", border = NA)
          lapply(clyde.poly[1:16], polygon, col = "#c9e9f6", border = NA)
          lapply(clyde.poly[44:49], polygon, col = "green", border = NA)
          lapply(clyde.poly[17:25], polygon, col = "darkgreen", border = NA)
          
          if(is.null(jura) == FALSE){
            lapply(jura.poly[3:4], polygon, col = "olivedrab", border = NA)
            lapply(jura.poly[7], polygon, col = "green", border = NA)
            lapply(jura.poly[5:6], polygon, col = "olivedrab", border = NA)
            lapply(jura.poly[1], polygon, col = "#c9e9f6", border = NA)
            lapply(jura.poly[2], polygon, col = "darkgreen", border = NA)
          }
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r13, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(-5, 55.1, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-3):match(year, mfys)])){
          axis(1, at = seq(-6, -5, by = 0.5), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-6, -5, by = 0.5)), "W")))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-7)], 
                    mfys[(match(year, mfys)-3)])){
          axis(2, at = seq(55.2, 55.8, by = 0.3), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(55.2, 55.8, by = 0.3)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-3)]){
          legdens <- c(2.83, 1, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat13$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("2.8", cex = 0.5)
          stW <- strwidth(c("2.8","1","0.1"), cex = 0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-5.88, 3), rep(55.15, 3) + 2*heightAdj, 
                   x1 = rep(-6, 3), y1 = rep(55.15, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-5.88, 3), rep(55.15, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat13$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-6.04, 3) - 0.5*stWdiff, rep(55.15, 3) + 2*heightAdj, 
               c("2.8", "1", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-5.88, 55.05, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-9, 54.9,
               legend = sapply(levels(as.factor(dat13$strata_type)), simpleCap),
               col = sedimentCol,
               pt.bg = bubbleFill,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
      
      ## Portrait Orientation
    } else if(orientation == "Portrait"){
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU13_bubble_plot_po.")), 
           width = 300, height = 320, units = "mm", res = 350)
      par(mfrow = c(4, 3), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-11):match(year, mfys)]){
        
        xx <- dat13[dat13$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat13$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-6.2, -4.6),
             ylim = c(55, 56),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # FU13 Boundary
          segments(-6, 55, -6, 56, lty = 2, col = "#dadada")
          segments(-6, 56, -4, 56, lty = 2, col = "#dadada")
          segments(-6.75, 56.6, -5, 56.6, lty = 2, col = "#dadada")
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          } else {
            plot(r13, add = TRUE, col = grey(51:100/100), legend = FALSE)
            plot(r13, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          }
          
          # FU13 strata
          lapply(clyde.poly[26:35], polygon, col = NA, border = "#dadada")
          #lapply(clyde.poly[1:16], polygon, col = NA, border = "#dadada")
          lapply(clyde.poly[44:49], polygon, col = NA, border = "#dadada")
          #lapply(clyde.poly[17:25], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[3:4], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[7], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[5:6], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[1], polygon, col = NA, border = "#dadada")
          lapply(jura.poly[2], polygon, col = NA, border = "#dadada")
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = sedimentCol[as.factor(nz$strata_type)], 
                  bg = bubbleFill[as.factor(nz$strata_type)], 
                  lwd = 2,
                  add = TRUE)
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          lines(x = c(-6, -6), y = c(56, 55), lwd = 1, lty = 2)
          lines(x = c(-6, -5), y = c(55, 55), lwd = 1, lty = 2)
          lines(x = c(-6, -5.5), y = c(56, 56), lwd = 1, lty = 2)
          
          lapply(clyde.poly[26:35], polygon, col = "olivedrab", border = NA)
          lapply(clyde.poly[1:16], polygon, col = "#c9e9f6", border = NA)
          lapply(clyde.poly[44:49], polygon, col = "green", border = NA)
          lapply(clyde.poly[17:25], polygon, col = "darkgreen", border = NA)
          
          if(is.null(jura) == FALSE){
            lapply(jura.poly[3:4], polygon, col = "olivedrab", border = NA)
            lapply(jura.poly[7], polygon, col = "green", border = NA)
            lapply(jura.poly[5:6], polygon, col = "olivedrab", border = NA)
            lapply(jura.poly[1], polygon, col = "#c9e9f6", border = NA)
            lapply(jura.poly[2], polygon, col = "darkgreen", border = NA)
          }
          
          # Add terrestrial map
          if(elevation == FALSE){
            polygon(uk.coast$lons, uk.coast$lats, col = "#cccccc", border = "#000000")
          } else {
            plot(r13, add = TRUE, col = grey(51:100/100), legend = FALSE)
          }
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
        }
        
        # Zeroes
        points(zDat$lons, zDat$lats, 
               pch = "x", 
               cex = 2, 
               lwd = 2, 
               col = "#ff0000")
        # Box and label
        box()
        text(-5, 55.1, labels = i, cex = 3, font = 2, pos = 4)
        # x-axes
        if(i %in% c(mfys[(match(year, mfys)-2):match(year, mfys)])){
          axis(1, at = seq(-6, -5, by = 0.5), cex.axis = 1.5, 
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(-6, -5, by = 0.5)), "W")))
        }
        # y-axes
        if(i %in% c(mfys[(match(year, mfys)-11)], 
                    mfys[(match(year, mfys)-8)], 
                    mfys[(match(year, mfys)-5)], 
                    mfys[(match(year, mfys)-2)])){
          axis(2, at = seq(55.2, 55.8, by = 0.3), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(55.2, 55.8, by = 0.3)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys)-2)]){
          legdens <- c(2.83, 1, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat13$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("2.8", cex = 0.5)
          stW <- strwidth(c("2.8","1","0.1"), cex=0.75)
          stWdiff <- stW - stW[1]
          segments(rep(-5.88, 3), rep(55.15, 3) + 2*heightAdj, 
                   x1 = rep(-6, 3), y1 = rep(55.15, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(-5.88, 3), rep(55.15, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat13$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(-6.04, 3) - 0.5*stWdiff, rep(55.15, 3) + 2*heightAdj, 
               c("2.8", "1", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(-5.88, 55.05, 
               bty = "n", 
               col = "#000000",
               cex = 1.2,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      if(stratum_legend == TRUE & style == "new"){
        legend(-8, 54.9,
               legend = sapply(levels(as.factor(dat13$strata_type)), simpleCap),
               col = sedimentCol,
               pt.bg = bubbleFill,
               horiz = TRUE, 
               xpd = NA, 
               pch = 21, 
               cex = 2,
               pt.cex = 2,
               pt.lwd = 2,
               bty = "n")
      }
      
      # Close File Connection
      dev.off()
      
    }
  }
  
  
  
  ### FU11 Bubble Plots
  if(unique(dat$FU) == "Devil's Hole"){
    
    # Get years
    mfys <- unique(dat$year)
    dat <- dat[dat$year %in% c(mfys[(match(year, mfys)-9):match(year, mfys)]),]
    
    # Calculate bubble radii
    dat$radii <- sqrt(dat$average_density/pi)
    
    # SymbolScaling
    symbolScaling <- 0.15
    
    # Remove FALSE strata
    dat <- dat[dat$strata_type != "FALSE",]
    
    
    ## Landscape Orientation  
    if(orientation == "Landscape"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU34_bubble_plot_ls.")), 
           width = 350, height = 160, units = "mm", res = 350)
      par(mfrow = c(2, 5), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-9):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-0.5, 2.5), 
             ylim = c(55.9, 57.6),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # FU34 boundaries
          lines(x = c(0, 2), y = c(57.5, 57.5), lwd = 2, lty = 2, col = "#dadada")
          lines(x = c(0, 2), y = c(56.0, 56.0), lwd = 2, lty = 2, col = "#dadada")   
          lines(x = c(2, 2), y = c(56, 57.5), lwd = 2, lty = 2, col = "#dadada")
          lines(x = c(0, 0), y = c(56, 57.5), lwd = 2, lty = 2, col = "#dadada")
          
          
          # Add terrestrial map
          # if(elevation == FALSE){
          #   polygon(uk.coast$lons, uk.coast$lats, col = "#dadada", border = "#dadada")
          # } else {
          #   plot(r11, add = TRUE, col = grey(51:100/100), legend = FALSE)
          #   plot(r11, add = TRUE, alpha = 0.2, col = terrain.colors(255), legend = FALSE)
          # }
          
          # FU11 strata
          plot(devils.hole.vms.poly, add = TRUE, 
               poly.args = list(col = NA, border = "#dadada"))
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#CC79A7", 
                  bg = rgb(204, 121, 167, 50, maxColorValue = 255), 
                  lwd = 2,
                  add = TRUE)
          
          # Zeroes
          points(zDat$lons, zDat$lats, 
                 pch = "x", 
                 cex = 2, 
                 lwd = 2, 
                 col = "#ff0000")
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          # FU34 boundaries
          lines(x = c(0, 2), y = c(57.5, 57.5), lwd = 2, lty = 2, col = "#000000")
          lines(x = c(0, 2), y = c(56.0, 56.0), lwd = 2, lty = 2, col = "#000000")   
          lines(x = c(2, 2), y = c(56, 57.5), lwd = 2, lty = 2, col = "#000000")
          lines(x = c(0, 0), y = c(56, 57.5), lwd = 2, lty = 2, col = "#000000")
          
          plot(devils.hole.vms.poly, add = TRUE, 
               poly.args = list(col = 2, border = NA))
          # plot holes
          pts <- get.pts(devils.hole.vms.poly)
          logic.pts.hole <- lapply(1:length(pts), function(x){pts[[x]]$hole == TRUE})
          pts.hole <- pts[logic.pts.hole == TRUE]
          pts.hole <- new("gpc.poly", pts=pts.hole)
          plot(pts.hole, add = TRUE, poly.args = list(col = "#c9e9f6", border = NA))
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
          # Zeroes
          points(zDat$lons, zDat$lats, 
                 pch = "x", 
                 cex = 2, 
                 lwd = 2, 
                 col = "#000000")
          
        }
        
        # Box and year label
        box()
        if(style == "old"){
          polygon(x = c(-0.1, 0.1, 0.1, -0.1), y = c(56.02, 56.02, 56.2, 56.2), 
                  col = "#c9e9f6", border = "#c9e9f6")
        }
        text(-0.45, 56.1, labels = i, cex = 3, font = 2, pos = 4)
        
        # x-axes
        if(i %in% mfys[c((match(year, mfys) - seq(4, 0, by = -1)))]){
          axis(1, at = seq(0, 2, by = 1), cex.axis = 1.5, 
               labels = c(0, parse(text = sprintf("%s^o*%s", 
                                                  abs(seq(1, 2, by = 1)), "W"))))
        }
        # y-axes
        if(i %in% mfys[c((match(year, mfys) - seq(9, 4, by = -5)))]){
          axis(2, at = seq(56.2, 57.2, by = 0.5), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(56.2, 57.2, by = 0.5)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys) - 4)]){
          legdens <- c(round(max(FU34_tvdata$average_density, na.rm = TRUE), 1), 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("2.8", cex = 0.5)
          stW <- strwidth(c("2.8","0.5","0.1"), cex = 0.75)
          stWdiff <- stW - stW[1]
          segments(rep(0.7, 3), rep(57.2, 3) + 2*heightAdj, 
                   x1 = rep(0.4, 3), y1 = rep(57.2, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(0.7, 3), rep(57.2, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(0.3, 3) - 0.5*stWdiff, rep(57.2, 3) + 2*heightAdj, 
               c(as.character(round(max(FU34_tvdata$average_density, na.rm = TRUE), 1)), 
                 "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(0.7, 57.075, 
               bty = "n", 
               col = "#000000",
               cex = 0.9,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      # if(stratum_legend == TRUE & style == "new"){
      #   legend(-9.25, 57.3,
      #          legend = "VMS",
      #          col = "#CC79A7",
      #          pt.bg = rgb(204, 121, 167, 50, maxColorValue = 255),
      #          horiz = TRUE, 
      #          xpd = NA, 
      #          pch = 21, 
      #          cex = 2,
      #          pt.cex = 2,
      #          pt.lwd = 2,
      #          bty = "n")
      # }
      
      # Close File Connection
      dev.off()
      
      
      ## Portrait Orientation
    } else if(orientation == "Portrait"){
      
      # Open File Connection
      jpeg(get.fname(paste0(wk.dir, "FU34_bubble_plot_po.")), 
           width = 150, height = 320, units = "mm", res = 350)
      par(mfrow = c(5, 2), mar = c(0, 0, 0, 0), oma = c(5.5, 3, 0.2, 0.2))
      
      for(i in mfys[(match(year, mfys)-9):match(year, mfys)]){
        
        xx <- dat[dat$year == i,]
        
        # Get scaling for bubble plot
        bubbleScale <- max(xx$radii, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)
        
        # Get zero/non-zero data
        nz <- xx[xx$average_density>0,]
        ind <- rev(order(nz$average_density))
        nz <- nz[ind,]
        zDat <- xx[xx$average_density==0,]
        
        # Plot
        plot(1, 1,
             xlim = c(-0.5, 2.5), 
             ylim = c(55.9, 57.6),
             xaxs = "i",
             yaxs = "i",
             type = "n",
             xlab = "",
             ylab = "",
             axes = FALSE)
        
        # New style map
        if(style == "new"){
          
          # FU34 boundaries
          lines(x = c(0, 2), y = c(57.5, 57.5), lwd = 2, lty = 2, col = "#dadada")
          lines(x = c(0, 2), y = c(56.0, 56.0), lwd = 2, lty = 2, col = "#dadada")   
          lines(x = c(2, 2), y = c(56, 57.5), lwd = 2, lty = 2, col = "#dadada")
          lines(x = c(0, 0), y = c(56, 57.5), lwd = 2, lty = 2, col = "#dadada")
          
          # FU11 strata
          plot(devils.hole.vms.poly, add = TRUE, 
               poly.args = list(col = NA, border = "#dadada"))
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#CC79A7", 
                  bg = rgb(204, 121, 167, 50, maxColorValue = 255), 
                  lwd = 2,
                  add = TRUE)
          
          # Zeroes
          points(zDat$lons, zDat$lats, 
                 pch = "x", 
                 cex = 2, 
                 lwd = 2, 
                 col = "#ff0000")
          
        } else if(style == "old"){
          
          polygon(x = c(-10, 10, 10, -10), y = c(50, 50, 70, 70), col = "#c9e9f6")
          
          # FU34 boundaries
          lines(x = c(0, 2), y = c(57.5, 57.5), lwd = 2, lty = 2, col = "#000000")
          lines(x = c(0, 2), y = c(56.0, 56.0), lwd = 2, lty = 2, col = "#000000")   
          lines(x = c(2, 2), y = c(56, 57.5), lwd = 2, lty = 2, col = "#000000")
          lines(x = c(0, 0), y = c(56, 57.5), lwd = 2, lty = 2, col = "#000000")
          
          plot(devils.hole.vms.poly, add = TRUE, 
               poly.args = list(col = 2, border = NA))
          # plot holes
          pts <- get.pts(devils.hole.vms.poly)
          logic.pts.hole <- lapply(1:length(pts), function(x){pts[[x]]$hole == TRUE})
          pts.hole <- pts[logic.pts.hole == TRUE]
          pts.hole <- new("gpc.poly", pts=pts.hole)
          plot(pts.hole, add = TRUE, poly.args = list(col = "#c9e9f6", border = NA))
          
          # Bubbles
          symbols(nz$lons, nz$lats, 
                  circles = nz$radii, 
                  inches = symbolScaling * bubbleScale,
                  fg = "#000000", 
                  bg = NA, 
                  lwd = 2,
                  add = TRUE)
          
          # Zeroes
          points(zDat$lons, zDat$lats, 
                 pch = "x", 
                 cex = 2, 
                 lwd = 2, 
                 col = "#000000")
          
        }
        
        # Box and year label
        box()
        if(style == "old"){
          polygon(x = c(-0.1, 0.1, 0.1, -0.1), y = c(56.02, 56.02, 56.2, 56.2), 
                  col = "#c9e9f6", border = "#c9e9f6")
        }
        text(-0.45, 56.1, labels = i, cex = 3, font = 2, pos = 4)
        
        # x-axes
        if(i %in% mfys[c((match(year, mfys) - seq(1, 0, by = -1)))]){
          axis(1, at = seq(0, 2, by = 1), cex.axis = 1.5, 
               labels = c(0, parse(text = sprintf("%s^o*%s", 
                                                  abs(seq(1, 2, by = 1)), "W"))))
        }
        # y-axes
        if(i %in% mfys[c((match(year, mfys) - seq(9, 1, by = -2)))]){
          axis(2, at = seq(56.2, 57.2, by = 0.5), cex.axis = 1.5,
               labels = parse(text = sprintf("%s^o*%s", 
                                             abs(seq(56.2, 57.2, by = 0.5)), "N")))
        }
        # Bubble Legend
        if(i == mfys[(match(year, mfys) - 1)]){
          legdens <- c(round(max(FU34_tvdata$average_density, na.rm = TRUE), 1), 0.5, 0.1)
          legRad <- sqrt(legdens/pi)
          hin <- par("pin")[2]
          ydiff <- par("usr")[c(4,3)]
          densPerInch <- abs(ydiff[2]-ydiff[1]) / hin
          radPerInch <- sqrt(max(dat$average_density)/pi)/symbolScaling
          heightAdj <- legRad/radPerInch*densPerInch
          # Get some adjustment values
          tAdj <- strheight("2.8", cex = 0.5)
          stW <- strwidth(c("2.8","0.5","0.1"), cex = 0.75)
          stWdiff <- stW - stW[1]
          segments(rep(0.7, 3), rep(57.2, 3) + 2*heightAdj, 
                   x1 = rep(0.4, 3), y1 = rep(57.2, 3) + 2*heightAdj, 
                   col="#000000")
          symbols(rep(0.7, 3), rep(57.2, 3) + heightAdj, 
                  circles = legRad, 
                  inches = symbolScaling * (max(legRad, na.rm = TRUE)/max(dat$radii, na.rm = TRUE)), 
                  fg = "#000000",
                  add = TRUE)
          text(rep(0.3, 3) - 0.5*stWdiff, rep(57.2, 3) + 2*heightAdj, 
               c(as.character(round(max(FU34_tvdata$average_density, na.rm = TRUE), 1)), 
                 "0.5", "0.1"), 
               col = "#000000",
               cex = 0.75)
          text(0.7, 57.075, 
               bty = "n", 
               col = "#000000",
               cex = 0.9,
               labels = expression(atop(Mean~Burrows~per~m^2)))
        }
      }
      
      # Sediment Type Legend
      # if(stratum_legend == TRUE & style == "new"){
      #   legend(-8.25, 57.3,
      #          legend = "VMS",
      #          col = "#CC79A7",
      #          pt.bg = rgb(204, 121, 167, 50, maxColorValue = 255),
      #          horiz = TRUE, 
      #          xpd = NA, 
      #          pch = 21, 
      #          cex = 2,
      #          pt.cex = 2,
      #          pt.lwd = 2,
      #          bty = "n")
      # }
      
      # Close File Connection
      dev.off()
      
    }
  }
  
}
