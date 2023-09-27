tv_density_beans <- function(TVdens, FU, wk.dir, boxes = TRUE, legend = TRUE,
                             Jura = FALSE, JuraDat = NULL, Stratified = TRUE,
                             timerange = c((max(TVdens$year)-4):max(TVdens$year))){
  
  
  ## Check Jura Conditions
  # Cannot plot asymmetric boxplots
  # Must include a Jura UWTV survey data object
  if (Jura == TRUE && boxes == TRUE){
    stop("Cannot plot boxes on combined FU13 beans")
  }
  if(Jura == TRUE){
    if(is.null(JuraDat)){stop("JuraDat data object must be specified")}
    TVdens <- as.data.frame(rbind(TVdens, JuraDat))
  }
  
  # Check FU argument to ensure it is valid
  f.u <- check.fu(FU)
  
  # Set up colour palettes
  sedimentCol <- colorRampPalette(c("#70543e", "#c2b280"))
  allDatCols <- colorRampPalette(c("#7f7f7f", "#56B4E9"))
  
  # Function to avoid overwriting other tvDensityBean plots
  get.fname <- function(base.name){
    fname <- function(i){paste(base.name, i, ".jpeg", sep="")}
    out <- fname(i <- 1)
    while(file.exists(out)){out <- fname(i <- i + 1)}
    return (out)
  }
  
  # Function to capitalize first letter of each word
  simpleCap <- function(x) {
    x <- tolower(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  # Specify filename with/without boxes
  if(Jura == FALSE){
    if(boxes == TRUE){
      fileName <- paste(gsub(" ", "_", f.u), "_survey_density_bean_boxplot", sep="")
    } else {
      fileName <- paste(gsub(" ", "_", f.u), "_survey_density_beanplot", sep="")}
  } else {
    if(boxes == TRUE){
      fileName <- "FU13_survey_density_bean_boxplot"
    } else {
      fileName <- "FU13_survey_density_beanplot"}
  }
  
  # Specify time range
  if(length(unique(TVdens$year)) > 5){
    TVdens <- TVdens[TVdens$year %in% timerange,]
  }
  
  # Open .jpeg file connection
  jpeg(get.fname(paste(wk.dir, fileName, sep="")), 
       width = 210, height = 90, units = "mm", res = 350)
  par(xpd=TRUE, mar=c(3.1, 4.1, 2.1, 1.1))
  
  
  ## Plotting options
  if(Stratified == FALSE){
    
    if(Jura == TRUE){
      
      survArea <- as.factor(substr(TVdens$station,1,2))
      
      # Set up colours
      colVec <- allDatCols(length(unique(TVdens$year)))
      colVecList <- as.list(colVec)
      colList <- lapply(colVecList, function(x){x <- c(x, "#ffffff", "#000000", NA)})
      juraList <- rep(list(c("#ffffff", "#000000", "#000000", NA)), 
                      times = length(unique(TVdens$year)))
      idx <- order(c(seq_along(colList), seq_along(juraList)))
      colList <- (c(colList, juraList))[idx]
      
      # Regular Beanplot
      beanplot(TVdens$average_density ~  survArea + TVdens$year,
               col = colList,
               border = rep(colVec, each = 2),
               ll = 0.05, 
               log = "", 
               method = "jitter", 
               side = "both",
               cutmin = -0.01,
               what = c(FALSE, TRUE, TRUE, TRUE),
               at = 1:length(unique(TVdens$year)),
               maxwidth = 0.75,
               axes = FALSE)
      axis(1, at = 1:length(unique(TVdens$year)), labels = unique(TVdens$year))
      axis(2)
      mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
      box()
      
    } else {
      
      if(boxes == TRUE){
        
        # Set up colours
        colVec <- allDatCols(length(unique(TVdens$year)))
        colVecList <- as.list(colVec)
        colList <- lapply(colVecList, function(x){x <- c(rep(x, times = 2), "#000000", NA)})
        
        beanplot(TVdens$average_density ~ TVdens$year,
                 col = colList,
                 border = colVec,
                 ll = 0.05, 
                 log = "", 
                 method = "jitter", 
                 cutmin = -0.01,
                 what = c(FALSE, TRUE, TRUE, TRUE),
                 at = 1:length(unique(TVdens$year)),
                 maxwidth = 0.75,
                 axes = FALSE)
        boxplot(TVdens$average_density ~ TVdens$year, 
                add = TRUE,
                boxwex = 0.1,
                col = NA,
                border = "#ffffff",
                axes = FALSE,
                pch = 8, 
                cex = 0.5,
                lwd = 1,
                staplewex = 0,
                lty = 1)
        axis(1, at = 1:length(unique(TVdens$year)), labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        
      } else {
        
        # Set up colours
        colVec <- allDatCols(length(unique(TVdens$year)))
        colVecList <- as.list(colVec)
        colList <- lapply(colVecList, function(x){x <- c(x, "#ffffff", "#000000", NA)})
        
        beanplot(TVdens$average_density ~ TVdens$year,
                           col = colList,
                           border = colVec,
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           at = 1:length(unique(TVdens$year)),
                           maxwidth = 0.75,
                           axes = FALSE)
        axis(1, at = 1:length(unique(TVdens$year)), labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        
      }
      
    }
    
  } else {
    
    if(f.u %in% c("devils hole", "north minch")){
      
      ## North Minch & Devil's Hole Plots
      
      if(boxes == TRUE){
        # Beanplot with boxplots (no staples)
        beanplot(TVdens$average_density ~ TVdens$year,
                           col = list(c("#D55E00", "#D55E00", "#ffffff", NA)),
                           border = "#D55E00",
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           at = 1:length(unique(TVdens$year)),
                           maxwidth = 0.75,
                           axes = FALSE)
        boxplot(TVdens$average_density ~ TVdens$year, 
                add = TRUE,
                boxwex = 0.1,
                col = NA,
                border = "#ffffff",
                axes = FALSE,
                pch = 8, 
                cex = 0.5,
                lwd = 1,
                staplewex = 0,
                lty = 1)
        axis(1, at = 1:length(unique(TVdens$year)), labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        if(legend == TRUE){
          legend(median(1:length(unique(TVdens$year))), par()$usr[4]*1.2, 
                 bty = "n", 
                 pt.cex = 2, 
                 xjust = 0.5,
                 legend = "VMS Fishing Area", 
                 pch = 19, 
                 horiz = TRUE, 
                 col = "#D55E00")
        }
        
      } else {
        
        # Regular Beanplot
        beanplot(TVdens$average_density ~ TVdens$year,
                           col = list(c("#D55E00", "#ffffff", "#ffffff", NA)),
                           border = "#D55E00",
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           maxwidth = 0.75,
                           axes = FALSE)
        axis(1, at = 1:length(unique(TVdens$year)), labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        if(legend == TRUE){
          legend(median(1:length(unique(TVdens$year))), par()$usr[4]*1.2, 
                 bty = "n", 
                 pt.cex = 2, 
                 xjust = 0.5,
                 legend = "VMS Fishing Area", 
                 pch = 19, 
                 horiz = TRUE, 
                 col = "#D55E00")
        }
        
      }
      
    } else if(f.u %in% c("fladen")){
      
      # Fladen plots
      # Set up bean placement
      ind1 <- 4*length(unique(TVdens$year))+(1*length(unique(TVdens$year))-1)
      a <- seq(0.75, ind1*0.75, by = 0.75)
      beanPlace <- a[-seq(5, length(a), 5)]
      yearPlace <- .colMeans(beanPlace, 4, length(beanPlace)/4)
      # Re-level strata factor
      TVdens$strata_type <- factor(TVdens$strata_type, 
                                   levels = c("C", "MC", "MF", "F"))
      # "#70543E" "#8B7354" "#A69269" "#C2B280"
      
      if(boxes == TRUE){
        
        # Set up stratum colours
        colList <- rep(list(c("#70543E", "#70543E", "#000000", NA),
                            c("#8B7354", "#8B7354", "#000000", NA),
                            c("#A69269", "#A69269", "#000000", NA),
                            c("#C2B280", "#C2B280", "#000000", NA)), 
                       times = length(unique(TVdens$year)))
        
        # Beanplot with boxplots (no staples)
        beanplot(TVdens$average_density ~  as.factor(TVdens$strata_type) + TVdens$year,
                           col = colList,
                           border = c("#70543E","#8B7354","#A69269","#C2B280"),
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           at = beanPlace,
                           maxwidth = 0.75,
                           axes = FALSE,
                           xlim = c(0,0.75+(ind1*0.75)))
        boxplot(TVdens$average_density ~  as.factor(TVdens$strata_type) + TVdens$year, 
                add = TRUE,
                at = beanPlace,
                boxwex = 0.1,
                col = NA,
                border = "#ffffff",
                axes = FALSE,
                pch = 8, 
                cex = 0.5,
                lwd = 1,
                staplewex = 0,
                lty = 1)
        axis(1, at = yearPlace, labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        if(legend == TRUE){
          legend(median(a), par()$usr[4]*1.2, 
                 bty = "n", 
                 pt.cex = 2, 
                 xjust = 0.5,
                 legend = levels(as.factor(TVdens$strata_type)), 
                 # text.width = 2,
                 pch = 19, 
                 horiz = TRUE, 
                 col = sedimentCol(length(levels(as.factor(TVdens$strata_type)))))
        }
        
      } else {
        
        # Set up stratum colours
        colList <- rep(list(c("#70543E", "#ffffff", "#000000", NA),
                            c("#8B7354", "#ffffff", "#000000", NA),
                            c("#A69269", "#ffffff", "#000000", NA),
                            c("#C2B280", "#ffffff", "#000000", NA)), 
                       times = length(unique(TVdens$year)))
        
        # Regular Beanplot
        beanplot(TVdens$average_density ~  TVdens$strata_type + TVdens$year,
                           col = colList,
                           border = c("#70543E","#8B7354","#A69269","#C2B280"),
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           at = beanPlace,
                           maxwidth = 0.75,
                           axes = FALSE,
                           xlim = c(0,0.75+(ind1*0.75)))
        axis(1, at = yearPlace, labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        if(legend == TRUE){
          legend(median(a), par()$usr[4]*1.2, 
                 bty = "n", 
                 pt.cex = 2, 
                 xjust = 0.5,
                 legend = levels(as.factor(TVdens$strata_type)), 
                 # text.width = 2,
                 pch = 19, 
                 horiz = TRUE, 
                 col = sedimentCol(length(levels(as.factor(TVdens$strata_type)))))
        }
        
      }
      
    } else if(f.u %in% c("firth forth")){
      
      # Set up bean placement
      ind1 <- 2*length(unique(TVdens$year))+(1*length(unique(TVdens$year))-1)
      a <- seq(0.75, ind1*0.75, by = 0.75)
      beanPlace <- a[-seq(3, length(a), 3)]
      yearPlace <- .colMeans(beanPlace, 2, length(beanPlace)/2)
      # Re-level strata factor
      TVdens$strata_type <- factor(TVdens$strata_type, 
                                   levels = c("SANDY MUD", "MUDDY SAND"))
      
      if(boxes == TRUE){
        # Set up stratum colours
        colList <- rep(list(c("#99835F", "#99835F", "#000000", NA),
                            c("#C2B280", "#C2B280", "#000000", NA)), 
                       times = length(unique(TVdens$year)))
        
        # Beanplot with boxplots (no staples)
        beanplot(TVdens$average_density ~  as.factor(TVdens$strata_type) + TVdens$year,
                           col = colList,
                           border = c("#99835F","#C2B280"),
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           at = beanPlace,
                           maxwidth = 0.75,
                           axes = FALSE,
                           xlim = c(0,0.75+(ind1*0.75)))
        boxplot(TVdens$average_density ~  as.factor(TVdens$strata_type) + TVdens$year, 
                add = TRUE,
                at = beanPlace,
                boxwex = 0.1,
                col = NA,
                border = "#ffffff",
                axes = FALSE,
                pch = 8, 
                cex = 0.5,
                lwd = 1,
                staplewex = 0,
                lty = 1)
        axis(1, at = yearPlace, labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        if(legend == TRUE){
          legend(median(a), par()$usr[4]*1.2, 
                 bty = "n", 
                 pt.cex = 2, 
                 xjust = 0.5,
                 legend = sapply(levels(as.factor(TVdens$strata_type)), simpleCap), 
                 # text.width = 2,
                 pch = 19, 
                 horiz = TRUE, 
                 col = c("#99835F","#C2B280"))
        }
        
      } else {
        
        # Set up stratum colours
        colList <- rep(list(c("#99835F", "#ffffff", "#000000", NA),
                            c("#C2B280", "#ffffff", "#000000", NA)), 
                       times = length(unique(TVdens$year)))
        
        # Regular Beanplot
        beanplot(TVdens$average_density ~  TVdens$strata_type + TVdens$year,
                           col = colList,
                           border = c("#99835F","#C2B280"),
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           at = beanPlace,
                           maxwidth = 0.75,
                           axes = FALSE,
                           xlim = c(0,0.75+(ind1*0.75)))
        axis(1, at = yearPlace, labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        if(legend == TRUE){
          legend(median(a), par()$usr[4]*1.2, 
                 bty = "n", 
                 pt.cex = 2, 
                 xjust = 0.5,
                 legend = sapply(levels(as.factor(TVdens$strata_type)), simpleCap), 
                 # text.width = 2,
                 pch = 19, 
                 horiz = TRUE, 
                 col = c("#99835F","#C2B280"))
        }
        
      }
      
    } else if(f.u %in% c("noup")){
      
      if(boxes == TRUE){
        
        # Beanplot with boxplots (no staples)
        beanplot(TVdens$average_density ~ TVdens$year,
                           col = list(c("#C2B280", "#C2B280", "#000000", NA)),
                           border = "#C2B280",
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           at = 1:length(unique(TVdens$year)),
                           maxwidth = 0.75,
                           axes = FALSE)
        boxplot(TVdens$average_density ~ TVdens$year, 
                add = TRUE,
                boxwex = 0.1,
                col = NA,
                border = "#ffffff",
                axes = FALSE,
                pch = 8, 
                cex = 0.5,
                lwd = 1,
                staplewex = 0,
                lty = 1)
        axis(1, at = 1:length(unique(TVdens$year)), labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        if(legend == TRUE){
          legend(median(1:length(unique(TVdens$year))), par()$usr[4]*1.2, 
                 bty = "n", 
                 pt.cex = 2, 
                 xjust = 0.5,
                 legend = "Muddy Sand", 
                 pch = 19, 
                 horiz = TRUE, 
                 col = "#C2B280")
        }
        
      } else {
        
        # Regular Beanplot
        beanplot(TVdens$average_density ~ TVdens$year,
                           col = list(c("#C2B280", "#ffffff", "#000000", NA)),
                           border = "#C2B280",
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           maxwidth = 0.75,
                           axes = FALSE)
        axis(1, at = 1:length(unique(TVdens$year)), labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        if(legend == TRUE){
          legend(median(1:length(unique(TVdens$year))), par()$usr[4]*1.2, 
                 bty = "n", 
                 pt.cex = 2, 
                 xjust = 0.5,
                 legend = "Muddy Sand", 
                 pch = 19, 
                 horiz = TRUE, 
                 col = "#C2B280")
        }
        
      }
      
    } else {
      
      ## Other 6.a FUs, FU9 MF
      # Set up bean placement
      ind1 <- 3*length(unique(TVdens$year))+(1*length(unique(TVdens$year))-1)
      a <- seq(0.75, ind1*0.75, by = 0.75)
      beanPlace <- a[-seq(4, length(a), 4)]
      yearPlace <- a[seq(2, length(a), 4)]
      # Re-level strata factor
      TVdens$strata_type <- factor(TVdens$strata_type, 
                                   levels = c("MUD", "SANDY MUD", "MUDDY SAND"))
      
      if(Jura == TRUE){
        
        survArea <- as.factor(substr(TVdens$station,1,2))
        
        # Set up stratum colours
        colList <- rep(list(c("#70543E", "#ffffff", "#000000", NA),
                            c("#ffffff", "#000000", "#000000", NA),
                            c("#99835F", "#ffffff", "#000000", NA),
                            c("#ffffff", "#000000", "#000000", NA),
                            c("#C2B280", "#ffffff", "#000000", NA),
                            c("#ffffff", "#000000", "#000000", NA)), 
                       times = length(unique(TVdens$year)))
        
        # Regular Beanplot
        beanplot(TVdens$average_density ~  survArea + TVdens$strata_type + TVdens$year,
                           col = colList,
                           border = rep(c("#70543E","#99835F","#C2B280"), each = 2),
                           ll = 0.05, 
                           log = "", 
                           method = "jitter", 
                           side = "both",
                           cutmin = -0.01,
                           what = c(FALSE, TRUE, TRUE, TRUE),
                           at = beanPlace,
                           maxwidth = 0.75,
                           axes = FALSE,
                           xlim = c(0,0.75+(ind1*0.75)))
        axis(1, at = yearPlace, labels = unique(TVdens$year))
        axis(2)
        mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
        box()
        if(legend == TRUE){
          legend(median(a), par()$usr[4]*1.2, 
                 bty = "n", 
                 pt.cex = 2, 
                 xjust = 0.5,
                 legend = sapply(levels(as.factor(TVdens$strata_type)), simpleCap), 
                 # text.width = 2,
                 pch = 19, 
                 horiz = TRUE, 
                 col = sedimentCol(length(levels(as.factor(TVdens$strata_type)))))
        }
        
        
      } else {
        
        if(boxes == TRUE){
          
          # Set up stratum colours
          colList <- rep(list(c("#70543E", "#70543E", "#000000", NA),
                              c("#99835F", "#99835F", "#000000", NA),
                              c("#C2B280", "#C2B280", "#000000", NA)), 
                         times = length(unique(TVdens$year)))
          
          # Beanplot with boxplots (no staples)
          beanplot(TVdens$average_density ~  as.factor(TVdens$strata_type) + TVdens$year,
                             col = colList,
                             border = c("#70543E","#99835F","#C2B280"),
                             ll = 0.05, 
                             log = "", 
                             method = "jitter", 
                             cutmin = -0.01,
                             what = c(FALSE, TRUE, TRUE, TRUE),
                             at = beanPlace,
                             maxwidth = 0.75,
                             axes = FALSE,
                             xlim = c(0,0.75+(ind1*0.75)))
          boxplot(TVdens$average_density ~  as.factor(TVdens$strata_type) + TVdens$year, 
                  add = TRUE,
                  at = beanPlace,
                  boxwex = 0.1,
                  col = NA,
                  border = "#ffffff",
                  axes = FALSE,
                  pch = 8, 
                  cex = 0.5,
                  lwd = 1,
                  staplewex = 0,
                  lty = 1)
          axis(1, at = yearPlace, labels = unique(TVdens$year))
          axis(2)
          mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
          box()
          if(legend == TRUE){
            legend(median(a), par()$usr[4]*1.2, 
                   bty = "n", 
                   pt.cex = 2, 
                   xjust = 0.5,
                   legend = sapply(levels(as.factor(TVdens$strata_type)), simpleCap), 
                   # text.width = 2,
                   pch = 19, 
                   horiz = TRUE, 
                   col = sedimentCol(length(levels(as.factor(TVdens$strata_type)))))
          }
          
        } else {
          
          # Set up stratum colours
          colList <- rep(list(c("#70543E", "#ffffff", "#000000", NA),
                              c("#99835F", "#ffffff", "#000000", NA),
                              c("#C2B280", "#ffffff", "#000000", NA)), 
                         times = length(unique(TVdens$year)))
          
          # Regular Beanplot
          beanplot(TVdens$average_density ~  TVdens$strata_type + TVdens$year,
                             col = colList,
                             border = c("#70543E","#99835F","#C2B280"),
                             ll = 0.05, 
                             log = "", 
                             method = "jitter", 
                             cutmin = -0.01,
                             what = c(FALSE, TRUE, TRUE, TRUE),
                             at = beanPlace,
                             maxwidth = 0.75,
                             axes = FALSE,
                             xlim = c(0,0.75+(ind1*0.75)))
          axis(1, at = yearPlace, labels = unique(TVdens$year))
          axis(2)
          mtext(parse(text = "Burrow~Count~per~m^2"), side = 2, line = 2.2)
          box()
          if(legend == TRUE){
            legend(median(a), par()$usr[4]*1.2, 
                   bty = "n", 
                   pt.cex = 2, 
                   xjust = 0.5,
                   legend = sapply(levels(as.factor(TVdens$strata_type)), simpleCap), 
                   # text.width = 2,
                   pch = 19, 
                   horiz = TRUE, 
                   col = sedimentCol(length(levels(as.factor(TVdens$strata_type)))))
          }
        }
      }
      
    } 
    
  }
  
  dev.off()
  
}
