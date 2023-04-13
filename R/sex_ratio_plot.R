sex_ratio_plot <- function(wdir, stock.obj, print.output = FALSE, type = "year")
{

  if(type == "year")
  {
    dat <- seasonSums(quantSums(stock.obj@catch.n))[,,"Male",,,]/seasonSums(unitSums(quantSums(stock.obj@catch.n)))
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$year),]
    png(filename=paste0(wdir, substr(stock.obj@name,1,2), "_sex_ratio_", type, ".png"), 
        width = 210, height = 100, bg = "white", res = 300, units="mm")
    par(mar = c(5.1, 4.1, 2.1, 0.5))
    
    bdat <- rbind(dat$data, 1-dat$data)
    barPoints <- barplot(bdat,
                         col = c("#999999", "#E69F00"),
                         xlab = "Year",
                         ylab = "Sex Ratio",
                         axes = FALSE,
                         space = 0)
    abline(h = 0.5, col = "#ffffff", lty = 2)
    axis(1, at = barPoints, labels = dat$year)
    axis(2)
    par(xpd = TRUE)
    legend(barPoints[round(length(barPoints)/2)], 1.15, 
           legend = c("Female","Male"),
           horiz = TRUE,
           bty = "n",
           fill = c("#E69F00", "#999999"), 
           cex = 1,
           xjust = 0.5)
  }
  
  if(type == "quarter")
  {
    nephShort <- FLCore::trim(stock.obj, year = (as.numeric(stock.obj@range["maxyear"])-14):as.numeric(stock.obj@range["maxyear"]))
    dat <- quantSums(nephShort@catch.n)[,,"Male",,,]/unitSums(quantSums(nephShort@catch.n))
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$year, dat$season),]
    dat$year_quarter <- dat$year + rep(seq(0,0.75,by=0.25), length(unique(dat$year)))
    
    # Create polygon lists
    # X variable
    xvar <- seq(0.5, 1.5, by = 0.25)
    xvar <- c(rep(xvar, each = 2)[-1], xvar[1])
    xvar <- rep(xvar, times = length(unique(dat$year)))
    xvar <- rep(0:(length(unique(dat$year))-1), each=10)+xvar
    lxvar <- split(xvar, rep(1:15, each=10))
    
    # Female
    ldatF <- split(dat$data, rep(1:length(unique(dat$year)), each = 4))
    ldatF <- lapply(ldatF, rep, each = 2)
    ldatF <- lapply(ldatF, function(x){x <- c(x, 1, 1)})
    ldatF <- Map(cbind, lxvar, ldatF)
    
    # Male
    ldatM <- split(dat$data, rep(1:length(unique(dat$year)), each = 4))
    ldatM <- lapply(ldatM, rep, each=2)
    ldatM <- lapply(ldatM, function(x){x <- c(x, 0, 0)})
    ldatM <- Map(cbind, lxvar, ldatM)
    
    png(filename = paste0(wdir, substr(stock.obj@name,1,2), "_sex_ratio_", type, ".png"), 
        width = 210, height = 100, bg = "white", res = 300, units = "mm")

    par(mar = c(5.1, 4.1, 2.1, 0.5))
    plot(1,1,
         xlim = c(0.5,15.5),
         ylim = c(0,1),
         type = "n",
         xlab = "Year",
         ylab = "Sex Ratio by Quarter",
         axes = FALSE)
    lapply(ldatF, polygon, col = "#E69F00")
    lapply(ldatM, polygon, col = "#999999")
    abline(h = 0.5, col = "#ffffff", lty = 2)
    axis(1, at = 1:15, labels = unique(dat$year))
    axis(2)
    box()
    par(xpd = TRUE)
    legend(8, 1.2, 
           legend = c("Female","Male"),
           horiz = TRUE,
           bty = "n",
           fill = c("#E69F00", "#999999"), 
           cex = 1,
           xjust = 0.5)
  }
  
  dev.off()
  
  if(print.output == T)
  {
    return(dat)
  }
}