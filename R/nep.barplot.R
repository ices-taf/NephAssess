`nep.barplot` <-
function(height, line, main, ylab, legend.names)
{
  NR <- nrow(height)
  NC <- ncol(height)
  space <- rep.int(c(1, rep.int(0, NR - 1)), NC)

  # set up bar locations: right, left, middle
  w.r <- cumsum(space + 1)
  w.m <- w.r - 0.5
  w.l <- w.m - 0.5

  opar <- par(yaxs = "i", xpd = TRUE)
  on.exit(par(opar))
  
  plot.new()
  xlim <- c(min(w.l), max(w.r))
  ylim <- c(0, max(height, line, na.rm = TRUE))
  plot.window(xlim, ylim)

  rect(w.l, 0, w.r, c(height), col = grey.colors(NR), border = par("fg"))

  w.m <- colMeans( matrix(w.m, ncol = NC) )
  lines(w.m, line, col = "red", lwd = 2)
  points(w.m, line, col = "red", pch = 16)

  axis(1, at = w.m, labels = colnames(height),
       lwd = 0, cex.axis = 0.9, mgp = c(0.8, 0.4, 0))
  suppressWarnings(
    axis(2, at = pretty(ylim), labels = pretty(ylim),
         cex.axis = 0.9, mgp = c(1,0.7,-0.5), tck = -0.02, las = 1)
  )
  
  title(main = main, ylab = ylab, mgp = c(2.5,0,0))

  legend.names[NR + 1] <- paste("  ", legend.names[NR + 1], sep="") # fix to move avay from line
  
  nep.legend(x = mean(w.m), y = 0.15 * diff(ylim) + max(ylim), xjust = 0.5,
             legend = legend.names, fill = c(grey.colors(NR),NA),
             col = c(rep(NA, NR), "red"), lty = c(rep(NA, NR), 1),
             lwd = c(rep(NA, NR), 2), bty = "n", ncol = NR+1, cex = 0.8,
             x.intersp = 0.1, xpd = NA, border = c(rep("black",NR), "white"))

  invisible ( NULL )
}

