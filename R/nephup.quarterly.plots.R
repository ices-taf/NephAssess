`nephup.quarterly.plots` <-
function (wk.dir, stock, effort.data)
{

  landings.q <- colSums(stock@landings[c("OTB_CRU", "OTT_CRU", "OTHER", "FPO"),,,,])
  landings.q.plot <- t(cbind(landings.q[,,1,,], landings.q[,,2,,], landings.q[,,3,,], landings.q[,,4,,]))
  landings.y.plot<- colSums(landings.q.plot)

  tmp <- colSums(stock@landings.n * stock@landings.wt)

  total.lands.q <- apply(tmp, c(1,3), sum)
  mal.ratio.q <- ifelse(total.lands.q==0, NA, tmp[,1,,,]/total.lands.q)
  fem.ratio.q <- 1-mal.ratio.q

  mal.lands.tot <- rowSums(tmp[,1,,,])
  fem.lands.tot <- rowSums(tmp[,2,,,])

  total.lands <- mal.lands.tot + fem.lands.tot    # these are numbers

  ratio.mal.tot <- ifelse(total.lands==0, NA, mal.lands.tot/total.lands)
  ratio.fem.tot <- 1-ratio.mal.tot


  NT.total.lands <- rowSums(colSums(stock@landings[c("OTB_CRU", "OTT_CRU", "OTHER", "FPO"),,,,,]))      # in weight
  NT.mal.lands <- NT.total.lands * ratio.mal.tot
  NT.fem.lands <- NT.total.lands * ratio.fem.tot
  plot.lands <- t(cbind(NT.mal.lands, NT.fem.lands))
  
  years.land<- stock@range["minyear"]:stock@range["maxyear"]
  years.eff<- as.numeric(dimnames(effort.data[,,,,,])$year)
  years.match<- years.land[match(years.eff, years.land)]
  
  # 2. EFFORT

  effort.y <- colSums(effort.data)
  effort.y.plot <- t(cbind(effort.y[,,1,,]))
  total.effort <- colSums(effort.y.plot)

  

  #!                                                     !#
  #
  #         PLOTS
  #!                                                     !#

  get.fname <-
  function (base.name)
  {
    fname <- function(i) paste(base.name, i, ".png", sep="")
    out <- fname(i <- 1)
    while (file.exists(out))
        out <- fname(i <- i + 1)

    return ( out )                                                           # function so you can't overwrite files
  }


  png(get.fname(paste(wk.dir, "quarterly landings", sep = "")), width = 2100, height = 2200, pointsize = 50)
  
  par(mfrow=c(2,1))

  # 1. LANDINGS Males/Females

  nep.barplot(plot.lands, line = NT.total.lands / 2,
              main = "Landings", ylab = "Landings (tonnes)",
              legend.names = c("Males", "Females", "Mean"))

  # 2. Quarterly Landings

  nep.barplot(landings.q.plot, line = landings.y.plot/4,
              main = "Quarterly Landings", ylab = "Quarterly Landings (tonnes)",
              legend.names = c("Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4", "Mean"))

 
  dev.off()

}

