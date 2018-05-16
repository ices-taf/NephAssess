`nephup.quarterly.plots.no.lpue` <-
function (wk.dir, stock, effort.data)
{

  tmp <- colSums(stock@landings.n * stock@landings.wt)

  total.lands.q <- apply(tmp, c(1,3), sum)
  mal.ratio.q <- ifelse(total.lands.q==0, NA, tmp[,1,,,]/total.lands.q)
  fem.ratio.q <- 1-mal.ratio.q

  mal.lands.tot <- rowSums(tmp[,1,,,])
  fem.lands.tot <- rowSums(tmp[,2,,,])

  total.lands <- mal.lands.tot + fem.lands.tot    # these are numbers

  ratio.mal.tot <- ifelse(total.lands==0, NA, mal.lands.tot/total.lands)
  ratio.fem.tot <- 1-ratio.mal.tot


  NT.total.lands <- rowSums(colSums(stock@landings[2:3,,,,,]))      # in weight
  NT.mal.lands <- NT.total.lands * ratio.mal.tot
  NT.fem.lands <- NT.total.lands * ratio.fem.tot
  plot.lands <- t(cbind(NT.mal.lands, NT.fem.lands))

  # 2. EFFORT

  effort.q <- colSums(effort.data)
  effort.q.plot <- t(cbind(effort.q[,,1,,], effort.q[,,2,,], effort.q[,,3,,], effort.q[,,4,,]))
  total.effort <- colSums(effort.q.plot)

  # 3. LPUE - MALES

  landings.q <- colSums(stock@landings[2:3,,,,])
  landings.q.plot <- t(cbind(landings.q[,,1,,], landings.q[,,2,,], landings.q[,,3,,], landings.q[,,4,,]))
  LPUE.total.q <- landings.q.plot*1000/effort.q.plot
  LPUE.m <-  LPUE.total.q  * t(mal.ratio.q)

  total.m <-  (NT.mal.lands *1000)/colSums(effort.q.plot)   # multiply by 1000 to get weight in KG from tonnes
  landings.q.plot.m <- landings.q.plot *  t(mal.ratio.q)

  LPUE.fem <- LPUE.total.q * t(fem.ratio.q)

  total.fem <-  (NT.fem.lands *1000)/colSums(effort.q.plot)
  total.landings <- rowSums(landings.q.plot)





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


  png(get.fname(paste(wk.dir, "quarterly landings", sep = "")), width = 2100, height = 3000, pointsize = 50)
  
  par(mfrow=c(4,1))

  # 1. LANDINGS

  nep.barplot(plot.lands, line = NT.total.lands / 2,
              main = "Landings", ylab = "Landings (tonnes)",
              legend.names = c("Males", "Females", "Mean"))

  # 2. EFFORT

  nep.barplot(effort.q.plot, line = total.effort / 4,
              main = "Effort", ylab = "Quarterly effort (days)",
              legend.names = c("Qtr 1", "Qtr 2", "Qtr 3", "Qtr 4", "Mean"))

  dev.off()

}

