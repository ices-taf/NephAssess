mean.wt.disc<- function (wk.dir, stock.list) 
{
    miny <- max(sapply(stock.list, function(x) x@range["minyear"]))
    maxy <- max(sapply(stock.list, function(x) x@range["maxyear"]))
    years <- miny:maxy
    new.list <- lapply(stock.list, trim, year = years)
    wt.in.discards <- lapply(new.list, function(x) {
        tot.wt <- seasonSums(quantSums(unitSums(x@discards.n * 
            x@discards.wt)))
        tot.num <- seasonSums(quantSums(unitSums(x@discards.n)))
        return(round(1000 * tot.wt/tot.num, 2))
    })
    tmp <- cbind(years, data.frame(matrix(unlist(wt.in.discards), 
        length(years), length(new.list))))
    colnames(tmp) <- c("Year", lapply(new.list, name))
    write.table(tmp, paste(wk.dir, "Mean weights in discards.csv", 
        sep = ""), row.names = FALSE, sep = ",")
    return(tmp)
    print("weights saved in file:")
    cat("\n")
    print(paste(Wkdir, "output/Mean weights in discards.csv", 
        sep = ""))
    cat("\n")
}

mean.wt.disc2<- function (stock.list) 
{
  miny <- max(sapply(stock.list, function(x) x@range["minyear"]))
  maxy <- max(sapply(stock.list, function(x) x@range["maxyear"]))
  years <- miny:maxy
  new.list <- lapply(stock.list, trim, year = years)
  wt.in.discards <- lapply(new.list, function(x) {
    tot.wt <- seasonSums(quantSums(unitSums(x@discards.n * 
                                              x@discards.wt)))
    tot.num <- seasonSums(quantSums(unitSums(x@discards.n)))
    return(round(1000 * tot.wt/tot.num, 2))
  })
  tmp <- cbind(years, data.frame(matrix(unlist(wt.in.discards), 
                                        length(years), length(new.list))))
  colnames(tmp) <- c("Year", lapply(new.list, name))

  return(tmp)
}