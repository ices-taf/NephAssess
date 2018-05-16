mean.weight <-

function(wk.dir, stock.list) 
{

# finds the common year range for the FUs in the list   
    miny <-max(sapply(stock.list,function(x) x@range["minyear"]))
    maxy <-max(sapply(stock.list,function(x) x@range["maxyear"]))    
    years <- miny:maxy
 
#  trims each stock object so that the year range is the same
    
    new.list <-lapply(stock.list,trim,year=years)
    
    wt.in.landings <-lapply(new.list,
                    function(x)
                    {
                        tot.wt <-seasonSums(quantSums(unitSums(x@landings.n*x@landings.wt)))
                        tot.num <-seasonSums(quantSums(unitSums(x@landings.n)))
                        return(round(1000*tot.wt/tot.num,2))
                    })
                    
    
    tmp <-cbind(years,data.frame(matrix(unlist(wt.in.landings),length(years),length(new.list))))
    colnames(tmp) <-c("Year",lapply(new.list,name))

    write.table(tmp, paste(wk.dir, "Mean weights in landings.csv", sep = ""), row.names=FALSE, sep =",")
	return(tmp)
	print("weights saved in file:")       
	cat("\n")
	print(paste(Wkdir, "output/Mean weights in landings.csv", sep=""))
	cat("\n")
}



mean.weight2 <-

function(stock.list) 
{

# finds the common year range for the FUs in the list   
    miny <-max(sapply(stock.list,function(x) x@range["minyear"]))
    maxy <-max(sapply(stock.list,function(x) x@range["maxyear"]))    
    years <- miny:maxy
 
#  trims each stock object so that the year range is the same
    
    new.list <-lapply(stock.list,trim,year=years)
    
    wt.in.landings <-lapply(new.list,
                    function(x)
                    {
                        tot.wt <-seasonSums(quantSums(unitSums(x@landings.n*x@landings.wt)))
                        tot.num <-seasonSums(quantSums(unitSums(x@landings.n)))
                        return(round(1000*tot.wt/tot.num,2))
                    })
                    
    
    tmp <-cbind(years,data.frame(matrix(unlist(wt.in.landings),length(years),length(new.list))))
    colnames(tmp) <-c("Year",lapply(new.list,name))
	return(tmp)
}
