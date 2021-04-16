create.international.landings<- function(wdir, fu, new.data.year, old.int.landings, new.scotland.landings, IC_file)
{
  int<- read.csv(old.int.landings)
  scot<- read.csv(new.scotland.landings)
  ic<- read.table(IC_file, sep="\t", head=T)
  ic.landings<- ic[ic$Catch.Cat.=="Landings",]
  
  if (!fu==unique(ic$Stock)) { stop("FU in Intercatch file does not match specified FU") }
  if (!new.data.year==unique(ic$Year)) { stop("Year in Intercatch file does not match new.data.year") }
  
  official.Scotland.new.year<- scot[scot$Year==new.data.year,"Sub.Total"]
  land.by.country<- as.data.frame((as.table(round(tapply(ic.landings$Catch..kg, list(ic.landings$Country), sum)/1000))))

  names(land.by.country)<- c("Country", "LANDINGS_IC")
  land.by.country$LANDINGS_OFFICIAL<- land.by.country$LANDINGS
  land.by.country[land.by.country$Country %in% "UK(Scotland)","LANDINGS_OFFICIAL"]<- official.Scotland.new.year
  write.csv(land.by.country, file=paste0(wdir,"landings.by.country.csv"), row.names = FALSE)
  
  common.years<- intersect(int$Year,scot$Year)
  int[int$Year %in% common.years, "Scotland"]<- scot[scot$Year %in% common.years, "Sub.Total"]
  int$Total<- int$Scotland + int$Other.Countries
  int[nrow(int)+1,] <- c(new.data.year,land.by.country[land.by.country$Country %in% "UK(Scotland)", "LANDINGS_OFFICIAL"], sum(land.by.country[!land.by.country$Country %in% "UK(Scotland)", "LANDINGS_OFFICIAL"]), sum(land.by.country$LANDINGS_OFFICIAL))
  write.csv(int, file=paste0(wdir,"international.landings.csv"), row.names = FALSE)
}
