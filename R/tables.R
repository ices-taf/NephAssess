tables <-
function(wk.dir, stock, effort.data, 
      f.u = c("north minch", "south minch", "clyde", "moray","forth","fladen","noup","devil's hole"))
{

  f.u <- tolower(f.u)
  f.u <- match.arg(f.u)

  ################################################################################################################################
  # 1. Landings(tonnes) by country and gear type                                                                                 #
  ################################################################################################################################
  
  
  year <- stock@range["minyear"]:stock@range["maxyear"]
  otb_cru<- rowSums(colSums(stock@landings["OTB_CRU",,,,] ))
  ott_cru<- rowSums(colSums(stock@landings["OTT_CRU",,,,] ))
  nephrops.trawl <- otb_cru + ott_cru
  other.trawl <- rowSums(colSums(stock@landings["OTHER",,,,] ))
  creel <- rowSums(colSums(stock@landings["FPO",,,,]))
  total <- nephrops.trawl + other.trawl + creel


    if (f.u == "north minch")
    {
      Total.2 <- vector("numeric", length=length(year))
      Other.UK <-  vector("numeric", length=length(year))
	  
	  tmp <- data.frame(cbind(year, nephrops.trawl, other.trawl, creel, total, Other.UK, Total.2))
      colnames(tmp) <- c("Year", "Nephrops Trawl", "Other", "Creel", "Sub Total", "Other UK", "Total")
    } else
    if (f.u == "clyde")
    {

      Total.2 <- vector("numeric", length=length(year))
      Other.UK <-  vector("numeric", length=length(year))

      tmp <- data.frame(cbind(year, nephrops.trawl, other.trawl, creel, total, Other.UK, Total.2))
      colnames(tmp) <- c("Year", "Nephrops Trawl", "Other", "Creel", "Sub Total", "Other UK", "Total")
    } else
    if (f.u == "south minch")
    {
      Total.2 <- vector("numeric", length=length(year))
      Other.UK <-  vector("numeric", length=length(year))
      Ireland <- vector("numeric", length=length(year))

      tmp <- data.frame(cbind(year, nephrops.trawl, other.trawl, creel, total, Other.UK, Ireland, Total.2))
      colnames(tmp) <- c("Year", "Nephrops Trawl", "Other", "Creel", "Sub Total", "Other UK", "Ireland", "Total")
    } else
    if (f.u == "forth")
    {
      Total.2 <-vector("numeric", length=length(year))
      Other.UK <-vector("numeric", length=length(year))
      
      tmp <- data.frame(cbind(year, nephrops.trawl, other.trawl, creel, total, Other.UK, Total.2))
      colnames(tmp) <- c("Year", "Nephrops Trawl", "Other", "Creel", "Sub Total", "Other UK", "Total")
    } else
    if (f.u == "moray")
    {
      Total.2 <-vector("numeric", length=length(year))
      Other.UK <-vector("numeric", length=length(year))
      
      tmp <- data.frame(cbind(year, nephrops.trawl, other.trawl, creel, total, Other.UK, Total.2))
      colnames(tmp) <- c("Year", "Nephrops Trawl", "Other", "Creel", "Sub Total", "Other UK", "Total")
    } else
    if (f.u == "noup")
    {
      Total.2 <-vector("numeric", length=length(year))
      Other.UK <-vector("numeric", length=length(year))
      
      tmp <- data.frame(cbind(year, nephrops.trawl, other.trawl, creel, total, Other.UK, Total.2))
      colnames(tmp) <- c("Year", "Nephrops Trawl", "Other", "Creel", "Sub Total", "Other UK", "Total")
    } else
    if (f.u == "fladen")
    {
      Total.2 <-vector("numeric", length=length(year))
      Other.else <-vector("numeric", length= length(year))
      
      tmp <- data.frame(cbind(year, nephrops.trawl, other.trawl, creel, total, Other.else, Total.2)) 
      colnames(tmp) <- c("Year", "Nephrops Trawl", "Other", "Creel", "Sub Total", "Other Countries", "Total")
    } else
    if (f.u == "devil's hole")
    {
      Total.2 <-vector("numeric", length=length(year))
      Other.else <-vector("numeric", length= length(year))
      
      tmp <- data.frame(cbind(year, nephrops.trawl, other.trawl, creel, total, Other.else, Total.2)) 
      colnames(tmp) <- c("Year", "Nephrops Trawl", "Other", "Creel", "Sub Total", "Other Countries", "Total")
    }
       
  write.table(tmp, paste(wk.dir, "Landings.csv", sep = ""), row.names=FALSE, sep =",")


  ################################################################################################################################
  # 2. Landings (tonnes), effort (‘000 hours trawling) and LPUE (kg/hour trawling) of Scottish Nephrops trawlers, 1981-2008      #
  #    data for all Nephrops gears combined, and for single and multirigs separately).                                           #
  ################################################################################################################################
  
  years.land<- stock@range["minyear"]:stock@range["maxyear"]
  years.eff<- as.numeric(dimnames(effort.data[,,,,,])$year)
  years.match<- years.land[match(years.eff, years.land)]
    
  # all nephrops trawl gears combined
  effort.nephrops.trawl <- rowSums(colSums(effort.data[c("OTB_CRU", "OTT_CRU"),,,,,]))
  LPUE.nephrops.trawl <- round(1000*nephrops.trawl[names(nephrops.trawl) %in% years.match] / effort.nephrops.trawl, 1)	#units:  kg/days fishing

  # Single rig
  single.rig.e <- rowSums(colSums(effort.data[c("OTB_CRU"),,,,,]))
  LPUE.single <- round(1000*otb_cru[names(otb_cru) %in% years.match] / single.rig.e, 1)	#units:  kg/days fishing

  # multi rig
  multi.rig.e <- rowSums(colSums(effort.data[c("OTT_CRU"),,,,,]))
  LPUE.multi <- round(1000*ott_cru[names(ott_cru) %in% years.match] / multi.rig.e, 1)	#units:  kg/days fishing

  tmp.2 <- data.frame(cbind(years.match, nephrops.trawl[names(nephrops.trawl) %in% years.match], effort.nephrops.trawl, LPUE.nephrops.trawl), 
				otb_cru[names(otb_cru) %in% years.match], single.rig.e, LPUE.single,
				ott_cru[names(ott_cru) %in% years.match], multi.rig.e, LPUE.multi)

  colnames(tmp.2) <-   c("Year", "Landings_Nep.trawl", "Effort_Nep.trawl", "LPUE_Nep.trawl", 
								"Landings_OTB_CRU", "Effort_OTB_CRU", "LPUE_OTB_CRU", 
								"Landings_OTT_CRU", "Effort_OTT_CRU", "LPUE_OTT_CRU")

  write.table(tmp.2, paste(wk.dir, "By gear.csv", sep = ""), row.names=FALSE, sep = ",")


  ################################################################################################################################
  #### 3. Mean sizes (CL mm) above and below 35 mm of male and female Nephrops in Scottish catches and landings, 1981-2008  ######
  ################################################################################################################################
  means <- mean.sizes(stock)

  means <- lapply(means, round, 1)
  means $ year <- year
  means <- means[c("year", "means.male.low.c", "means.female.low.c", "means.male.low", "means.female.low", "means.male.high", "means.female.high")]
  names(means) <- c("year", "males", "females", "males.", "females", "males", "females")
  means <- data.frame(means)

  tmp.3 <- means


  write.table(tmp.3, paste(wk.dir, "Mean sizes.csv", sep = ""), row.names=FALSE, sep = ",")

}

