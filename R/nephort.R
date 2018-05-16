`nephort` <-
function(wdir, eff.file){

working.directory<-wdir
effort.file <- eff.file

#require(FLCore, quietly=T)


paste(strsplit(readLines(paste(working.directory, effort.file, sep=""))[1], "\t")[[1]], sep=" ")->area.name
stock<-paste(area.name[1])
area.name<-area.name[area.name!=""]


for(i in (2:length(area.name))){
stock<-paste(stock, area.name[i], sep="_")
}

## creates a text string of the area name

effort.data<-read.table(paste(working.directory, effort.file, sep=""), skip=3, header=F)
colnames(effort.data)<-c("year", "OTB_CRU", "OTT_CRU", "OTB_DEF", "OTT_DEF")

effort.start.year <- as.numeric(strsplit(readLines(paste(working.directory, effort.file, sep=""))[2], split="\t")[[1]][1])
effort.end.year   <- as.numeric(strsplit(readLines(paste(working.directory, effort.file, sep=""))[2], split="\t")[[1]][2])


neph.effort <- FLQuant(dimnames=list(gear=c("OTB_CRU", "OTT_CRU", "OTB_DEF", "OTT_DEF"), 
year=as.character(c(effort.start.year:effort.end.year )), area =stock))

neph.effort@units<-"Days"

gears<- dimnames(neph.effort)$gear
years<- unique(effort.data$year)

for(i in (1:length(gears)))
{
	for(j in (1:length(years)))
	{
		neph.effort[gears[i],as.character(years[j]),,,] <- effort.data[effort.data$year==years[j],gears[i]]
	}
}

return(neph.effort)

}

