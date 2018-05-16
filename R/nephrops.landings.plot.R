`nephrops.landings.plot` <-
function (flneph.object, years, legend.type){


temp  <-  flneph.object@landings[,,,1,]+flneph.object@landings[,,,2,]+
                flneph.object@landings[,,,3,]+flneph.object@landings[,,,4,]

if(missing(years)){
start.year <-  as.numeric(colnames(temp)[1])
end.year   <-  as.numeric(colnames(temp)[length(colnames(temp))])
}

else {
start.year <- years[1]
end.year   <- years[2]
}

data.start.year <- as.numeric(colnames(temp)[1])

temp[temp < 0.1] <- NA

plot(as.vector(temp[1,c((start.year-data.start.year+1):(end.year-data.start.year+1)),,,])/1000~c(start.year:end.year), 
type="l", bty="L", lwd=2, xlab="Year", ylab="Landings ('000 tonnes)", ylim=c(0, 1.2*(max(c(as.vector(temp[1,c((start.year-start.year+1):
(end.year-start.year+1)),,,]), as.vector(temp[2,c((start.year-start.year+1):(end.year-start.year+1)),,,])), na.rm=T)/1000)))
points(as.vector(temp[1,c((start.year-data.start.year+1):(end.year-data.start.year+1)),,,])/1000~c(start.year:end.year), pch=19)


lines(as.vector(temp[2,c((start.year-data.start.year+1):(end.year-data.start.year+1)),,,])/1000~c(start.year:end.year), lty=2, lwd=2)
points(as.vector(temp[2,c((start.year-data.start.year+1):(end.year-data.start.year+1)),,,])/1000~c(start.year:end.year), pch=21, bg="white")

if(legend.type=="none"){
break
}

if(missing(legend.type)){
leg.pos <- start.year
}

if(legend.type=="left"){
leg.pos<-start.year
}

if(legend.type=="right"){
leg.pos <- end.year-4
}

legend(x=leg.pos, y=1.2*(max(c(as.vector(temp[1,c((start.year-start.year+1):
(end.year-start.year+1)),,,]), as.vector(temp[2,c((start.year-start.year+1):(end.year-start.year+1)),,,])), na.rm=T)/1000), legend=rownames(temp),
lty=c(1,2,3,1), lwd=c(2,2,2,1), pch=c(19,21,24,22), pt.bg="white", bty="n", cex=0.8)

}

