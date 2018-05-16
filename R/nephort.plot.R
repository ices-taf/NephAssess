`nephort.plot` <-
function(nephort.object){

annual.effort <- nephort.object[,,,1,]+nephort.object[,,,2,]+nephort.object[,,,3,]+nephort.object[,,,4,]
 
annual.effort[annual.effort<0.1] <-NA

start.year <-  as.numeric(colnames(annual.effort)[1])
end.year   <-  as.numeric(colnames(annual.effort)[length(colnames(annual.effort))])

data.start.year <- as.numeric(colnames(annual.effort)[1])

plot(as.vector(annual.effort[1,c((start.year-data.start.year+1):(end.year-data.start.year+1)),,,])/1000~c(start.year:end.year), 
type="l", lwd=2, xlab="Year", ylab="Hours Trawled ('000)", ylim=c(0, 1.2*(max(c(as.vector(annual.effort[1,c((start.year-start.year+1):
(end.year-start.year+1)),,,]), as.vector(annual.effort[2,c((start.year-start.year+1):(end.year-start.year+1)),,,])), na.rm=T)/1000)))
legend

points(as.vector(annual.effort[1,c((start.year-data.start.year+1):(end.year-data.start.year+1)),,,])/1000~c(start.year:end.year), pch=3)


lines(as.vector(annual.effort[2,c((start.year-data.start.year+1):(end.year-data.start.year+1)),,,])/1000~c(start.year:end.year), lty=2, lwd=2)

points(as.vector(annual.effort[2,c((start.year-data.start.year+1):(end.year-data.start.year+1)),,,])/1000~c(start.year:end.year), pch=4)

}

