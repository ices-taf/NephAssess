`discards.ldist.plot` <-
function(flneph.object, years, extra.space){

op <- par(no.readonly = TRUE)
on.exit(op)

par(mfrow=c(1,2))

if (missing(extra.space)){
extra.space <- 2
}

if (missing(years)){
start.year <- as.numeric(colnames(flneph.object@discards.n)[1])
end.year   <- as.numeric(colnames(flneph.object@discards.n)[dim(flneph.object@discards.n)[2]])
}
else{
start.year <- years[1]
end.year   <- years[2]
}
    
object.start.year <- as.numeric(colnames(flneph.object@discards.n)[1])

min.length <- as.numeric(rownames(flneph.object@discards.n)[1])
max.length <- as.numeric(rownames(flneph.object@discards.n)[dim(flneph.object@discards.n)[1]])

plot((flneph.object@discards.n[,3,1,1,1]+flneph.object@discards.n[,3,1,2,1]+flneph.object@discards.n[,3,1,3,1]+flneph.object@discards.n[,3,1,4,1])/80000~seq(min.length,max.length, by=2), xlim=c(10,40),
 type="l", lty=0 ,ylim=c(start.year, end.year+extra.space), ylab="Year", xlab="Length (mm)", main = "Males" )
year.range <-c(start.year:end.year)

for(i in (1:(end.year-start.year+1))){

lines(year.range[i]+(flneph.object@discards.n[,i,1,1,1]+flneph.object@discards.n[,i,1,2,1]+flneph.object@discards.n[,i,1,3,1]+flneph.object@discards.n[,i,1,4,1])/
max(c(flneph.object@discards.n[,i,1,4,1],flneph.object@discards.n[,i,1,3,1],flneph.object@discards.n[,i,1,2,1],flneph.object@discards.n[,i,1,1,1]),na.rm=T)~seq(min.length,max.length, by=2))

}

mean.l.vector<-vector(length=0)

for (i in ((start.year-object.start.year+1):(end.year-object.start.year+1))){

temp.mat <- (flneph.object@discards.n[,i,1,1,1]+flneph.object@discards.n[,i,1,2,1]+flneph.object@discards.n[,i,1,3,1]+flneph.object@discards.n[,i,1,4,1])
temp.vect <-vector(length=0)

for(j in (1:dim(temp.mat)[1])){

    rep.lengths <- rep(as.numeric(rownames(temp.mat)[j]), temp.mat[j])
    temp.vect <-c(temp.vect, rep.lengths)
    }
    
mean.l.vector <- c(mean.l.vector, mean(temp.vect, na.rm=T))    

}

lines(x=mean.l.vector, y=c(start.year:end.year), col=2)


plot((flneph.object@discards.n[,3,1,1,1]+flneph.object@discards.n[,3,1,2,1]+flneph.object@discards.n[,3,1,3,1]+flneph.object@discards.n[,3,1,4,1])/80000~seq(min.length,max.length, by=2), xlim=c(10,40),
 type="l", lty=0 ,ylim=c(start.year, end.year+extra.space), ylab="", xlab="Length (mm)", main = "Females" )
year.range <-c(start.year:end.year)

for(i in (1:(end.year-start.year+1))){

lines(year.range[i]+(flneph.object@discards.n[,i,2,1,1]+flneph.object@discards.n[,i,2,2,1]+flneph.object@discards.n[,i,2,3,1]+flneph.object@discards.n[,i,2,4,1])/
max(c(flneph.object@discards.n[,i,2,4,1],flneph.object@discards.n[,i,2,3,1],flneph.object@discards.n[,i,2,2,1],flneph.object@discards.n[,i,2,1,1]),na.rm=T)~seq(min.length,max.length, by=2))

}

mean.l.vector<-vector(length=0)

for (i in ((start.year-object.start.year+1):(end.year-object.start.year+1))){

temp.mat <- (flneph.object@discards.n[,i,2,1,1]+flneph.object@discards.n[,i,2,2,1]+flneph.object@discards.n[,i,2,3,1]+flneph.object@discards.n[,i,2,4,1])
temp.vect <-vector(length=0)

for(j in (1:dim(temp.mat)[1])){

    rep.lengths <- rep(as.numeric(rownames(temp.mat)[j]), temp.mat[j])
    temp.vect <-c(temp.vect, rep.lengths)
    }
    
mean.l.vector <- c(mean.l.vector, mean(temp.vect, na.rm=T))    

}

lines(x=mean.l.vector, y=c(start.year:end.year), col=2)

}

