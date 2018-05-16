#this function takes a dataframe which must contain the following columns (order is not important, names are)
#Year, Sex, Length, Landings, Discards, Catch
#function also takes a name for the graph title,  minimum and maximum years.
#by default the function bins length classes into 2mm slots - if you want anything different then change the line bin<-2 to something else!
plot.ld <- function(df, area, miny, maxy, mls, reference){
par(lwd=2)
bin <- 2
df$length2 <- df$Length+(ifelse(floor(df$Length/bin)-df$Length/bin==0,0,-1))

df2<-tapply.ID(df, "Landings", c("Year", "Sex", "length2"), "sum", "Landings")

df2$Discards<-tapply.ID(df, "Discards", c("Year", "Sex", "length2"), "sum", "temp")[,"temp"]
df2$Catch<-tapply.ID(df, "Catch", c("Year", "Sex", "length2"), "sum", "temp")[,"temp"]

df2$Length <- df2$length2+1
df<-df2
tab <- with(df, tapply(Catch, list(Year, Sex), max))
vec <- vectorise.names(tab, c("max", "Year", "Sex"))

ld2 <- merge(df, vec)
ld2$scaled.catch <- ld2$Catch/ld2$max
ld2$scaled.landing <- ld2$Landings/ld2$max
ld2$time.catch <- ld2$Year+(.9999*ld2$scaled.catch)
ld2$time.landing <- ld2$Year+(.9999*ld2$scaled.landing)


#stack the catch and landings together, to use cbind the columns need to be the same name, so make a temp data frame and rename time.landing time.catch
ld2 <- ld2[ld2$scaled.catch>0 & ld2$Year>=miny & ld2$Year<=maxy,]
ld2 <- sort.data.frame(~ Year+ Sex+ Length, ld2)
length(ld2$Year)
temp <- as.data.frame(ld2$Year)
names(temp)[1] <- "Year"
temp$Sex <- ld2$Sex
temp$Length <- ld2$Length
temp$time.catch <- ld2$time.landing
temp$type <- rep("land", length(temp$Year))

ld2$type <- rep("catch", length(ld2$Year))
ld3 <- rbind(ld2[,c("Year", "Sex", "Length","type","time.catch")],temp)
ld3 <- sort.data.frame(~Sex +type + Year+ Length, ld3)
length(ld2[,1])
length(ld3[,1])

#set the colours to be black and white
t <- trellis.par.get()$superpose.line
t$col <- rep("black", length(unique(ld3$Year)))
trellis.par.set("superpose.line",t)

###############################################
#here's the plot
plot1 <- with(ld3, xyplot( time.catch~Length|Sex, groups=Year, panel=function(x,y, subscripts,groups,...){
#panel.grid(h=-1, v=-1)
len <- length(x)
#print(len)
#print(x,y)
len2 <- len/2
#panel.superpose(x, y, subscripts, groups, type="l", lty=1)
panel.superpose(x[1:len2], y[1:len2], subscripts, groups, type="l", lty=2, lwd=1.8)
panel.superpose(x[len2+1:len], y[len2+1:len],subscripts, groups, type="l", lty=1, lwd=1.8)
if(reference>0) panel.abline(v=reference, lty=4)
panel.abline(v=mls, lty=3)

#create a df with the input data
df <- as.data.frame(y[len2+1:len])
names(df)[1] <- "freq"
df$len <- x[len2+1:len]
df$year <- floor(df$freq)
df$freq2 <- df$freq-df$year
df$lenfreq <- df$freq2*df$len
#print(df)

#plot the mean for the landings
m <-tapply.ID(df, c("lenfreq","freq2"), "year", "sum", c("meanlen", "totfreq"))
m$Mean.Land.Length <- m$meanlen/m$totfreq
m$year <- as.numeric(m$year)+0.5
panel.lines(m$Mean.Land.Length, m$year, type="l", lty=1, lwd=2, col="black")
#print(m[,c("year", "Mean.Land.Length")])


#now do the catches - all lengths
df <- as.data.frame(y[1:len2])
names(df)[1] <- "freq"
df$len <- x[1:len2]
df$year <- floor(df$freq)
df$freq2 <- df$freq-df$year
df$lenfreq <- df$freq2*df$len

#plot the mean for the catches
m2 <-tapply.ID(df, c("lenfreq","freq2"), "year", "sum", c("meanlen", "totfreq"))
m2$Mean.Catch.Length <- m2$meanlen/m2$totfreq
m2$year <- as.numeric(m2$year)+0.5
panel.lines(m2$Mean.Catch.Length, m2$year, type="l", lty=2, lwd=2, col="black")
m3 <- merge(m[,c("year", "Mean.Land.Length")], m2[,c("year", "Mean.Catch.Length")])
print(m3)


#tab <- with(df,tapply(lenfreq, year, sum))
#meanvec <- vectorise.names(tab, c("meanlen", "year"))
#tab <- with(df,tapply(freq2, year, sum))#
#vec<- vectorise.names(tab, c("totfreq", "year"))
#m <- merge(meanvec, vec)
#m$meanval <- m$meanlen/m$totfreq
#m$year <- as.numeric(m$year)+0.5
#panel.lines(m$meanval, m$year, type="l", lty=2, col="red",lwd=2 )


},main=paste("Length frequencies for catch (dotted) and landed(solid):\n Nephrops in ", area, sep=""),
                          sub=(ifelse(reference>0, paste("Mean length of landings and catch vertically \n MLS (", mls, "mm) and ", reference, "mm levels displayed", sep=""),paste("Mean length of landings and catch vertically \n MLS (", mls, "mm) indicated", sep="")))
                          , xlab="length",ylab="Year", layout=c(2,1)))


print(plot1)
}
