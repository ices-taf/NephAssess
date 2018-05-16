`clyde.strata.old` <-
function(clyde.obj){

clyde.folder <- "C:/nephrops/Carlos/Assessment/WGCSE 09/Clyde/Clyde Sediments/"
clyde.files <- list.files(clyde.folder)

strata.vector <- vector(length=length(clyde.obj$lat))

for (a in (1:length(clyde.obj$lat))){

x.val <- clyde.obj$lon[a]
y.val <- clyde.obj$lat[a]

for(i in (26:35)){
clyde <- read.table(paste(clyde.folder, clyde.files[i], sep=""), header=T)
if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=clyde[,1], pol.y=clyde[,2])>0){
  strata.vector[a] <- "SANDY MUD"
  }
}

for(i in (1:16)){
clyde <- read.table(paste(clyde.folder, clyde.files[i], sep=""), header=T)
if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=clyde[,1], pol.y=clyde[,2])>0){
  strata.vector[a] <- "FALSE"
  }
}


clyde <- read.table(paste(clyde.folder, clyde.files[41], sep=""), header=T)
if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=clyde[,1], pol.y=clyde[,2])>0){
  strata.vector[a] <- "MUDDY SAND"
  }


for(i in (44:49)){
clyde <- read.table(paste(clyde.folder, clyde.files[i], sep=""), header=T)
if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=clyde[,1], pol.y=clyde[,2])>0){
strata.vector[a] <- "MUDDY SAND"
}
}


for(i in (17:25)){
clyde <- read.table(paste(clyde.folder, clyde.files[i], sep=""), header=T)
if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=clyde[,1], pol.y=clyde[,2])>0){
strata.vector[a] <- "MUD"
}
}



}
return(strata.vector)

}

