sj.strata.old <-
function(sj.obj)
{

  jura.folder <- "C:/Work/Neph-LR/Jura Sediments/"
  jura.files <- list.files(jura.folder)
  
  strata.vector <- vector(length=length(sj.obj$lat))
  
  for (a in (1:length(sj.obj$lat)))
  {
  
    x.val <- sj.obj$lon[a]
    y.val <- sj.obj$lat[a]
    
    sj <- read.table(paste(jura.folder, jura.files[3], sep=""), header=T)
    if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=sj[,1], pol.y=sj[,2])>0)
    {
      strata.vector[a] <- "MUDDY SAND"
    }
    
    sj <- read.table(paste(jura.folder, jura.files[4], sep=""), header=T)
    if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=sj[,1], pol.y=sj[,2])>0)
    {
      strata.vector[a] <- "MUDDY SAND"
    }
    
    sj <- read.table(paste(jura.folder, jura.files[7], sep=""), header=T)
    if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=sj[,1], pol.y=sj[,2])>0)
    {
      strata.vector[a] <- "SANDY MUD"
    }
    
    sj <- read.table(paste(jura.folder, jura.files[5], sep=""), header=T)
    if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=sj[,1], pol.y=sj[,2])>0)
    {
      strata.vector[a] <- "MUDDY SAND"
    }
    
    sj <- read.table(paste(jura.folder, jura.files[6], sep=""), header=T)
    if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=sj[,1], pol.y=sj[,2])>0)
    {
      strata.vector[a] <- "MUDDY SAND"
    }
    
    sj <- read.table(paste(jura.folder, jura.files[1], sep=""), header=T)
    if (point.in.polygon(point.x=x.val, point.y=y.val, pol.x=sj[,1], pol.y=sj[,2])>0)
    {
      strata.vector[a] <- NA # should be "FALSE" me thinks...
    }
    
    sj <- read.table(paste(jura.folder, jura.files[2], sep=""), header=T)
    if(point.in.polygon(point.x=x.val, point.y=y.val, pol.x=sj[,1], pol.y=sj[,2])>0)
    {
      strata.vector[a] <- "MUD"
    }
  }
  
  return(strata.vector)
}

