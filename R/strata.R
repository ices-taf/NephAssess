
make.strata.data <-
function(fladen.dir, moray.dir, forth.dir, north.minch.dir, south.minch.dir, clyde.dir, jura.dir, output.dir)
{
  read.file <- function(x) read.table(x, header = TRUE, col.names = c("lon", "lat"))
  
  make.poly <- 
  function(folder)
  {
    fnames <- list.files(folder, full.name = TRUE)
    f.u.poly <- lapply(fnames, read.file)
    names(f.u.poly) <- list.files(folder)
    
    f.u.poly
  }
 
   clyde.poly <- make.poly (clyde.dir)
  save(clyde.poly, file = paste(output.dir, "clyde.rda", sep = ""))
                          
  south.minch.poly <- make.poly (south.minch.dir)
  save(south.minch.poly, file = paste(output.dir, "south.minch.rda", sep = ""))
  
  north.minch.poly <- make.poly (north.minch.dir)
  save(north.minch.poly, file = paste(output.dir, "north.minch.rda", sep = ""))
  
  moray.poly <- make.poly (moray.dir)
  save(moray.poly, file = paste(output.dir, "moray.rda", sep = ""))
  
  forth.poly <- make.poly (forth.dir)
  save(forth.poly, file = paste(output.dir, "forth.rda", sep = ""))
  
  fladen.poly <- make.poly (fladen.dir)
  save(fladen.poly, file = paste(output.dir, "fladen.rda", sep = ""))

  jura.poly <- make.poly (jura.dir)
  save(jura.poly, file = paste(output.dir, "jura.rda", sep = ""))
  
  invisible() 
}

check.fu <-
function (f.u)
{
  f.u <- tolower(f.u)
  stopifnot( f.u %in% c("south minch", "clyde", "jura", "moray firth", "firth forth", "north minch", "fladen",
              "devils hole", "noup"))
  
  f.u
}

which.poly <-
function (obj, fu.poly)
{
 lapply( seq_along( obj $ lon ), 
  function (i)
  {
    which(
      sapply(fu.poly, 
        function(x) 
          point.in.polygon(obj $ lon[i], obj $ lat[i], x $ lon, x $ lat) ) > 0) 
  })
}


clyde.strata <-
function(obj, check = FALSE)
{
  assign.type <-
  function (x)
  {
    if (any(x %in% 17:25)) return( "MUD" )
    if (any(x %in% c(44:49))) return( "SANDY MUD" )
    if (any(x %in% 1:16)) return( "FALSE" )
    if (any(x %in% 26:35)) return( "MUDDY SAND" )
    return ("FALSE")
  }       

  if (check) return( cbind( sapply( seq_along(clyde.poly), assign.type), names(clyde.poly) ) )
                           
  sapply( which.poly( obj, clyde.poly ), assign.type ) 
}

south.minch.strata <-
function(obj, check = FALSE)
{
  assign.type <-
  function (x)
  {
    if (any(x %in% 1:16)) return( "MUD" )
    #if (any(x == 52)) return( "FALSE" )
    if (any(x %in% 53:72)) return( "SANDY MUD" )
    #if (any(x == 51)) return( "FALSE" )
    if (any(x %in% 17:50)) return( "MUDDY SAND" )
    return ("FALSE")
  }

  if (check) return( cbind( sapply( seq_along(south.minch.poly), assign.type), names(south.minch.poly) ) )
                               
  sapply( which.poly (obj, south.minch.poly), assign.type)       
}

moray.strata <- 
function (obj, check = FALSE)
{
  assign.type <-
  function (x)
  {
    if (any(x == 1)) return( "MUD" )
    if (any(x %in% 21:25)) return( "SANDY MUD" )
    if (any(x %in% 9:20)) return( "FALSE" )
    if (any(x %in% 2:8)) return( "MUDDY SAND" )
    return ("FALSE")
  }

  if (check) return( cbind( sapply( seq_along(moray.poly), assign.type), names(moray.poly) ) )
                               
  sapply( which.poly (obj, moray.poly), assign.type)       
}

devils.hole.strata <- 
function (obj, check = FALSE)
{
  assign.type <-
  function (x)
  {
    if (any(x %in% 1:12)) return( "MUDDY SAND" )
    if (any(x %in% 13:22)) return( "FALSE" )
    return ("FALSE")
  }

  if (check) return( cbind( sapply( seq_along(devils.hole.poly), assign.type), names(devils.hole.poly) ) )
                               
  sapply( which.poly (obj, devils.hole.poly), assign.type)       
}

forth.strata <- 
function(obj, check = FALSE)
{
  assign.type <-
  function (x)
  {
    if (any(x %in% 1:4)) return( "MUD" )
    if (any(x %in% 17:22)) return( "FALSE" )
    if (any(x %in% 54:66)) return( "SANDY MUD" )
    if (any(x %in% 23:53)) return( "FALSE" )
    if (any(x %in% 5:16)) return( "MUDDY SAND" )
    return ("FALSE")
  }

  if (check) return( cbind( sapply( seq_along(forth.poly), assign.type), names(forth.poly) ) )
                               
  sapply( which.poly (obj, forth.poly), assign.type)       
}

fladen.strata <- 
function(obj, check = FALSE)
{
  assign.type <-
  function (x)
  {
    if (any(x == 16)) return( "MUDDY SAND" )
    if (any(x %in% c(86, 93))) return( "SANDY MUD" )
    if (any(x %in% 37:38)) return( "FALSE" )
    if (any(x %in% 1:5)) return( "MUD" )
    if (any(x %in% c(15, 21))) return( "MUDDY SAND" )
    if (any(x %in% 75:85)) return( "FALSE" )
    if (any(x %in% 86:102)) return( "SANDY MUD" )
    if (any(x %in% 39:67)) return( "FALSE" )
    if (any(x %in% 6:36)) return( "MUDDY SAND" )
    return ("FALSE")
  }

  if (check) return( cbind( sapply( seq_along(fladen.poly), assign.type), names(fladen.poly) ) )
                               
  sapply( which.poly (obj, fladen.poly), assign.type)       
}

fladen.surfer.strata <-
function(obj, check = FALSE)
{
  assign.type <-
  function (x)
  {
    if (any(x %in% c(6,8,11,15,16,21))) return( "FALSE" )
    if (any(x %in% c(13,22))) return( "MC" )
    if (any(x %in% c(12))) return( "MF" )
    if (any(x %in% c(10))) return( "F" )
    if (any(x %in% c(5,7,9,14))) return( "MF" )
    if (any(x %in% c(1,2,3,4,23))) return( "MC" )
    if (any(x %in% c(17:20,24,25))) return( "C" )                   
    return ("FALSE")
  }       

  if (check) return( cbind( sapply( seq_along(fladen.surfer.poly), assign.type), names(fladen.surfer.poly) ) )
                           
  sapply( which.poly( obj, fladen.surfer.poly ), assign.type ) 
}

sj.strata <-
function(obj, check = FALSE)
{
  assign.type <-
  function (x)
  {
    if (any(x == 2)) return( "MUD" )
    if (any(x == 1)) return( "FALSE" )
    if (any(x %in% 5:6)) return( "MUDDY SAND" )
    if (any(x == 7)) return( "SANDY MUD" )
    if (any(x %in% 3:4)) return( "MUDDY SAND" )
    return ("FALSE")
  }

  if (check) return( cbind( sapply( seq_along(jura.poly), assign.type), names(jura.poly) ) )
                               
  sapply( which.poly (obj, jura.poly), assign.type)       
}

north.minch.strata <-
function (obj, check = FALSE)
{

  if (check) 
  {
    cat("no files used so nothing to check!\n")
    invisible()
  }

  sapply( seq(along = obj $ lats),
    function (i)
    {
      x.val <- obj $ lons[i]
      y.val <- obj $ lats[i]

      if ( point.in.polygon( x.val, y.val, c(-7.2, -6.2, -6.2, -7.2), c(57.9, 57.9, 57.5, 57.5) ) )
          return ( "X" )

      else if ( point.in.polygon( x.val, y.val, c(-6.2, -5.8, -5.8, -6.2), c(57.9, 57.9, 57.5, 57.5) ) )
          return ( "W" )

      else if ( point.in.polygon( x.val, y.val, c(-6.33, -5.8, -5.8, -6.33), c(58.4, 58.4, 57.9, 57.9) ) )
          return ( "U" )

      else if ( point.in.polygon( x.val, y.val, c(-5.8, -5.4, -5.4, -5.8), c(58.4, 58.4, 57.9, 57.9) ) )
          return ( "V" )

      else
        return ( "FALSE" )
    })
}


noup.strata <-
function(obj, check = FALSE)
{
  assign.type <-
  function (x)
  {
    if (any(x %in% 1:2)) return( "MUDDY SAND" )
	if (any(x %in% 3:5)) return( "FALSE" )
	return ( "FALSE" )
  }

  if (check) return( cbind( sapply( seq_along(noup.poly), assign.type), names(noup.poly) ) )
                               
  sapply( which.poly (obj, noup.poly), assign.type)       
}