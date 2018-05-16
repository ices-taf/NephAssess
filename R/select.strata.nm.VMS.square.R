select.strata.nm.VMS.square <-
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

      if ( point.in.polygon( x.val, y.val, c(-7,-6.2,-6.2,-7), c(57.9, 57.9, 57.5, 57.5) ) )
          return ( "X" )

      else if ( point.in.polygon( x.val, y.val, c(-6.2,-5,-5,-6.2), c(57.9, 57.9, 57.5, 57.5) ) )
          return ( "W" )

      else if ( point.in.polygon( x.val, y.val, c(-7,-5.8,-5.8,-7), c(59,59,57.9,57.9) ) )
          return ( "U" )

      else if ( point.in.polygon( x.val, y.val, c(-5.8,-5,-5,-5.8), c(59,59,57.9,57.9) ) )
          return ( "V" )

      else
        return ( "FALSE" )
    })
}