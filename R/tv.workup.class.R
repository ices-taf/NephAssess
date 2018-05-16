
print.tvworkup <-
function (x, ...)
{
  for (i in seq_along(x))
  {
    txt <- gettextf("Strata type: %s", names(x[i]))
    cat("\n\n", txt, "\n", sep = "")
    cat( gsub("[a-zA-Z: ]", "-", txt), "\n", sep = "") 
    for (j in names(x[[i]])[-1])
    {
      cat( gettextf("%15s : ", j) )
      if (j == "av.dens") cat(gettextf(" %.2f", x[[i]] [[j]]))
      else cat(gettextf(" %10.3f", x[[i]] [[j]]))
      cat("\n")
    }
    cat("\nstrata_type:\n")
    print(x[[i]] $ strata_type)
  }
  invisible()
}

"[.tvworkup" <-
function (x, ...) 
{
  y <- NextMethod("[")
  class(y) <- oldClass(x)
  y
}

