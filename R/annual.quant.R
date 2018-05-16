annual.quant <-
function(quant.object)
{
  quant.object[,,,1,] + quant.object[,,,2,] + quant.object[,,,3,] + quant.object[,,,4,]
  # or apply(quant.object @ .Data, c(1,2,3,5), sum) ??
}

