`mean.sizes` <-
function (stock)
{

#  Mean size in catches  <35 mm

  lcats <- as.numeric(rownames(stock@landings.n))+1
  lcats.lower <- lcats[1:13]
  lcats.higher <- lcats[14:length(lcats)]

  low.male.c <- stock@catch.n[1:13,,1,,,]@.Data
  tmp <- low.male.c * lcats.lower
  x <- colSums(tmp)
  y <- colSums(low.male.c)
  means.male.low.c <- rowSums(x)/rowSums(y)
  means.male.low.c[!is.finite(means.male.low.c)] <- NA

  low.female.c <- stock@catch.n[1:13,,2,,,]@.Data
  tmp <- low.female.c * lcats.lower
  x <- colSums(tmp)
  y <- colSums(low.female.c)
  means.female.low.c <- rowSums(x)/rowSums(y)
  means.female.low.c[!is.finite(means.female.low.c)] <- NA


#  Mean size in landings  < 35 mm

  low.male <- stock@landings.n[1:13,,1,,,]@.Data     # the 13th number is the cut off point for the lower size category
  tmp <- low.male*lcats.lower
  x <- colSums(tmp)
  y <- colSums(low.male)
  means.male.low <- rowSums(x)/rowSums(y)
  means.male.low[!is.finite(means.male.low)] <- NA

  low.female <- stock@landings.n[1:13,,2,,,]@.Data
  tmp <- low.female*lcats.lower
  x <- colSums(tmp)
  y <- colSums(low.female)
  means.female.low <- rowSums(x)/rowSums(y)
  means.female.low[!is.finite(means.female.low)] <- NA


 # Mean size in landings > 35mm

  high.male <- stock@landings.n[14:(length(lcats)),,1,,,]@.Data
  tmp <- high.male*lcats.higher
  x <- colSums(tmp)
  y <- colSums(high.male)
  means.male.high <- rowSums(x)/rowSums(y)
  means.male.high[!is.finite(means.male.low)] <- NA

  high.female <- stock@landings.n[14:(length(lcats)),,2,,,]@.Data
  tmp <- high.female*lcats.higher
  x <- colSums(tmp)
  y <- colSums(high.female)
  means.female.high <- rowSums(x)/rowSums(y)
  means.female.high[!is.finite(means.female.low)] <- NA

  list(
        means.male.low.c = means.male.low.c, means.female.low.c = means.female.low.c,
        means.male.low = means.male.low, means.female.low = means.female.low,
        means.male.high = means.male.high, means.female.high = means.female.high
       )
}

