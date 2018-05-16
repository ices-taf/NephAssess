create.count.files <-
function(target.folder, big.csv)
{

  sites <- unique(big.csv[,1])

  for (i in (1:length(sites)))
  {
    temp.mat <- big.csv[big.csv[,1]==sites[i],]

    countfile <- file(description = paste(target.folder, "/", sites[i], ".txt", sep=""), open = "w", blocking = TRUE)

    cat(dim(temp.mat)[1]+1, file = countfile, sep = "\n")
    cat(paste("0 0"), file = countfile, sep = "\n")

    for (j in (1:dim(temp.mat)[1]))
    {
      cat(paste(temp.mat[j,2], temp.mat[j,3], temp.mat[j,4], sep=" "), file=countfile, sep="\n")
    }    

    close(countfile)
  }
}

