lat.lon.distances <- function(poly.list){

    require(splancs, quietly=T)
    require(sp, quietly=T)


    temp.mat <- cbind(poly.list[,1], poly.list[,2])
    colnames(temp.mat) <- c("x","y")

    res.mat <- matrix(nrow=dim(temp.mat)[1], ncol=2)
    
    colnames(res.mat) <- c("x", "y")

    sw.x <- min(temp.mat[,1])
    sw.y <- min(temp.mat[,2])

        for(i in (1:dim(temp.mat)[1])){
    
            ll <- matrix(c(sw.x, temp.mat[i,1], sw.y, sw.y), ncol=2)
            km <- spDistsN1(ll, ll[1,], longlat=TRUE)

            res.mat[i,1] <- km[2]

            ll <- matrix(c(sw.x, sw.x, sw.y, temp.mat[i,2]), ncol=2)
            km <- spDistsN1(ll, ll[1,], longlat=TRUE)

            res.mat[i,2] <- km[2]

        }
    
    

    return(res.mat)

}
