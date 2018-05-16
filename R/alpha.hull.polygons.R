alpha.hull.polygons<- function(lons, lats, a)
{ 
	m<- as.matrix(cbind(lons, lats), ncol=2)

	ahull.o<- ahull(m, alpha = a) 
	
	test<- as.data.frame(ahull.o$arcs)
	test<- subset(test, end1 != end2)
	ahull.o$arcs<- as.matrix(test)

	plot(m, cex=0.1)
	for (i in 1:dim(ahull.o$arcs)[1]){
		  arc(ahull.o$arcs[i,1:2],ahull.o$arcs[i,3],ahull.o$arcs[i,4:5],
		  ahull.o$arcs[i,6],col=2)
		  }
	  
	n.points<- dim(ahull.o$arcs)[1]
	e1<- ahull.o$arcs[,7]
	e2<- ahull.o$arcs[,8]

	breaks<- which((e1[-1] != e2[-n.points]) == TRUE)
	n.groups<- length(breaks) + 1

	groups.list<- vector("list", length=n.groups)
	groups.list[[1]]<- e1[1:breaks[1]]
	groups.list[[n.groups]]<- e1[(breaks[n.groups-1] + 1) : n.points]
	for(i in 1:(length(breaks)-1))
	{
		groups.list[[i+1]]<- e1[(breaks[i] + 1) : (breaks[i+1])]
	}

	poly<- ahull.o$x
	rownames(poly)<- NULL

	logic.gl<- lapply(1:n.groups, function(v) 
	{
		length(groups.list[[v]]) >2
	})

	groups.list<- groups.list[logic.gl==T]
	groups.list<- lapply(1:length(groups.list), function(v)
	{
		temp<- as.data.frame(poly[groups.list[[v]],])
		colnames(temp)<- c("x", "y")
		#temp$group<- rep(v, n=dim(temp)[1])
		temp		
	})
	return(groups.list)
}
