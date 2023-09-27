#this function takes was agreed at WGNSSK to produce a standard LF plot for the North sea/Celtic seas Nephrops advice sheets
#"Year","Sex","Length","LandNaL","CatchNaL"
## Load data frame with annual catch and landings LDs, assumed to be called CatchLDsYr for this example.
## Parameter names in CatchLDsYr are assumed to be:
## * Year 
## * Sex
## * Length
## * CatchNaL = catch numbers-at-length
## * LandNaL = landings numbers-at-length
#function also takes a name for the graph title,  minimum and maximum years.
#by default the function bins length classes into 2mm slots - if you want anything different then change the line bin<-2 to something else!


NEP_LD_plot_ICES<- function(df, FU, FUMCRS, RefLth, out.dir)
  {

    vectorise.names <- function(tab,name){
      n<-length(attributes(tab)[[1]])
      dims<-attributes(tab)[[1]]
      len<-prod(attributes(tab)[[1]])
      d2<-c(dims, 0)
      n1 <- name[1]
      n2 <- name[2:length(name)]
      #set up the data frame to be of the correct length
      df<-data.frame(as.vector(tab))
      names(df)<-"value"
      j<-2
      for(i in 1:n){
        ech<- max(1,prod(dims[0:(i-1)]))  # this is the number of sets
        reps<-max(1,prod(d2[(i+1):n]))  # this is the number of repeats of each number within a set
        df[j]<-rep(dimnames(tab)[[i]],reps,each=ech)
        j<-j+1
      }
      names(df)<-c("value", n2)
      names(df)[1] <- n1
      df
    }  
    
    
    # width of length bins
    BinWth <- 2
    
    # annual numbers-at-length by sex, aggregated in size bins 
    # scaled to the maximum of the catch LD
    # renaming sex labels to Female and Male
    df <- df %>%
      mutate(Length=round_any(Length,BinWth,floor)+1,
             Sex=ifelse(Sex=="F","Female",
                        ifelse(Sex=="M","Male",NA))) %>%
      ddply(.,.(Year,Sex,Length),summarise,
            LandNaL=sum(LandNaL),
            CatchNaL=sum(CatchNaL)) %>%
      left_join(.,ddply(.,.(Year,Sex),summarise,MaxNaL=max(CatchNaL))) %>%
      mutate(LandSNaL=LandNaL/MaxNaL,
             CatchSNaL=CatchNaL/MaxNaL,
             TimeLand=Year+0.9999*LandSNaL,
             TimeCatch=Year+0.9999*CatchSNaL) %>%
      dplyr::select(Year,Sex,Length,TimeLand,TimeCatch) %>%
      gather(key="Type",value="Time",TimeCatch,TimeLand) 
    
    
    png(filename=paste0(out.dir,"FU",FU,"_annual_LDs.png"),
        width=6,height=8,units="in",res=300,pointsize=12,
        restoreConsole=TRUE,type=c("windows","cairo","cairo-png"))
    
    par(lwd=2)
    
    t.par <- trellis.par.get("superpose.line")
    t.par2 <- t.par
    t.par2$col <- 1
    trellis.par.set("superpose.line",t.par2)
    
    plot1 <- with(df,xyplot(Time~Length|Sex,groups=Year, 
                            panel=function(x,y,subscripts,groups,...){
                              len <- length(x)
                              len2 <- len/2
                              panel.superpose(x[1:len2], y[1:len2], subscripts, groups, type="l", lty=2, lwd=1.8)
                              panel.superpose(x[len2+1:len], y[len2+1:len],subscripts, groups, type="l", lty=1, lwd=1.8)
                              if(RefLth>0) panel.abline(v=RefLth,lty=3)
                              panel.abline(v=FUMCRS,lty=3)
                              # landings
                              df <- as.data.frame(y[len2+1:len])
                              names(df)[1] <- "freq"
                              df$len <- x[len2+1:len]
                              df$year <- floor(df$freq)
                              df$freq2 <- df$freq-df$year
                              df$lenfreq <- df$freq2*df$len
                              tab <- with(df,tapply(lenfreq, year, sum))
                              meanvec <- vectorise.names(tab, c("meanlen", "year"))
                              tab <- with(df,tapply(freq2, year, sum))
                              vec<- vectorise.names(tab, c("totfreq", "year"))
                              m <- merge(meanvec, vec)
                              m$meanval <- m$meanlen/m$totfreq
                              m$year <- as.numeric(m$year)+0.5
                              panel.lines(m$meanval, m$year, type="l", lty=1, lwd=2)
                              #print(m)
                              # catches
                              df <- as.data.frame(y[1:len2])
                              names(df)[1] <- "freq"
                              df$len <- x[1:len2]
                              df$year <- floor(df$freq)
                              df$freq2 <- df$freq-df$year
                              df$lenfreq <- df$freq2*df$len
                              tab <- with(df,tapply(lenfreq, year, sum))
                              meanvec <- vectorise.names(tab, c("meanlen", "year"))
                              tab <- with(df,tapply(freq2, year, sum))
                              vec<- vectorise.names(tab, c("totfreq", "year"))
                              m <- merge(meanvec, vec)
                              m$meanval <- m$meanlen/m$totfreq
                              m$year <- as.numeric(m$year)+0.5
                              panel.lines(m$meanval, m$year, type="l", lty=2, col="red",lwd=2 )
                            },
                            main="Length frequencies for catches (dashed) and landings (solid)",
                            sub=(ifelse(RefLth>0, 
                                        paste0("Mean length of catches (red) and landings (blue) \n MCRS (",FUMCRS," mm CL) and ",RefLth," mm (vertical dashed lines)"),
                                        paste0("Mean length of catches (red) and landings (blue) \n MCRS (",FUMCRS," mm CL; vertical dashed line)"))),
                            xlab="Length  [mm]",ylab="Year",layout=c(2,1)))
    
    print(plot1)
    trellis.par.set("superpose.line",t.par) # reset trellis colours
    
    dev.off()
}




