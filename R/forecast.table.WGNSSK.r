#~~~~~~~~~~~~~~~forecast.table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description: Function to create the forecast summary output
#                 table including dead & surviving discards
# Author: HD/CM
# Date:   26/3/2014
#
#  Input parameters:
#
#   wk.dir        - working directory where the input .csv file is stored & where output
#                   file will be created
#
#   fu            - string giving the name of the FU - used to title the output file & also
#                   if north minch allows for different input format
#
#   hist.sum.table - .csv file with the following columns (column names must
#                           be identical):
#                  'year' - up to and including the most recent year of survey data
#                  'mean.wt.landings' - annual individual mean weight in the landings (g)
#                  'mean.wt.discards' - annual individual mean weight in the discards (g)
#                  'dead.discard.rate'- annual discard rate - dead discards as % of
#                     removals (dead discards + landings)
#                  'adjusted.abundance'- bias corrected survey abundance in millions
#                     (if fu=="north minch",function looks for column titled
#                     'adjusted.abundance.VMS').  The last value in this column is used
#                     in the forecast calculations.
#
#   land.wt.yrs, disc.wt.yrs, disc.rt.yrs - sequences of years over which the landings mean wt,
#                   discard mean wt & dead discard rate are averaged (not weighted average)
#
#   hr.rates     - array of harvest rates (%).  Should be named to provide the information in
#                 the basis column of the summary table
#
#   d.surv       - discard survival rate, defaults to 0.25.
#
# Notes:  function does some checks on the rates - are they % or decimal? Output in tonnes,
#         provided inputs are survey in millions & weights in g.  Rounds output to whole
#         tonnes.
#
# 01/05/2015
#  Function edited from forecast.table.r to accomodate changes in the format and method of forescast for
# advice given in 2015. Landings and discards are now wanted and unwanted catches respectively (no survival).
#  Total discard rates used in place of dead discard rates.
# 04/05/2017
#  Function edited from forecast.table.2015 to give 3 forecast tables as requested by WGNSSK 2017

forecast.table.WGNSSK<- function(wk.dir, fu, hist.sum.table, mean.wts, land.wt.yrs, disc.wt.yrs, disc.rt.yrs,
                          h.rates, d.surv =0.25, latest.advice)
{

#  setwd(wk.dir)
  expl.dat<- read.csv(paste0(wk.dir, hist.sum.table))
  wts<- read.csv(paste0(wk.dir, mean.wts))

#----------------------------Inputs----------------------------------------------------------------------------

# Discard survival
  if(d.surv>1){d.surv <-d.surv/100}

  # Mean weights
  #land.mean.wt <-round(mean(expl.dat$mean.wt.landings[expl.dat$year %in% land.wt.yrs],na.rm=T),2)
  #disc.mean.wt <- round(mean(expl.dat$mean.wt.discards[expl.dat$year %in% disc.wt.yrs],na.rm=T),2)
  land.mean.wt <-round(mean(wts$mean.wt.landings.g[wts$years %in% land.wt.yrs],na.rm=T),2)
  disc.mean.wt <- round(mean(wts$mean.wt.discards.g[wts$years %in% disc.wt.yrs],na.rm=T),2)
  disc.above.MCS.mean.wt<- round(mean(wts$mean.wt.discards.over.g[wts$years %in% disc.wt.yrs],na.rm=T),2)
  disc.below.MCS.mean.wt<- round(mean(wts$mean.wt.discards.under.g[wts$years %in% disc.wt.yrs],na.rm=T),2)

# Discard rates
  disc.mean.rate <-round(mean(expl.dat$discard.rate[expl.dat$year %in% disc.rt.yrs])/100,3)
  disc.mean.rate.above.MCS<- disc.mean.rate*(disc.mean.wt-disc.below.MCS.mean.wt)/(disc.above.MCS.mean.wt-disc.below.MCS.mean.wt)
  disc.mean.rate.below.MCS<- disc.mean.rate-disc.mean.rate.above.MCS
  dead.disc.mean.rate<- disc.mean.rate*(1-d.surv)/(disc.mean.rate*(1-d.surv) + (1-disc.mean.rate))
  dead.disc.mean.rate.below.MCS<- disc.mean.rate.below.MCS*(1-d.surv)/(disc.mean.rate.below.MCS*(1-d.surv) + (1-disc.mean.rate.below.MCS))

  if(disc.mean.rate==0)
    {
      disc.mean.rate.above.MCS<- disc.mean.rate.below.MCS<- dead.disc.mean.rate<- dead.disc.mean.rate.below.MCS<- 0
      disc.mean.wt<- disc.above.MCS.mean.wt<- disc.below.MCS.mean.wt<- 0
    }

#TV Survey abundance
  
  ab.col<- names(expl.dat)[grep("adjusted.abundance", names(expl.dat))[1]]
  last.survey.year<- expl.dat[ max(which(!is.na(expl.dat[,ab.col]))),"year"]
  surv.abundance <- expl.dat[expl.dat$year %in% last.survey.year,ab.col]
  if(fu %in% c("north minch", "nm", "FU11", "FU 11")){
    surv.abundance <-expl.dat[expl.dat$year %in% last.survey.year,"adjusted.abundance.VMS"]
  }

#Harvest ratios
  if(!is.null(names(h.rates))){
    summary.output <-data.frame(Basis=names(h.rates))
  }else{
    summary.output <-data.frame(Basis=rep("",length(h.rates)))
  }
  summary.output$harvest.rate <-unname(h.rates)
  if(summary.output$harvest.rate[1]>1){
    hrs <-summary.output$harvest.rate/100
  }
#--------------------------------------------------------------------------------------------------------------


  #Catch options assuming zero discards
  summary.output1<-summary.output
  summary.output1$wanted.catch.tonnes <-round(hrs*surv.abundance*(1-disc.mean.rate)*land.mean.wt)
  summary.output1$unwanted.catch.tonnes <-round(hrs*surv.abundance*disc.mean.rate*disc.mean.wt)
  summary.output1$total.catch.tonnes <-with(summary.output1,wanted.catch.tonnes+unwanted.catch.tonnes)
  summary.output1$percentage.advice.change <- paste0(icesRound(100*(summary.output1$total.catch.tonnes-latest.advice[2])/latest.advice[2]), "%")
  summary.output1[summary.output1$Basis %in% "Flower","percentage.advice.change"]<- paste0(icesRound(100*(summary.output1[summary.output1$Basis %in% "Flower","total.catch.tonnes"]-latest.advice[1])/latest.advice[1]), "%")
  summary.output1<- summary.output1[,c("Basis", "total.catch.tonnes", "wanted.catch.tonnes", "unwanted.catch.tonnes", "harvest.rate", "percentage.advice.change")]

  #Catch options assuming discarding is allowed
  summary.output2<-summary.output
  summary.output2$landings.tonnes <-round(hrs*surv.abundance*(1-dead.disc.mean.rate)*land.mean.wt)
  summary.output2$dead.discards.tonnes <-round(hrs*surv.abundance*dead.disc.mean.rate*disc.mean.wt)
  summary.output2$surviving.discards.tonnes <-round(summary.output2$dead.discards.tonnes/((1-d.surv)/d.surv))
  summary.output2$total.catch.tonnes <-with(summary.output2,landings.tonnes+dead.discards.tonnes+surviving.discards.tonnes)
  summary.output2$dead.removals.tonnes <-with(summary.output2,landings.tonnes+dead.discards.tonnes)
  summary.output2$percentage.advice.change <- paste0(icesRound(100*(summary.output2$total.catch.tonnes-latest.advice[2])/latest.advice[2]), "%")
  summary.output2[summary.output2$Basis %in% "Flower","percentage.advice.change"] <- paste0(icesRound(100*(summary.output2[summary.output2$Basis %in% "Flower","total.catch.tonnes"]-latest.advice[1])/latest.advice[1]), "%")
  summary.output2<- summary.output2[,c("Basis", "total.catch.tonnes", "dead.removals.tonnes", "landings.tonnes", "dead.discards.tonnes", "surviving.discards.tonnes","harvest.rate", "percentage.advice.change")]

  #Discarding allowed for de minimis excemptions only
  summary.output3<-summary.output
  dead.discards.below.MCS.N<- hrs*surv.abundance*dead.disc.mean.rate.below.MCS
  surviving.discards.N<- dead.discards.below.MCS.N*d.surv/(1-d.surv)
  unwanted.above.MCS.N<- (hrs*surv.abundance+surviving.discards.N)*disc.mean.rate.above.MCS
  landings.N<- hrs*surv.abundance-(unwanted.above.MCS.N+dead.discards.below.MCS.N)
  summary.output3$landings.tonnes<- round(landings.N*land.mean.wt)
  summary.output3$unwanted.above.MCS.tonnes<- round(unwanted.above.MCS.N*disc.above.MCS.mean.wt)
  summary.output3$dead.discards.below.MCS.tonnes<- round(dead.discards.below.MCS.N*disc.below.MCS.mean.wt)
  summary.output3$surviving.discards.tonnes<- round(surviving.discards.N*disc.below.MCS.mean.wt)
  summary.output3$total.catch.tonnes <- with(summary.output3,landings.tonnes+unwanted.above.MCS.tonnes+dead.discards.below.MCS.tonnes+surviving.discards.tonnes)
  summary.output3$dead.removals.tonnes <- with(summary.output3,landings.tonnes+unwanted.above.MCS.tonnes+dead.discards.below.MCS.tonnes)
  summary.output3$percentage.advice.change <- paste0(icesRound(100*(summary.output3$total.catch.tonnes-latest.advice[2])/latest.advice[2]), "%")
  summary.output3[summary.output3$Basis %in% "Flower","percentage.advice.change"] <- paste0(icesRound(100*(summary.output3[summary.output3$Basis %in% "Flower","total.catch.tonnes"]-latest.advice[1])/latest.advice[1]), "%")
  summary.output3<- summary.output3[,c("Basis", "total.catch.tonnes","dead.removals.tonnes","landings.tonnes","unwanted.above.MCS.tonnes","dead.discards.below.MCS.tonnes","surviving.discards.tonnes","harvest.rate", "percentage.advice.change")]


 #Collect all outputs and inputs into a single object
  out.sum <-vector("list")
  out.sum$table1 <-summary.output1
  out.sum$table2 <-summary.output2
  out.sum$table3 <-summary.output3


#  out.sum$table[,2:4]<-lapply(out.sum$table[,2:4],round,0)
#  out.sum$input.txt <-rep("",11)

  out.sum$input.txt[1] <-paste("Abundance in TV (",last.survey.year,") = " ,surv.abundance,
               " million",sep="")
  out.sum$input.txt[2] <-paste("Mean weight in landings (",land.wt.yrs[1],"-",land.wt.yrs[length(land.wt.yrs)],") = ",
                         round(land.mean.wt,2)," g",sep="")
  out.sum$input.txt[3] <-paste("Mean weight in discards (",disc.wt.yrs[1],"-",disc.wt.yrs[length(disc.wt.yrs)],") = ",
                         round(disc.mean.wt,2)," g",sep="")
  out.sum$input.txt[4] <-paste("Mean weight in unwanted catch > MCS (",disc.rt.yrs[1],"-",disc.rt.yrs[length(disc.rt.yrs)],
                               ") = ",disc.above.MCS.mean.wt," g",sep="" )
  out.sum$input.txt[5] <-paste("Mean weight in unwanted catch < MCS (",disc.rt.yrs[1],"-",disc.rt.yrs[length(disc.rt.yrs)],
                               ") = ",disc.below.MCS.mean.wt," g",sep="" )
  out.sum$input.txt[6] <-paste("Discard rate total (",disc.rt.yrs[1],"-",disc.rt.yrs[length(disc.rt.yrs)],
                               ") = ",disc.mean.rate," proportion by number",sep="" )
  out.sum$input.txt[7] <-paste("Discard rate > MCS (",disc.rt.yrs[1],"-",disc.rt.yrs[length(disc.rt.yrs)],
                               ") = ",round(disc.mean.rate.above.MCS,3)," proportion by number",sep="" )
  out.sum$input.txt[8] <-paste("Discard rate < MCS (",disc.rt.yrs[1],"-",disc.rt.yrs[length(disc.rt.yrs)],
                               ") = ",round(disc.mean.rate.below.MCS,3)," proportion by number",sep="" )
  out.sum$input.txt[9] <-paste("Discard survival rate = ",d.surv," proportion by number",sep="")
  out.sum$input.txt[10] <-paste("Dead discard rate total (",disc.rt.yrs[1],"-",disc.rt.yrs[length(disc.rt.yrs)],
                               ") = ",round(dead.disc.mean.rate,3)," proportion by number",sep="" )
  out.sum$input.txt[11] <-paste("Dead discard rate < MCS (",disc.rt.yrs[1],"-",disc.rt.yrs[length(disc.rt.yrs)],
                                ") = ",round(dead.disc.mean.rate.below.MCS,3)," proportion by number",sep="" )

  #Save output to file
  filename<- paste(wk.dir,fu,"_forecast_table_WGNSSK.csv",sep="")
  cat("Catch options assuming zero discards","\n",file=filename, append=FALSE)
  suppressWarnings(write.table(out.sum$table1,filename,row.names=FALSE, sep=",", append=TRUE))
  cat("\n",file=filename, append=TRUE); cat("Catch options assuming discarding is allowed","\n",file=filename, append=TRUE)
  suppressWarnings(write.table(out.sum$table2,filename,row.names=FALSE, sep=",", append=TRUE))
  cat("\n",file=filename, append=TRUE); cat("Discarding assumed below MCS only","\n",file=filename, append=TRUE)
  suppressWarnings(write.table(out.sum$table3,filename,row.names=FALSE, sep=",", append=TRUE))
  cat("\n",file=filename, append=TRUE); cat("Inputs for the catch options","\n",file=filename, append=TRUE)
  for (l in 1:length(out.sum$input.txt)){
    cat(out.sum$input.txt[l],file=filename, append=TRUE)
    cat("\n",file=filename, append=TRUE)
  }
}


