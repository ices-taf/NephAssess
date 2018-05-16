#~~~~~~~~~~~~~~~forecast.table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Description: Function to create the forecast summary output
#                 table including dead & surviving discards
# Author: HD
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
#  Corrected error with discard rate calculation (when the average of the last 3 years < 1% the
# discard percentage was multiplied by 100. eg Fladen_discard_12-14=0.6% -----> old output was 60%
#

forecast.table.2014<- function(wk.dir, fu, hist.sum.table, land.wt.yrs, disc.wt.yrs, disc.rt.yrs, 
                          h.rates, d.surv =0.25)
{

  setwd(wk.dir)
  expl.dat<- read.csv(hist.sum.table)

# Calculate mean weights
  land.mean.wt <-mean(expl.dat$mean.wt.landings[expl.dat$year %in% land.wt.yrs],na.rm=T)
  disc.mean.wt <-mean(expl.dat$mean.wt.discards[expl.dat$year %in% disc.wt.yrs],na.rm=T)

# Dead discard mean rate
  dead.disc.mean.rate <-mean(expl.dat$dead.discard.rate[expl.dat$year %in% disc.rt.yrs])
  if(dead.disc.mean.rate>1){
    dead.disc.mean.rate <-dead.disc.mean.rate/100
  } else{
		dead.disc.mean.rate <-round(dead.disc.mean.rate/100,3)
  }

#
  surv.abundance <-expl.dat$adjusted.abundance[length(expl.dat$adjusted.abundance)]
  if(fu == "north minch"){
    surv.abundance <-expl.dat$adjusted.abundance.VMS[length(expl.dat$adjusted.abundance.VMS)]
  }

#
  if(!is.null(names(h.rates))){
    summary.output <-data.frame(Basis=names(h.rates))
  }else{
    summary.output <-data.frame(Basis=rep("",length(h.rates)))
  }
  
  summary.output$harvest.rate <-unname(h.rates) 
  if(summary.output$harvest.rate[1]>1){
    hrs <-summary.output$harvest.rate/100
  }

  if(d.surv>1){d.surv <-d.surv/100}
  
  summary.output$landings.tonnes <-round(hrs*surv.abundance*(1-dead.disc.mean.rate)*land.mean.wt)
  summary.output$dead.discards.tonnes <-round(hrs*surv.abundance*dead.disc.mean.rate*disc.mean.wt)
  summary.output$surviving.discards.tonnes <-round(summary.output$dead.discards.tonnes/((1-d.surv)/d.surv))
  summary.output$total.catch.tonnes <-with(summary.output,landings.tonnes+dead.discards.tonnes+
                                                  surviving.discards.tonnes)
  summary.output$dead.removals.tonnes <-with(summary.output,landings.tonnes+dead.discards.tonnes)
  
  out.sum <-vector("list")
  out.sum$table <-summary.output[,c(1,6,7,3,4,5,2)]
  out.sum$table[,2:6]<-lapply(out.sum$table[,2:6],round,0)
  out.sum$input.txt <-rep("",5)
  
  out.sum$input.txt[1] <-paste("Bias corrected survey index (",expl.dat$year[length(expl.dat$year)],") = " ,surv.abundance,
               " million",sep="")
  out.sum$input.txt[2] <-paste("Mean weight in landings (",land.wt.yrs[1],"-",land.wt.yrs[length(land.wt.yrs)],") = ",
                         round(land.mean.wt,2)," g",sep="")
  out.sum$input.txt[3] <-paste("Mean weight in discards (",disc.wt.yrs[1],"-",disc.wt.yrs[length(land.wt.yrs)],") = ",
                         round(disc.mean.wt,2)," g",sep="")
  out.sum$input.txt[4] <-paste("Dead discard rate by number (",disc.rt.yrs[1],"-",disc.rt.yrs[length(disc.rt.yrs)],
                         ") = ",round(100*dead.disc.mean.rate,1)," %",sep="" )
  out.sum$input.txt[5] <-paste("Discard survival rate = ",round(d.surv*100,0)," %",sep="")
  
  write.csv(out.sum$table,paste(fu,"_summary_table.csv",sep=""),row.names=FALSE)
  for (l in 1:length(out.sum$input.txt)){
    cat("\n",out.sum$input.txt[l],file=paste(fu,"_summary_table.csv",sep=""), append=TRUE)
  }

}

