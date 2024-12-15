##########################################################
#
# Calculate statistics 
#
#---------------------
# Create dataframe with statistics (median, CrI) 
# of each column of input dataframe
##########################################################

# input = DFst = data frame (s x t), with rows=nsim, col=timepoints
# creates stats (median,etc) of EACH COLUMN!!!
# output = data frame (t x 3), rows=timepoints, col=median,LCrI,UCrI

statsFun <- function(DFst){
  eStats <- data.frame(
    epiM = sapply(DFst, function(DFst)   median(DFst,            na.rm=TRUE)),
    epiL = sapply(DFst, function(DFst) quantile(DFst,prob=0.025, na.rm=TRUE)),
    epiU = sapply(DFst, function(DFst) quantile(DFst,prob=0.975, na.rm=TRUE))
    )
  return(eStats)
}

################################### END ##################