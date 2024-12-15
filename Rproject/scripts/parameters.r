#################################################
# INITIATLIZATION
# Define fixed parameters, initial states, Time-maximum etc
#################################################

library(dplyr)

ag <- 3 #number of risk groups

nv <- 45 # number of model variables=5*3*3 (5 infection status S,E,I,Y,R; 3 vac.status; 3 sex.act.groups)

#---------------------
# Fixed parameters
#---------------------

aS <- c(0.1,  0.2,    0.7)/365
aC <- c(0.0202,.0823, 0.4343) # #cas  partn/d 
u  <- c(0.2190,0.2320,0.3284) # #sex acts /main partner/day 
q  <- c(0.679, 0.584, 0.55) # fraction of each act.group with main partn
mu     <- 1/(50*365) # 50yrs sex active 
oldVac <- 0.25
theta  <- 1/4 
delta  <- 1/4 
sigma1 <- 0.70
sigma2 <- 0.85
psi    <- 1/(3*365) # duration vac protection 3 yrs
muD    <- 0.0004
zeta   <- 0.01

#-------------------------------------------------------
# Transitions between sexual activity groups
#
# Data from SOAP: MSM in PrEP pilot summer2019-dec2022
# Analyses from DvW, 16may2024 
# No access to data/analyses - received only transition probabilities)
#-------------------------------------------------------

TransitionProbs <- matrix(c(0,0.3016,0.1140,0.1667,0,0.3499,0.0067,0.0416,0),nrow=ag,ncol=ag)
G <-   -log(1-TransitionProbs)/180

#---------------------------------------------------------
# READ FILE WITH DATA DAILY CASES (last update 2apr2024, with cases until 16mar2024)
#---------------------------------------------------------
gg <- read.csv("data/dataCasesDaily_update20240402.csv",header=TRUE,sep = ";",stringsAsFactors=FALSE)
gg <- mutate(gg,
  dateSymp = as.Date(dat.ezd.merge,"%d-%m-%Y"), #make new columns with nicer names than names in data file
  ncases = Op.26.maart.2024.of.eerder)
dataFM <- data.frame(gg[1:Tmax,4:5]) #take first nd days of data (data points on 27apr-25jul)
firstrow <- data.frame(dateSymp=as.Date(c("2022-04-26")),ncases=c(0))
dataFM <- rbind(firstrow,dataFM) 

#---------------------------
# Read file with daily numbers of vaccine doses
dataVacNumDaily <- read.table(
  file = "data/dataVacNumDailyMatrix.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = TRUE)

#---------------------
# uncertain param
#---------------------

#names of uncParam
upNames <- c("infectiousSym1","infectiousSym2","infectiousSym3", "beta","epsilonS","epsilonC","w","aC3","T1","T2","D1","D2") 

#nunmber of uncer parameters
Kpar <- length(upNames)   

# Ranges for Uniform distributions (priors)
upPriors <- matrix(nrow = Kpar, ncol = 3)
upPriors[ 1,] <- c( 4,21,"Infectious period with symptoms, period 1")
upPriors[ 2,] <- c( 2,14,"Infectious period with symptoms, period 2")
upPriors[ 3,] <- c( 2, 7,"Infectious period with symptoms, period 3")
upPriors[ 4,] <- c(.3,.7,"Beta")
upPriors[ 5,] <- c(.5,.9,"Epsilon casual partners")
upPriors[ 6,] <- c(.5,.9,"Epsilon steady partners")
upPriors[ 7,] <- c(.1,.9,"Ratio infectivity asymptom/symptom (w)")
upPriors[ 8,] <- c(.3,.6,"Frequency CAI, high activity group (aC3)")
upPriors[ 9,] <- c(52,60,"Day 1st behavioural adaptations (T1)")
upPriors[10,] <- c(70,80,"Day 2nd behavioural adaptations (T2)")
upPriors[11,] <- c( 0,.5,"Reduction casual partners, D1")
upPriors[12,] <- c( 0,.5,"Reduction casual partners, D2")
upPriors <- as.data.frame(upPriors)
colnames(upPriors) <- c("upMin","upMax","nameLong")
upPriors <- mutate(upPriors,
  upMin = as.numeric(upMin),
  upMax = as.numeric(upMax)  )

#save table with unc par values
write.table(upPriors,
  file="param/upPriors.txt",
  sep="\t",
  row.names=FALSE,
  quote=FALSE )

################################## END #########