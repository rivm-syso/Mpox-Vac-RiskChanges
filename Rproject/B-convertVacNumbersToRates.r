##########################################################
#
# Convert Excel file with WEEKLY_vaccination_NUMBERS
#      to data frame with DAILY_vaccination_RATES
#
##########################################################

library("readxl")

#-----------------------------
#
dataVacPerWeek <- read_excel("data/dataVacPerWeek.xlsx")
dataVacPerWeek <- as.data.frame(dataVacPerWeek)
nweeks <- nrow(dataVacPerWeek) # number of rows = number of weeks in the data 

distrVac <- c(0.2,0.4,0.4) #fraction of vaccinations in each sex.act.group

MS <- dataVacPerWeek[rep(1:nweeks, each = 7),]

FirstDosesG1  <- distrVac[1]*MS$FirstDoses/7
FirstDosesG2  <- distrVac[2]*MS$FirstDoses/7
FirstDosesG3  <- distrVac[3]*MS$FirstDoses/7
SecondDosesG1 <- distrVac[1]*MS$SecondDoses/7
SecondDosesG2 <- distrVac[2]*MS$SecondDoses/7
SecondDosesG3 <- distrVac[3]*MS$SecondDoses/7
dataDate <- seq(as.Date('2022-04-25'), as.Date('2023-12-31'), by = "days")

MS <- cbind(MS,dataDate,FirstDosesG1,FirstDosesG2,FirstDosesG3,SecondDosesG1,SecondDosesG2,SecondDosesG3)

dataVacNumDaily <- as.matrix(MS)
dataVacNumDaily <- dataVacNumDaily[2:nrow(dataVacNumDaily),]

write.table(dataVacNumDaily,
  file="data/dataVacNumDailyMatrix.txt",
  sep="\t",
  row.names=FALSE,
  quote=FALSE
)

################################### END ##################