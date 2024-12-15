##########################################################
#
# convert Excel file with WEEKLY_vaccination_NUMBERS
#      to data frame with DAILY_vaccination_RATES
#
##########################################################

library("readxl")

#-----------------------------

dataVacPerWeek <- read_excel("data/dataVacPerWeek_HYPOTH2024.xlsx")
dataVacPerWeek <- as.data.frame(dataVacPerWeek)
nweeks <- nrow(dataVacPerWeek) # number of rows = number of weeks in the data 

distrVac <- c(0.2,0.4,0.4) #fraction of vaccinations in each sex.act.group

MS <- dataVacPerWeek[rep(1:nweeks, each = 7),]

FirstDosesAG1  <- distrVac[1]*MS$FirstDosesA/7
FirstDosesAG2  <- distrVac[2]*MS$FirstDosesA/7
FirstDosesAG3  <- distrVac[3]*MS$FirstDosesA/7
SecondDosesAG1 <- distrVac[1]*MS$SecondDosesA/7
SecondDosesAG2 <- distrVac[2]*MS$SecondDosesA/7
SecondDosesAG3 <- distrVac[3]*MS$SecondDosesA/7

FirstDosesBG1  <- distrVac[1]*MS$FirstDosesB/7
FirstDosesBG2  <- distrVac[2]*MS$FirstDosesB/7
FirstDosesBG3  <- distrVac[3]*MS$FirstDosesB/7
SecondDosesBG1 <- distrVac[1]*MS$SecondDosesB/7
SecondDosesBG2 <- distrVac[2]*MS$SecondDosesB/7
SecondDosesBG3 <- distrVac[3]*MS$SecondDosesB/7

dataDate <- seq(as.Date('2024-01-01'), as.Date('2025-12-28'), by = "days")

MS <- cbind(MS,dataDate,FirstDosesAG1,FirstDosesAG2,FirstDosesAG3,SecondDosesAG1,SecondDosesAG2,SecondDosesAG3
                       ,FirstDosesBG1,FirstDosesBG2,FirstDosesBG3,SecondDosesBG1,SecondDosesBG2,SecondDosesBG3)

dataVacNumDaily <- as.matrix(MS)

write.table(dataVacNumDaily,
  file="data/dataVacNumDailyMatrix2024.txt",
  sep="\t",
  row.names=FALSE,
  quote=FALSE
)

################################### END ##################