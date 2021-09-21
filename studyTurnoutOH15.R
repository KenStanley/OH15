#
# studyPriorityPrecincts 
#
rm(list=ls())
source("directory_path.R")
library(dplyr)
library(tictoc)
library(magrittr)

source("sheet_ops_nice.R")
source("countBallots.R")
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

load( file="OH15only.rds", verbose=TRUE)

unique( OH15only$CITY )


OH15only$MyWard = substr( OH15only$PRECINCT_NAME, 1, 4)
OH15only$voted2017 = ( OH15only$GENERAL.11.07.2017 == "X" ) 
OH15only$voted2018 = ( OH15only$GENERAL.11.06.2018 == "X" ) 
OH15only$voted2019 = ( OH15only$GENERAL.11.05.2019 == "X" ) 

OH15only$consistentVoters = OH15only$voted2019 * OH15only$voted2018 * 
  OH15only$voted2017 * OH15only$voted2020

franklinCountyPrecinctsSS = "https://docs.google.com/spreadsheets/d/1-m5VsqfXOS0Ox1rdC6UsL9pLMuCFDcdLipP22sdelTI/edit#gid=1301414353"
allMappablePrecinctsSheet = "allMappablePrecincts"

priorityPrecincts = read_sheet_nice( ss=franklinCountyPrecinctsSS, sheet=allMappablePrecinctsSheet )

priorityPrecinctsTrue = priorityPrecincts %>% filter( Experiment > 0 )

OH15priorityOnly = merge( OH15only,priorityPrecinctsTrue[,c("PRECINCT_NAME", "percentDem2020" )], by="PRECINCT_NAME" )


turnoutByWard =  group_by( OH15priorityOnly , MyWard ) %>% summarise( count=n(), 
                                                                      voted2020=sum(voted2020), 
                                                                      voted2019=sum(voted2019), 
                                                                      voted2018=sum(voted2018), 
                                                                      voted2017=sum(voted2017),
                                                                      consistentVoters=sum(consistentVoters))



turnoutByHousehold =  group_by( OH15priorityOnly %>% 
                                  filter (voted2020), MyWard, household ) %>% 
  summarise( count=n(), 
             voted2019=sum(voted2019), voted2020=sum(voted2020),
             consistentVoters=mean(consistentVoters))


turnoutByHouseholdAll =  group_by( OH15priorityOnly , household ) %>% summarise( count=n(),
                                                                                 consistentVoters=mean(consistentVoters))

householdAllReport = group_by(turnoutByHouseholdAll, consistentVoters ) %>% summarise(count=n())

#
#  What I want to do here is figure out what percent of the voters who voted in 2020 
#  but are not consistent voters voted in 2019 by household 
#
#
#

turnoutByHousehold$consistentVoters = round(turnoutByHousehold$consistentVoters*5)/5
turnoutByHousehold$superConsistentVoters = turnoutByHousehold$consistentVoters == 1 
householdReport = group_by(turnoutByHousehold, MyWard, superConsistentVoters ) %>% summarise(count=n(), 
                                                                                             voted2019=sum(voted2019), voted2020=sum(voted2020))


turnoutByPrecinct =  group_by( OH15priorityOnly , PRECINCT_NAME ) %>% summarise( count=n(), 
                                                                                 voted2020=sum(voted2020), 
                                                                                 voted2019=sum(voted2019), 
                                                                                 voted2018=sum(voted2018), 
                                                                                 voted2017=sum(voted2017))


turnoutByWard$turnout17_20 = turnoutByWard$voted2017 / turnoutByWard$voted2020
turnoutByWard$turnout18_20 = turnoutByWard$voted2018 / turnoutByWard$voted2020
turnoutByWard$turnout19_20 = turnoutByWard$voted2019 / turnoutByWard$voted2020
turnoutByWard$turnout19_18 = turnoutByWard$voted2019 / turnoutByWard$voted2018

sum(turnoutByWard$voted2017) / sum(turnoutByWard$voted2020)
sum(turnoutByWard$voted2018) / sum(turnoutByWard$voted2020)
sum(turnoutByWard$voted2019) / sum(turnoutByWard$voted2020)
sum(turnoutByWard$voted2019) / sum(turnoutByWard$voted2018)






turnoutByAllWards =  group_by( OH15only , MyWard ) %>% summarise( count=n(), 
                                                                  voted2020=sum(voted2020), 
                                                                  voted2019=sum(voted2019), 
                                                                  voted2018=sum(voted2018), 
                                                                  voted2017=sum(voted2017))



turnoutByAllWards$turnout17_20 = turnoutByAllWards$voted2017 / turnoutByAllWards$voted2020
turnoutByAllWards$turnout18_20 = turnoutByAllWards$voted2018 / turnoutByAllWards$voted2020
turnoutByAllWards$turnout19_20 = turnoutByAllWards$voted2019 / turnoutByAllWards$voted2020

sum(turnoutByAllWards$voted2017) / sum(turnoutByAllWards$voted2020)
sum(turnoutByAllWards$voted2018) / sum(turnoutByAllWards$voted2020)
sum(turnoutByAllWards$voted2019) / sum(turnoutByAllWards$voted2020)
sum(turnoutByAllWards$voted2019) / sum(turnoutByAllWards$voted2018)

