#
#  powerAnalysisForADualRandomization
#
#
#   turnoutModel is described in the call to lm() 
#      
#
#  partisanship model:
#      Rs vote R 
#      Ds vote D
#      all unaffiliated voters at whatever rate it takes in that election to 
#      cause the 2020 numbers to work out. 
#
#  double check:
#      How do the projected 2021 results look?

rm( list=ls())
library(dplyr)
library(tictoc)
library(magrittr)
source("directory_path.R")
source("sheet_ops_nice.R")
source("countBallots.R")

tic()
load(file="votersInTargetPrecincts.rds",verbose=TRUE)
toc()
tic()
experimentPrecinctsSS = "https://docs.google.com/spreadsheets/d/1ip2qjg0-wYb_4I5sq9rhznUlKilcFu_hasWmEo0_SPI/edit#gid=137738856"
experimentPrecinctsSheet = "allInfoForRandomization"

experimentalPrecints = read_sheet_nice(ss=experimentPrecinctsSS, sheet=experimentPrecinctsSheet)
oh15votersInTargetPrecincts = votersInTargetPrecincts %>% filter( CONGRESSIONAL_DISTRICT == 15 )
toc()


oh15votersInTargetPrecincts$DemVoter = oh15votersInTargetPrecincts$PARTY_AFFILIATION=="D" |
  oh15votersInTargetPrecincts$PARTY_AFFILIATION=="G"
oh15votersInTargetPrecincts$RepVoter = oh15votersInTargetPrecincts$PARTY_AFFILIATION=="R"

oh15votersInTargetPrecincts$voteScore = oh15votersInTargetPrecincts$DemVoter - oh15votersInTargetPrecincts$RepVoter


partyVotes = group_by( oh15votersInTargetPrecincts %>% filter( voted2020), PRECINCT_NAME  ) %>% 
  summarise( DemVoter=sum(DemVoter), RepVoter = sum(RepVoter), allVoters=n() )


partyAndExperimental = merge(partyVotes, experimentalPrecints, by="PRECINCT_NAME")

partyAndExperimental$percentDD    = partyAndExperimental$DemVoter /   
  partyAndExperimental$allVoters  


partyAndExperimental$percentRR    = partyAndExperimental$RepVoter /   
  partyAndExperimental$allVoters  


partyAndExperimental$percentUnaffiliatedVote = 1 - partyAndExperimental$percentDD - partyAndExperimental$percentRR

partyAndExperimental$percentUnaffiliatedDemVote = partyAndExperimental$`Percent D 2020` - 
  partyAndExperimental$percentDD 

partyAndExperimental$percentUnaffiliatedVoteD = partyAndExperimental$percentUnaffiliatedDemVote / 
  partyAndExperimental$percentUnaffiliatedVote

oh15votersInTargetPrecinctsSansGrandA = oh15votersInTargetPrecincts %>% 
  filter( PRECINCT_NAME != "GRANDVIEW-A")


# browser()
oh15votersForPowerTest = merge( oh15votersInTargetPrecinctsSansGrandA, 
                                partyAndExperimental[,c("PRECINCT_NAME" ,
                                                        "percentUnaffiliatedVoteD", 
                                                        "Percent D 2020"  )], 
                                by="PRECINCT_NAME")



# 
# alotOfCounties2020 = read.csv(file=file.path("/Users/kenstanley/Downloads/13AUG2020",
#                                              "SWVF_23_44.txt"),stringsAsFactors =  FALSE )
# 
# 
# franklinCounty2020 = alotOfCounties2020 %>% filter( CONGRESSIONAL_DISTRICT == 15 )
# 
# save(franklinCounty2020,file="franklinCounty2020.rds")

load(file="franklinCounty2020.rds", verbose=TRUE )


OH15voters2020 = merge( oh15votersForPowerTest, experimentalPrecints[,c("PRECINCT_NAME","2020 Voters")], 
                        by="PRECINCT_NAME")

OH15voters2020$previousPresidential = OH15voters2020$GENERAL.11.08.2016 == "X"
OH15voters2020$previousGubernatorial = OH15voters2020$GENERAL.11.06.2018 == "X"
OH15voters2020$previousLocal = OH15voters2020$GENERAL.11.07.2017 == "X"

allColNames = colnames(OH15voters2020)[96:107]
allPrimaries = allColNames[which(grepl("PRIMA",allColNames))] # 4 years of primaries 
allGenerals = allColNames[which(grepl("GENERAL",allColNames))] # 4 years of primaries 

recentColNames = allColNames[8:12] # two years worth of primaries 
recentPrimaries = recentColNames[which(grepl("PRIMA",recentColNames))]


OH15voters2020$generalVotes = countBallots(OH15voters2020[,c(allGenerals)],"X") 

OH15voters2020$primaryVotes = countBallots(OH15voters2020[,c(allPrimaries)],"X") + 
  countBallots(OH15voters2020[,c(allPrimaries)],"D") + 
  countBallots(OH15voters2020[,c(allPrimaries)],"R")

OH15voters2020$recentPrimaryVotes = countBallots(OH15voters2020[,c(recentPrimaries)],"X") + 
  countBallots(OH15voters2020[,c(recentPrimaries)],"D") + 
  countBallots(OH15voters2020[,c(recentPrimaries)],"R")

OH15voters2020$recentLocalVote = OH15voters2020$GENERAL.11.05.2019 == "X"

localVoteAsLMofPreviousVotes = lm(data=OH15voters2020, formula = recentLocalVote ~
                                    previousLocal + # previousPresidential + previousGubernatorial +
                                    recentPrimaryVotes + primaryVotes + generalVotes + PRECINCT_NAME )


summary( localVoteAsLMofPreviousVotes)











oh15votersForPowerTest$previousPresidential = oh15votersForPowerTest$GENERAL.11.03.2020 == "X"
oh15votersForPowerTest$previousGubernatorial = oh15votersForPowerTest$GENERAL.11.06.2018 == "X"
oh15votersForPowerTest$previousLocal = oh15votersForPowerTest$GENERAL.11.05.2019 == "X"

allColNames = colnames(oh15votersForPowerTest)[103:112]
allPrimaries = allColNames[which(grepl("PRIMA",allColNames))] # 4 years of primaries 
allGenerals = allColNames[which(grepl("GENERAL",allColNames))] # 4 years of primaries 

recentColNames = allColNames[8:12] # two years worth of primaries 
recentPrimaries = recentColNames[which(grepl("PRIMA",recentColNames))]


oh15votersForPowerTest$generalVotes = countBallots(oh15votersForPowerTest[,c(allGenerals)],"X") 

oh15votersForPowerTest$primaryVotes = countBallots(oh15votersForPowerTest[,c(allPrimaries)],"X") + 
  countBallots(oh15votersForPowerTest[,c(allPrimaries)],"D") + 
  countBallots(oh15votersForPowerTest[,c(allPrimaries)],"R")

oh15votersForPowerTest$recentPrimaryVotes = countBallots(oh15votersForPowerTest[,c(recentPrimaries)],"X") + 
  countBallots(oh15votersForPowerTest[,c(recentPrimaries)],"D") + 
  countBallots(oh15votersForPowerTest[,c(recentPrimaries)],"R")


# oh15votersForPowerTest$recentLocalVote = oh15votersForPowerTest$GENERAL.11.05.2019 == "X"

oh15votersForPowerTest$predicted2021turnout = predict.lm( localVoteAsLMofPreviousVotes, newdata=oh15votersForPowerTest)

#
#  Now let's add predicted R vs D vote 
#  We need to know by precinct, what percent of the undecideds will vote 
#  Democratic 
# 

oh15votersForPowerTest$MyWard = substr( oh15votersForPowerTest$PRECINCT_NAME, 1, 7)
oh15votersForPowerTest$voted2017 = ( oh15votersForPowerTest$GENERAL.11.07.2017 == "X" )
oh15votersForPowerTest$voted2018 = ( oh15votersForPowerTest$GENERAL.11.06.2018 == "X" )
oh15votersForPowerTest$voted2019 = ( oh15votersForPowerTest$GENERAL.11.05.2019 == "X" )
oh15votersForPowerTest$voted2020 = ( oh15votersForPowerTest$GENERAL.11.03.2020 == "X" )
oh15votersForPowerTest$voted2021 = ( (nchar(oh15votersForPowerTest$PRIMARY.05.04.2021) > 0)  |
                                  (nchar(oh15votersForPowerTest$PRIMARY.08.03.2021) > 0 ) )


oh15votersForPowerTest$anyVoter = oh15votersForPowerTest$voted2020 |
  oh15votersForPowerTest$voted2021 | oh15votersForPowerTest$newRegs


oh15votersForPowerTest$alwaysVoter = oh15votersForPowerTest$voted2021 |
  ( oh15votersForPowerTest$voted2017 & oh15votersForPowerTest$voted2019 & oh15votersForPowerTest$voted2020 )


Nov1_2020 = as.Date("2020-11-01")
oh15votersForPowerTest$newRegs  = as.Date(oh15votersForPowerTest$REGISTRATION_DATE) > Nov1_2020

oh15votersForPowerTest$occasionalVote = oh15votersForPowerTest$newRegs |
  oh15votersForPowerTest$voted2020 & (!oh15votersForPowerTest$alwaysVoter)

oh15votersForPowerTest$voteScore = oh15votersForPowerTest$DemVoter - oh15votersForPowerTest$RepVoter

# browser()

colsWeNeed = c("PRECINCT_NAME" ,   "SOS_VOTERID" , "LAST_NAME"  ,   "FIRST_NAME"  , 
               "MIDDLE_NAME" ,"SUFFIX" ,"DATE_OF_BIRTH"  , "REGISTRATION_DATE"   , 
               "household"    , "RESIDENTIAL_ADDRESS1"  ,  "RESIDENTIAL_SECONDARY_ADDR"   ,
               "mappableAddress"   ,"DemVoter" , "RepVoter" ,  
               "predicted2021turnout"  ,   "percentUnaffiliatedVoteD" , 
               "MyWard", "voted2017", "voted2018", "voted2019", 
               "voted2020", "voted2021" , "anyVoter", "CONGRESSIONAL_DISTRICT",
               "alwaysVoter", "occasionalVote", "voteScore", "generalVotes" ,
               "primaryVotes" , "newRegs")

oh15ForPowerTest = oh15votersForPowerTest[ ,colsWeNeed]

set.seed(1234)
oh15ForPowerTest$turnoutRand = runif(nrow(oh15ForPowerTest))
oh15ForPowerTest$partisanshipRand = runif(nrow(oh15ForPowerTest))
oh15ForPowerTest$demVote = oh15ForPowerTest$partisanshipRand < oh15ForPowerTest$percentUnaffiliatedVoteD
oh15ForPowerTest$demVote[which(oh15ForPowerTest$DemVoter)] = 1 
oh15ForPowerTest$demVote[which(oh15ForPowerTest$RepVoter)] = 0 
oh15ForPowerTest$turnout  = oh15ForPowerTest$turnoutRand < oh15ForPowerTest$predicted2021turnout



turnoutByPrec = group_by( oh15ForPowerTest, PRECINCT_NAME ) %>% 
  summarise( expectedTurnout = sum( turnout),
             expectedDemVote = sum(turnout * demVote) )

save(oh15ForPowerTest, file="oh15ForPowerTest.rds")
save(experimentalPrecints, file="experimentalPrecints.rds")
