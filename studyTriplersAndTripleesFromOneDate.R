#
# studyTriplersAndTripleesFromOneData
#

TriplersSS =  "https://docs.google.com/spreadsheets/d/1v48k6F5a_FurPG1sP81u0-AhSud6t47QCy4uvUin_NA/edit#gid=0"
thisDateSheet= "VoteTriplers matched to voter database" # "June 19th"

triplers = read_sheet_nice( ss=TriplersSS, sheet=thisDateSheet, skip=0)

triplers %<>% filter( !is.na(TriplerID))

colnames(triplers)

load(file="FranklinVoterFile.rds",verbose=TRUE)

triplerVoterFile = merge(triplers[,c("TriplerSosId","Tripler.in.voter.database",
                                     "Triplee1ID","Triplee2ID","Triplee3ID")], 
                         FranklinVoterFile, by.x="TriplerSosId", by.y="SOS_VOTERID")



triplerVoterFile$MyWard = substr( triplerVoterFile$PRECINCT_NAME, 1, 4)
triplerVoterFile$voted2017 = ( triplerVoterFile$GENERAL.11.07.2017 == "X" ) 
triplerVoterFile$voted2018 = ( triplerVoterFile$GENERAL.11.06.2018 == "X" ) 
triplerVoterFile$voted2019 = ( triplerVoterFile$GENERAL.11.05.2019 == "X" ) 
triplerVoterFile$voted2020 = ( triplerVoterFile$GENERAL.11.03.2020 == "X" ) 

triplerVoterFile$alwaysVoters = triplerVoterFile$voted2017 * triplerVoterFile$voted2018 * triplerVoterFile$voted2019  * triplerVoterFile$voted2020




FranklinVoterFile$voted2017 = ( FranklinVoterFile$GENERAL.11.07.2017 == "X" ) 
FranklinVoterFile$voted2018 = ( FranklinVoterFile$GENERAL.11.06.2018 == "X" ) 
FranklinVoterFile$voted2019 = ( FranklinVoterFile$GENERAL.11.05.2019 == "X" ) 
FranklinVoterFile$voted2020 = ( FranklinVoterFile$GENERAL.11.03.2020 == "X" ) 

FranklinVoterFile$alwaysVoters = FranklinVoterFile$voted2017 * FranklinVoterFile$voted2018 * FranklinVoterFile$voted2019  * FranklinVoterFile$voted2020

# 
# OH15only$NameAgeAddress = paste(proper(OH15only$FIRST_NAME),
#                                 " ",proper(OH15only$MIDDLE_NAME),
#                                 " ",proper(OH15only$LAST_NAME),
#                                 " ",proper(OH15only$SUFFIX),
#                                 " [",OH15only$age,"] ",proper(OH15only$RESIDENTIAL_ADDRESS1),
#                                 " ",proper(OH15only$RESIDENTIAL_SECONDARY_ADDR), sep="" )



triplee1s = merge(triplerVoterFile [,c("alwaysVoters","Tripler.in.voter.database", "Triplee1ID")],
                  FranklinVoterFile[,c("SOS_VOTERID","voted2017","voted2019")], 
                  by.x="Triplee1ID",
                  by.y="SOS_VOTERID")


triplee2s = merge(triplerVoterFile [,c("alwaysVoters","Tripler.in.voter.database", "Triplee2ID")],
                  FranklinVoterFile[,c("SOS_VOTERID","voted2017","voted2019")], 
                  by.x="Triplee2ID",
                  by.y="SOS_VOTERID")


triplee3s = merge(triplerVoterFile [,c("alwaysVoters","Tripler.in.voter.database", "Triplee3ID")],
                  FranklinVoterFile[,c("SOS_VOTERID","voted2017","voted2019")], 
                  by.x="Triplee3ID",
                  by.y="SOS_VOTERID")


