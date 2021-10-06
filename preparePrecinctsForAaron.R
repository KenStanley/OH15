#
#  preparePrecinctsForAaron 
#
#  For each precinct, we want to know:
#     How many Russo voters there are in that precinct (i.e. in Russo's precinct)
#     How many always voter households there are in that precinct
#        2017 & 2019 -or- Aug 2021 -or- May 2021 
#     How many target households are there 
#     How many target voters are there in those households 
#     How many inaccessbile voters are there (parital guesswork here)
#
#   Gated community check 
# 


#   update the voted flags

redo = TRUE 
tic()
if ( redo) {
  #  This has not been tested - but shoudl work 
  load(file="FranklinVoterFile.rds",verbose=TRUE)
  
  FranklinVoterFile$MyWard = substr( FranklinVoterFile$PRECINCT_NAME, 1, 7)
  FranklinVoterFile$voted2017 = ( FranklinVoterFile$GENERAL.11.07.2017 == "X" )
  FranklinVoterFile$voted2018 = ( FranklinVoterFile$GENERAL.11.06.2018 == "X" )
  FranklinVoterFile$voted2019 = ( FranklinVoterFile$GENERAL.11.05.2019 == "X" )
  FranklinVoterFile$voted2020 = ( FranklinVoterFile$GENERAL.11.03.2020 == "X" )
  FranklinVoterFile$voted2021 = ( (nchar(FranklinVoterFile$PRIMARY.05.04.2021) > 0)  | 
                                    (nchar(FranklinVoterFile$PRIMARY.08.03.2021) > 0 ) )
  
  FranklinVoterFile$household = paste( FranklinVoterFile$RESIDENTIAL_ADDRESS1 , FranklinVoterFile$RESIDENTIAL_SECONDARY_ADDR )
  
  
  FranklinVoterFile$alwaysVoter = FranklinVoterFile$voted2021 | 
    ( FranklinVoterFile$voted2017 & FranklinVoterFile$voted2019 & FranklinVoterFile$voted2020 )
  
  
  Nov1_2020 = as.Date("2020-11-01")
  FranklinVoterFile$newRegs  = as.Date(FranklinVoterFile$REGISTRATION_DATE) > Nov1_2020
  
  FranklinVoterFile$occasionalVote = FranklinVoterFile$newRegs | 
    FranklinVoterFile$voted2020 & (!FranklinVoterFile$alwaysVoter)
  
  
  targetPrecinctSS = "https://docs.google.com/spreadsheets/d/1-m5VsqfXOS0Ox1rdC6UsL9pLMuCFDcdLipP22sdelTI/edit#gid=1366853863"
  targetPrecinctSheet = "TargetPrecincts"
  
  allPotentialTargetPrecincts = read_sheet_nice(ss=targetPrecinctSS, sheet=targetPrecinctSheet)
  
  targetPrecincts = allPotentialTargetPrecincts %>% filter(accept==1)
  
  votersInTargetPrecincts = merge( FranklinVoterFile,targetPrecincts[,c("PRECINCT_NAME","accept"), 
                                                                     by="PRECINCT_NAME"] )
  
  columbusVoters = FranklinVoterFile[ which( grepl("COLS",FranklinVoterFile$PRECINCT_NAME)),]
  
  browser()
  
  save( votersInTargetPrecincts, file="votersInTargetPrecincts.rds")
}
toc()
load(file="votersInTargetPrecincts.rds",verbose=TRUE)

precinctsWithRusso = group_by( votersInTargetPrecincts, PRECINCT_NAME, STATE_REPRESENTATIVE_DISTRICT ) %>% summarise( count=n())

#
#   There is a mystery here that I don't have time to chase down - why are there 
#   Columbus precincts in allPrecinctsWithRusso that aren't in ColPrecinctsWithRusso
# 
ColPrecinctsWithRusso = group_by( columbusVoters, PRECINCT_NAME, CONGRESSIONAL_DISTRICT, STATE_REPRESENTATIVE_DISTRICT ) %>% summarise( count=n())
allPrecinctsWithRusso = group_by( FranklinVoterFile, PRECINCT_NAME, CONGRESSIONAL_DISTRICT, STATE_REPRESENTATIVE_DISTRICT ) %>% summarise( count=n() ) 

oh15votersInTargetPrecincts = votersInTargetPrecincts %>% filter( CONGRESSIONAL_DISTRICT == 15 )


oh15votersInTargetPrecincts$DemVoter = oh15votersInTargetPrecincts$PARTY_AFFILIATION=="D" |
  oh15votersInTargetPrecincts$PARTY_AFFILIATION=="G"
oh15votersInTargetPrecincts$RepVoter = oh15votersInTargetPrecincts$PARTY_AFFILIATION=="R"

oh15votersInTargetPrecincts$voteScore = oh15votersInTargetPrecincts$DemVoter - oh15votersInTargetPrecincts$RepVoter

group_by(oh15votersInTargetPrecincts, voteScore) %>% summarise(count=n())

oh15votersInTargetPrecincts$anyVoter = oh15votersInTargetPrecincts$voted2020 |
  oh15votersInTargetPrecincts$voted2021 | oh15votersInTargetPrecincts$newRegs

householdsInTargetPrecincts = group_by( oh15votersInTargetPrecincts %>% filter(anyVoter), 
                                        PRECINCT_NAME, CONGRESSIONAL_DISTRICT, 
                                        household ) %>% 
  summarise(  count=n(), alwaysHouse=mean(alwaysVoter), occHouse=sum(occasionalVote), 
              anyVoter=sum(anyVoter),
              voted2018=sum(voted2018),
              voted2019=sum(voted2019),
              voteScore=mean(voteScore) )

leanDemHousehodlsInTargPrec =householdsInTargetPrecincts %>% filter( voteScore >= 0 )

leanDemHousehodlsInTargPrec$roundedOccHouse = round(leanDemHousehodlsInTargPrec$occHouse*4)/4 
leanDemHousehodlsInTargPrec$roundedOccHouse = ( leanDemHousehodlsInTargPrec$occHouse > 0 ) + 0
leanDemHousehodlsInTargPrec$roundedVoteScore = round(leanDemHousehodlsInTargPrec$voteScore*2)/2 
leanDemHousehodlsInTargPrec$roundedAlwaysHouse= round(leanDemHousehodlsInTargPrec$alwaysHouse*3)/3


leanDemHousehodlsInTargPrec$alwaysHousehold = (leanDemHousehodlsInTargPrec$alwaysHouse == 1) &
  ( leanDemHousehodlsInTargPrec$occHouse == 0 )

# leanDemHousehodlsInTargPrec$occHouse = leanDemHousehodlsInTargPrec$occHouse > 0 

try = group_by( leanDemHousehodlsInTargPrec, roundedAlwaysHouse, roundedVoteScore,roundedOccHouse ) %>% 
  summarise(count=n())

precinctInfoII = group_by( leanDemHousehodlsInTargPrec, PRECINCT_NAME, CONGRESSIONAL_DISTRICT, 
                         ) %>% summarise( alwaysHousehold=sum(alwaysHousehold), 
                                          occVoters=sum(occHouse),  
                                          voted2019=sum(voted2019),  
                                          voted2018=sum(voted2018),  
                                          anyVoters=sum(anyVoter),
                                          occHouse=sum(roundedOccHouse) )



targetPrecinctsWithHouseholdInfoII = merge(precinctInfoII, targetPrecincts[,c("PRECINCT_NAME" , "mappableAddress",  "fractionInaccessible" ) ], 
                                        by="PRECINCT_NAME")

browser()
write_sheet_nice(targetPrecinctsWithHouseholdInfoII, ss=targetPrecinctSS, sheet="allInfoForRandomizationB")
