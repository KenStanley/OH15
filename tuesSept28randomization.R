#
#  tuesSept28randomization
#

redo= FALSE 
if ( redo ) { rm( list=ls() )}

tuesSept28universeSS = "https://docs.google.com/spreadsheets/d/16VqhLgBF0sFHR11pOF4p45BtsYJ0ccCNSHrEB4znaRc/edit#gid=235166215"
tuesSept28universeSheet = "Universeforrandomization20210927-19318072675"

if ( redo ) { tuesSept28universe = read_sheet_nice(ss=tuesSept28universeSS, sheet=tuesSept28universeSheet) } 

#
# match this to the by god Voter File 
#

if ( redo ) { 
  load(file="FranklinVoterFile.rds", verbose=TRUE) 
}

tuesSept28universeA = tuesSept28universe %>% 
  dplyr::rename( SOS_VOTERID = StateFileID)

universeNotInVoterFile = anti_join( tuesSept28universeA, 
                                    FranklinVoterFile, 
                                    by="SOS_VOTERID")


intersect( colnames(tuesSept28universeA),colnames(FranklinVoterFile)  )

universeInVoterFile = merge( tuesSept28universeA, 
                             FranklinVoterFile, 
                             by="SOS_VOTERID")

allPrecincts = group_by( universeInVoterFile, PRECINCT_NAME ) %>% 
  summarise( PrecinctName = first( PrecinctName), numTargetVotersInPrecinct=n() )

activePrecincts = allPrecincts %>% filter( numTargetVotersInPrecinct > 50 )

voterFileInActivePrecincts = merge( FranklinVoterFile, activePrecincts, by="PRECINCT_NAME" ) %>% 
  filter( CONGRESSIONAL_DISTRICT == 15 )


voterFileInActivePrecincts$votedin2020 = voterFileInActivePrecincts$GENERAL.11.03.2020=="X"
voterFileInActivePrecincts$Rep = voterFileInActivePrecincts$PARTY_AFFILIATION=="R" & voterFileInActivePrecincts$votedin2020
voterFileInActivePrecincts$Dem = voterFileInActivePrecincts$PARTY_AFFILIATION=="D" & voterFileInActivePrecincts$votedin2020
voterFileInActivePrecincts$household = paste( voterFileInActivePrecincts$RESIDENTIAL_ADDRESS1,  voterFileInActivePrecincts$RESIDENTIAL_SECONDARY_ADDR)

voterFileInActivePrecincts$demScore = voterFileInActivePrecincts$Dem - voterFileInActivePrecincts$Rep # 1 = Dem,  0 = neither, -1 = Rep

#
#  Eliminate locked doors 
#  Eliminate voters not in OH15 
#

justPredictedTurnout = oh15ForPowerTest[,c("SOS_VOTERID", "predicted2021turnout", "percentUnaffiliatedVoteD")]

#
#
#
voterFileInActivePrecinctsSansTurnoutScores = anti_join( voterFileInActivePrecincts,
                                                         justPredictedTurnout, 
                                                         by="SOS_VOTERID") 

# voterFileInActivePrecinctsSansTurnoutScores are all in OH12 or OH3 
# stopifnot( sum(voterFileInActivePrecinctsSansTurnoutScores$CONGRESSIONAL_DISTRICT == 15) == 0 )
# And since we got rid of the non OH15 voters, voterFileInActivePrecinctsSansTurnoutScores is 
# now empty 

stopifnot( nrow( voterFileInActivePrecinctsSansTurnoutScores)==0)

# 
# load(file="votersInTargetPrecincts.rds",verbose=TRUE)
# 
# votersMissingFrom08D = voterFileInActivePrecincts %>% filter( PRECINCT_NAME == "COLS 08-D" ) %>% 
#   anti_join ( voterFileInActivePrecincts, by="SOS_VOTERID")

#
# restrict our attention to COLS 08-D to see how we would have randomized this.  
#

voterFileInActivePrecsWithTurnoutScores = merge( voterFileInActivePrecincts,
                                                 justPredictedTurnout, 
                                                 by="SOS_VOTERID") 



emilyCols08DSS = "https://docs.google.com/spreadsheets/d/1ltqw-qxLBjMBAnAQQJCvQ4iwjlour4JY6tlnhNFQDt4/edit#gid=274375449"
emilyCols08DSheet = "All registered voters in COLS 8-D"



if ( redo ) { emily08D =   read_sheet_nice( ss=emilyCols08DSS, sheet=emilyCols08DSheet) }

emily08_D = emily08D %>% 
  dplyr::rename( SOS_VOTERID = StateFileID )



voterFileInCols08D = voterFileInActivePrecsWithTurnoutScores %>% filter( PRECINCT_NAME == "COLS 08-D")

inEmilyNotInVFCols08D = anti_join( emily08_D, voterFileInCols08D, by="SOS_VOTERID" )

inVFNotInEmilyCols08D = anti_join( voterFileInCols08D, emily08_D,  by="SOS_VOTERID" )

inVFandEmilyCols08D = merge( voterFileInCols08D, emily08_D,  by="SOS_VOTERID" )

colsToView = c("SOS_VOTERID", "VOTER_STATUS"    ,      "PARTY_AFFILIATION"  , 
               "TSMPrtsn" , "percentUnaffiliatedVoteD", "GenTO" , "predicted2021turnout", "General20" ,"GENERAL.11.03.2020"  ,
               "General19"  , "GENERAL.11.05.2019" , "Primary20Party" , "PRIMARY.03.17.2020"  ,
               "PRIMARY.05.04.2021" , "PRIMARY.08.03.2021" , "meanTSMPrtsn", "maxTSMPrtsn" ) 
# View( inVFNotInEmilyCols08D[,intersect( colnames( inVFNotInEmilyCols08D),colsToView )])



# View(inVFandEmilyCols08D[,colsToView])


householdsInCols08D = group_by( inVFandEmilyCols08D %>% filter( GenTO > 25 ),PRECINCT_NAME, household)  %>% 
  summarise(  demScore = mean(demScore), meanTSMPrtsn=mean(TSMPrtsn), 
              maxTSMPrtsn = max(TSMPrtsn), .groups="drop")

#
#  At least one member in the household would be a target per Emily, 
#    average of all members is at least 5/8 Dem 
#  This is motivated by the idea that if the voter we want to target is 
#  has a score of exactly 75 we would not want to knock on that door
#  if the other person living at that house has a partisanship score of 
#  less than 50 
#
demHouseholdsInActivePrecincts = 
  householdsInActivePrecincts %>% filter( maxTSMPrtsn > .75 &  
                                            meanTSMPrtsn > .625 )


demHouseholdsInActivePrecincts$demHousehold = TRUE 

kenTargetsInCols08D = merge(inVFandEmilyCols08D, 
                         demHouseholdsInActivePrecincts, 
                         by="household", all.x=TRUE)

kenTargetsInCols08D$demHousehold[is.na(kenTargetsInCols08D$demHousehold)] = FALSE 

#
#  OK now let's turn our attention to what Emily sent last night. 
#

emilyCols08DtargetsInVoterFile = universeInVoterFile %>% filter ( PRECINCT_NAME == "COLS 08-D")

inMineNotEmilys = anti_join( kenTargetsInCols08D,
                             emilyCols08DtargetsInVoterFile,
                             by="SOS_VOTERID"
                             )


inEmilysNotMine = anti_join( emilyCols08DtargetsInVoterFile,
                             kenTargetsInCols08D,
                             by="SOS_VOTERID" )

kenTargetsInCols08D$inKen = TRUE 
emilyCols08DtargetsInVoterFile$inEmily = TRUE 

inEither = merge( kenTargetsInCols08D, 
                  emilyCols08DtargetsInVoterFile, all=TRUE )

inEither$inEmily[is.na(inEither$inEmily)] = FALSE
inEither$inKen[is.na(inEither$inKen)] = FALSE


inEither$household = paste( inEither$RESIDENTIAL_ADDRESS1,  inEither$RESIDENTIAL_SECONDARY_ADDR)

# sum( inEither$householdNew != inEither$household, na.rm=TRUE )

View(inEither[,c(colsToView, "FIRST_NAME", "LAST_NAME", "household", "inKen", "inEmily")])

setdiff( colsToView, colnames( inEither))

comparisonSS = "https://docs.google.com/spreadsheets/d/1oZ5X9wqRI7JV5OsOkKLiG5s7kKpR7E0lbwFvH53YwNo/edit#gid=0"

write_sheet_nice(inEither[,c(colsToView, "FIRST_NAME", "LAST_NAME", "household", "inKen", "inEmily")],
                 ss=comparisonSS, sheet="inEither") 

