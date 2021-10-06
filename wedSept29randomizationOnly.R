#
#  wedSept29randomization
#

redo= FALSE 
if ( redo ) { rm( list=ls() )}
library(tictoc)
tic()

wedSept29universeSS = "https://docs.google.com/spreadsheets/d/1p8XwpuDOELVMGIBD_qsUZxKEKyDg_RhU1VfS4ouhej0/edit#"
wedSept29universeSheet = 1

if ( redo ) { wedSept29universe = read_sheet_nice(ss=wedSept29universeSS, sheet=wedSept29universeSheet) } 

#
# match this to the by god Voter File 
#

if ( redo ) { 
  load(file="FranklinVoterFile.rds", verbose=TRUE) 
}

wedSept29universeA = wedSept29universe 
# %>%  dplyr::rename( SOS_VOTERID = StateFileID)

wedSept29universeA$SOS_VOTERID = wedSept29universeA$StateFileID


universeNotInVoterFile = anti_join( wedSept29universeA, 
                                    FranklinVoterFile, 
                                    by="SOS_VOTERID")


intersect( colnames(wedSept29universeA),colnames(FranklinVoterFile)  )

wedSept29universeA$TMCstreetNum = as.character(lapply(wedSept29universeA$Address,
                                                       function(x) strsplit(as.character(x)," ")[[1]][1] ) )



universeInVoterFile = merge( wedSept29universeA, 
                             FranklinVoterFile, 
                             by="SOS_VOTERID", all.x=TRUE )



universeInVoterFile$VFstreetNum = as.character(lapply(universeInVoterFile$RESIDENTIAL_ADDRESS1,
                                                     function(x) strsplit(as.character(x)," ")[[1]][1] ) )


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

# View(inEither[,c(colsToView, "FIRST_NAME", "LAST_NAME", "household", "inKen", "inEmily")])

# setdiff( colsToView, colnames( inEither))

comparisonSS = "https://docs.google.com/spreadsheets/d/1oZ5X9wqRI7JV5OsOkKLiG5s7kKpR7E0lbwFvH53YwNo/edit#gid=0"

# write_sheet_nice(inEither[,c(colsToView, "FIRST_NAME", "LAST_NAME", "household", "inKen", "inEmily")],
#                  ss=comparisonSS, sheet="inEither") 

#
#  Here is the randomization 
#
#  First we get the households from Emily
#  Then we eliminate inaccessible households 
#  Second we get order everything by:
#    1) Precinct
#    2) Num voters in this household (1, 2, more than 2)
#    3) meanTurnout amongst the top 3 in the household 
#  Then we break them into groups of size and choose one from each group 
#


load(  file="householdApts.rds", verbose=TRUE)
load( file="allLockedDoors.rds", verbose=TRUE)

universeInVoterFileWithAptBldgs = merge( universeInVoterFile, 
                                         householdApts, 
                                         by=c("PRECINCT_NAME" ,"RESIDENTIAL_ADDRESS1"),
                                         all.x=TRUE)


universeInVoterFileWithAptBldgs$incompleteAddress = 
  ( universeInVoterFileWithAptBldgs$numAptsInBldg > 1 ) & 
  ( nchar( universeInVoterFileWithAptBldgs$RESIDENTIAL_SECONDARY_ADDR) ==0 ) 


sum(universeInVoterFileWithAptBldgs$incompleteAddress,na.rm=TRUE)

universeInVoterFileWithLockedDoors = merge( universeInVoterFileWithAptBldgs, 
                                            allLockedDoors[,c("PRECINCT_NAME" ,  "RESIDENTIAL_ADDRESS1",  "inaccessible"  )], 
                                            by=c("PRECINCT_NAME" ,  "RESIDENTIAL_ADDRESS1"), all.x=TRUE  )

# dim(universeInVoterFileWithLockedDoors)
sum( universeInVoterFileWithLockedDoors$inaccessible=="l",na.rm=TRUE )
# intersect( colnames(universeInVoterFileWithAptBldgs), colnames(allLockedDoors))

universeInVoterFileWithLockedDoors$accessible = is.na(universeInVoterFileWithLockedDoors$inaccessible)

sum(universeInVoterFileWithLockedDoors$accessible)

universeInVoterFileWithLockedDoors$incompleteAddress[ 
  is.na(universeInVoterFileWithLockedDoors$incompleteAddress)] = FALSE 


universeInVoterFileWithLockedDoors$incompleteAddress[ 
  is.na(universeInVoterFileWithLockedDoors$incompleteAddress)] = FALSE 


universeInVoterFileWithLockedDoors$household = paste( universeInVoterFileWithLockedDoors$RESIDENTIAL_ADDRESS1,  universeInVoterFileWithLockedDoors$RESIDENTIAL_SECONDARY_ADDR)

sum( universeInVoterFileWithLockedDoors$VFstreetNum == universeInVoterFileWithLockedDoors$TMCstreetNum)

# View( universeInVoterFileWithLockedDoors[which( universeInVoterFileWithLockedDoors$VFstreetNum != 
#                                              universeInVoterFileWithLockedDoors$TMCstreetNum), 
#       c("Address", "RESIDENTIAL_ADDRESS1")])

universeInVoterFileWithLockedDoors$movedSinceMar = 
  universeInVoterFileWithLockedDoors$VFstreetNum != 
  universeInVoterFileWithLockedDoors$TMCstreetNum



universeInVoterFileTreatable = universeInVoterFileWithLockedDoors %>% 
  filter( accessible & !incompleteAddress & !movedSinceMar )

# 
# Bogus sum( universeInVoterFileTreatable == universeInVoterFileTreatable$RESIDENTIAL_ADDRESS1, na.rm=TRUE )
# View( universeInVoterFileTreatable[which(universeInVoterFileTreatable$Address != universeInVoterFileTreatable$RESIDENTIAL_ADDRESS1),
#                                     c("address", "RESIDENTIAL_ADDRESS1")])

#              maxHouseTurnout=max(Turnout), 


householdsInUniverse = group_by(universeInVoterFileTreatable, PRECINCT_NAME, 
                                household) %>% summarise( numVotersInHousehold=n(), 
                                                          turnout=sum(Turnout)+max(Turnout) , 
                                                          maxHousePartisanship=max(Partisanship), 
                                                          meanHousePartisanship=mean(Partisanship), .groups = "drop")  

# load( file="experimentalPrecints.rds", verbose = TRUE )

precinctOrder = experimentalPrecints[ c( "PRECINCT_NAME" , "Order" )] %>% 
  dplyr::rename( precOrder = Order )

# 
# universeInVoterFileTreatableExprPrec = merge( universeInVoterFileTreatable, 
#                                                experimentalPrecints[ c( "PRECINCT_NAME" , "Order" )] ) %>% 
#                                                  dplyr::rename( precOrderOne = Order )



householdsInUniverseWithPrecOrder = merge( householdsInUniverse, precinctOrder, 
                                           by="PRECINCT_NAME" )

#  numVotersInHousehold, 
sortedHouseholds = householdsInUniverseWithPrecOrder %>% arrange( PRECINCT_NAME,
                                                                  turnout )

sortedHouseholds$category = "not set" 


for ( precinctIndex in 1:26 ) {
  indices= which( sortedHouseholds$precOrder == precinctIndex )
  
  indicesZeroBased = indices - min(indices)
  stopifnot( max( indices)==(min(indices)+length(indices)-1) )
  
  denominator = 6
  
  numBlocksOfSix = ceiling( length(indices) / denominator )
  offsetWithinBlock = floor( runif(numBlocksOfSix)* denominator  )
  allControlIndices = ((1:numBlocksOfSix)-1)* denominator + offsetWithinBlock  + min(indices)
  
  controlIndices = intersect(indices,  allControlIndices)
  treatmentIndices = setdiff(indices,  allControlIndices)
  sortedHouseholds$category[ controlIndices ] = "inPrecControl"
  sortedHouseholds$category[ treatmentIndices ] = "inPrecTreatment"
  
  
  
}


print( paste( "precinctIndex=",precinctIndex, Sys.time())) 
toc() 


#
#   Now put it back together with the   
#

universeInVoterFile$household = paste(universeInVoterFile$RESIDENTIAL_ADDRESS1, 
                                      universeInVoterFile$RESIDENTIAL_SECONDARY_ADDR)

universeInVoterFileA = merge( universeInVoterFileWithLockedDoors, sortedHouseholds, 
                              all.x=TRUE )

group_by( universeInVoterFileA, category ) %>% summarise( coutn= n() )

group_by( universePrecinctDoubleCheck, category ) %>% summarise( coutn= n() )

categoryByPrecinct = group_by( universeInVoterFileA, PRECINCT_NAME,category )  %>% 
  summarise( count = n() )

# 
# iseveryone in the sameousehold also in the same category  
#
length( which( ( universeInVoterFileA$precOrder <= 6 )& 
                 ( universeInVoterFileA$category=="inPrecTreatment")) ) 


universeInVoterFileA$category[which(universeInVoterFileA$inaccessible=="l")] = "inaccessible"
universeInVoterFileA$category[which(universeInVoterFileA$incompleteAddress)] = "incomplete address"
universeInVoterFileA$category[which(universeInVoterFileA$movedSinceMar)] = "moved since March "
universeInVoterFileA$category[which(is.na(universeInVoterFileA$category))] = "moved since March "


categoryByPrecinct = group_by( universeInVoterFileA %>% filter( category== "inPrecControl"  | category== "inPrecTreatment" )
                               , PRECINCT_NAME,category )  %>% 
  summarise( count = n(), aveTurnout=mean(Turnout), .groups="drop" )


# View( universeInVoterFileA[which( is.na(universeInVoterFileA$category)),])


shareRandomizationWithEmilySS = "https://docs.google.com/spreadsheets/d/1pEInYA4W4yyaqqcMQcnrvxRKlZ3WHdH4HK539nrn4T0/edit#gid=0"
shareRandomizationWithEmilySheet = "OH15 within precinct randomization household universe"

colnamesToSend = c( colnames(wedSept29universe ), "numAptsInBldg"  , "inaccessible"  ,
                    "accessible"  , "movedSinceMar",  "numVotersInHousehold" , "maxHousePartisanship"  ,
                    "meanHousePartisanship"  , "precOrder" ,  "category"   )

browser()

write_sheet_nice( universeInVoterFileA[,colnamesToSend], 
                  ss=shareRandomizationWithEmilySS ,
                  sheet=shareRandomizationWithEmilySheet )


doubleCheckOfficialPrecinctsSS = "https://docs.google.com/spreadsheets/d/1nFBRiqrX38pAA3TrUcfXZKHahmDUb1Jm3G8a768Ha9k/edit#gid=190287690"
doubleCheckOfficialPrecinctsSheet = "Both"

# doubleCheckOfficialPrecincts = read_sheet_nice( ss=doubleCheckOfficialPrecinctsSS,
#                                                 sheet=doubleCheckOfficialPrecinctsSheet)


universePrecinctDoubleCheck = merge( universeInVoterFileA %>% 
                                       filter( category == "inPrecTreatment" | category == "inPrecControl" ), 
                                     doubleCheckOfficialPrecincts, 
                                     by.x=c("PRECINCT_NAME", "precOrder") ,
                                     by.y=c("PRECINCT_NAME", "Order") )


universeInVoterFileB = universeInVoterFileA %>% dplyr::rename( Order = precOrder )

inUniverseNotMatchOfficialPrec = anti_join( universeInVoterFileB,
                                            doubleCheckOfficialPrecincts, 
                                            by=c("PRECINCT_NAME", "Order"))


"Cols 02-F"
Cols02F = universeInVoterFileB %>% filter( PRECINCT_NAME == "COLS 02-F" )

Cols02Fss = "https://docs.google.com/spreadsheets/d/1unNy4m7LfO7obLmvImWhMZW_9i-hX3YQuYHD_SBzmvU/edit#gid=0"

write_sheet_nice( Cols02F, ss=Cols02Fss, sheet="COLS 02-F")

treatmentPrecincts = doubleCheckOfficialPrecincts[ which(doubleCheckOfficialPrecincts$Order <= 13 ) , ]

AllVotersInTreatmentPrecincts = merge( FranklinVoterFile,
                                        treatmentPrecincts, 
                                         by="PRECINCT_NAME")                                                         
                                                         


randomizationSS = "https://docs.google.com/spreadsheets/d/1pEInYA4W4yyaqqcMQcnrvxRKlZ3WHdH4HK539nrn4T0/edit#gid=99147741"
randomizationSheet = "OH15 within precinct randomization household universe"

inPrecinctRandomization = read_sheet_nice( ss=randomizationSS,
                                           sheet=randomizationSheet)

inPrecinctRandomizationSOS_VOTERID =  inPrecinctRandomization %>% dplyr::rename( SOS_VOTERID = StateFileID ) 


AllMissingVotersInTreatmentPrecincts = anti_join( AllVotersInTreatmentPrecincts, 
                                                  inPrecinctRandomizationSOS_VOTERID,  
                                                  by="SOS_VOTERID")


AllMissingVotersInTreatmentPrecinctsSansReps = AllMissingVotersInTreatmentPrecincts %>% 
  filter( PARTY_AFFILIATION != "R" ) # now down to 4971 


AllMissingVotersWhoVotedNov2020 = AllMissingVotersInTreatmentPrecinctsSansReps %>% 
  filter( GENERAL.11.03.2020 == "X")  # now down to 3135 


AllMissingVotersWhoMissedNov17orNov19 = AllMissingVotersWhoVotedNov2020 %>% 
  filter( GENERAL.11.07.2017 != "X" |  GENERAL.11.05.2019 != "X" )  # now down to 2218  





# View( AllMissingVotersWhoMissedNov17orNov19[,c(1:9,106:129)])



allZipcodesInRandomization = group_by( AllVotersInTreatmentPrecincts, 
                                       RESIDENTIAL_ZIP ) %>% summarise( numExpVotersThisZipCode = n() )


allZipcodesInFranklinCounty = group_by( FranklinVoterFile, 
                                       RESIDENTIAL_ZIP ) %>% summarise( numFCVotersThisZipCode = n() )


allVotersAndExpVotersByZip  = merge( allZipcodesInRandomization,allZipcodesInFranklinCounty)

columnsToSendToMelissaForNCOA = c("SOS_VOTERID","PRECINCT_NAME","LAST_NAME","FIRST_NAME","MIDDLE_NAME","SUFFIX",
  "DATE_OF_BIRTH","RESIDENTIAL_ADDRESS1","RESIDENTIAL_SECONDARY_ADDR","RESIDENTIAL_CITY","RESIDENTIAL_STATE","RESIDENTIAL_ZIP" )

write.csv( AllVotersInTreatmentPrecincts[,columnsToSendToMelissaForNCOA], 
           file="KendallStanleyColumbus4Oct2021.csv")



allZipcodesInCOLS02F = group_by( AllVotersInTreatmentPrecincts %>% filter ( PRECINCT_NAME == "COLS 02-F"), 
                                       RESIDENTIAL_ZIP ) %>% summarise( numExpVotersThisZipCode = n() )


                                                         