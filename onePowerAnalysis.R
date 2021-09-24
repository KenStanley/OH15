#
#  onePowerAnalysis   
#
#  OH15withBothAssigns has all the data for a single randomization into 
#  treatment and control precincts    
#

tic()

CLEANUP = TRUE 
if ( CLEANUP ) { 
  rm( list=ls())
  franklinCountyPrecinctsSS = "https://docs.google.com/spreadsheets/d/1-m5VsqfXOS0Ox1rdC6UsL9pLMuCFDcdLipP22sdelTI/edit#gid=1301414353"
  source("sheet_ops_nice.R")
  # load( file="OH15withBothAssigns.rds", verbose=TRUE )
  
  load( file="oh15ForPowerTest.rds", verbose = TRUE )
  load( file="experimentalPrecints.rds", verbose = TRUE )
  if ( 1 ) {
    oh15ForPowerTest$predicted2021turnout[ which(oh15ForPowerTest$newRegs) ] = 
      pmin( 1, oh15ForPowerTest$predicted2021turnout[ which(oh15ForPowerTest$newRegs) ] + 0.1 )
  }
  
  load( file="householdApts.rds",verbose=TRUE)
  load( file="allLockedDoors.rds",verbose=TRUE)
  
  experimentalPrecints %<>%
    dplyr::rename( alwaysVoteHouseholds = `number of households where all voters vote consistently`) %<>%
    dplyr::rename( leanDems = `number of lean dem voters (i.e. voted in 2020 or 2021 or registered since 2020)`) %<>%
    dplyr::rename( leanDemHouseholds = `number of households containing at least one lean Dem occasional voter`) %<>%
    dplyr::rename( sampleAddress = `sample mappable Address (in case you want to see where they are on the map)`)
  
}
inaccessibleAffectSize = .005

accessibleAffectSize = .05 # This should be enough to give us a good shot at a result in within-precinct control 
sansStratification = TRUE 
numCanvasses = 10
numTrials = 20
minimunNumPrecinctsInTreatment = 6
maximunNumPrecinctsInTreatment = 16

meanTrunoutImpact = rep( -13, maximunNumPrecinctsInTreatment)
meanNetDemImpact= rep( -13, maximunNumPrecinctsInTreatment)
turnoutPower= rep( -13, maximunNumPrecinctsInTreatment)
netDemPower= rep( -13, maximunNumPrecinctsInTreatment)
turnoutPreDiff= rep( -13, maximunNumPrecinctsInTreatment)
anyPvalue = rep( -13, maximunNumPrecinctsInTreatment)
addedVotes = rep( -13, maximunNumPrecinctsInTreatment)

for (numPrecinctsInTreatment in minimunNumPrecinctsInTreatment:maximunNumPrecinctsInTreatment) {
  
  
  percentControl = 1/6 
  
  
  turnoutByPrecTop = group_by( oh15ForPowerTest, PRECINCT_NAME ) %>% 
    summarise( expectedTurnout = sum( turnout),
               expectedDemVote = sum(turnout * demVote), .groups = 'drop' )
  
  
  turnoutByPrecTop$fracDem = turnoutByPrecTop$expectedDemVote / turnoutByPrecTop$expectedTurnout
  
  compareExpToActual = merge( turnoutByPrecTop, experimentalPrecints, 
                              by="PRECINCT_NAME")
  
  colsOfInterest = c( "PRECINCT_NAME"  , "expectedTurnout"   ,
                      "expectedDemVote" , "Voted 2019"   , "fracDem", "Percent D 2020"   )
  
  #
  #  Here we give those who have updated their voter registration a bump, 
  #  mainly because those who have just registered may have no voting record
  #  and hence be seen as non-voters 
  #
  
  
  
  householdsInTargetPrecincts = group_by( oh15ForPowerTest , # %>% filter(anyVoter) - leaves these until after the randomization 
                                          PRECINCT_NAME, CONGRESSIONAL_DISTRICT,
                                          household , RESIDENTIAL_ADDRESS1, 
                                          RESIDENTIAL_SECONDARY_ADDR ) %>%
    summarise(  count=n(), alwaysHouse=mean(alwaysVoter), occHouse=sum(occasionalVote),
                anyVoter=sum(anyVoter),
                voted2018=sum(voted2018),
                voted2019=sum(voted2019),
                voted2020=sum(voted2020),
                newRegs=sum(newRegs),
                predicted2021turnout=sum(predicted2021turnout),
                voteScore=mean(voteScore * anyVoter * 10 + voteScore ), .groups = 'drop' )
  
  
  householdsWithIncompleteAddr = merge( householdsInTargetPrecincts, householdApts, 
                                        by=c("PRECINCT_NAME", "RESIDENTIAL_ADDRESS1"), all.x=TRUE)
  
  householdsWithInaccessible = merge( householdsWithIncompleteAddr, allLockedDoors, 
                                      by=c("PRECINCT_NAME", "RESIDENTIAL_ADDRESS1"), all.x=TRUE)
  
  
  householdsWithInaccessible$inaccessible[which(is.na(householdsWithInaccessible$inaccessible))] = "u" 
  
  householdsWithInaccessible$avoid = householdsWithInaccessible$inaccessible=="l"
  householdsWithInaccessible$avoid[which(  householdsWithInaccessible$numAptsInBldg > 1 & 
                                             ( nchar(householdsWithInaccessible$RESIDENTIAL_SECONDARY_ADDR )<2) )] = TRUE 
  
  
  colsToView = c( "PRECINCT_NAME" ,     "RESIDENTIAL_ADDRESS1", "RESIDENTIAL_SECONDARY_ADDR" , 
                  "household"  , "numAptsInBldg"   ,   "inaccessible" ,   "avoid"  )
  
  
  
  householdsWithInaccessible$category = "not set"
  householdsWithInaccessible$category[which( householdsWithInaccessible$alwaysHouse == 1)] = "always"
  householdsWithInaccessible$category[which( householdsWithInaccessible$avoid )] = "inaccessible" # Leave out of the randomization pool     
  householdsWithInaccessible$category[which( householdsWithInaccessible$voteScore < 0 )] = "leanR"
  householdsWithInaccessible$category[which( householdsWithInaccessible$anyVoter == 0  )] = "noVoters"
  
  
  if(sansStratification )  {
    
    householdsWithInaccessible$treatmentRand = runif(nrow( householdsWithInaccessible))
    
    householdsWithInaccessible$category[ which( ( householdsWithInaccessible$category=="not set")
                                                & ( householdsWithInaccessible$treatmentRand < percentControl ) )  ] = "control"
    
    householdsWithInaccessible$category[ which( ( householdsWithInaccessible$category=="not set")
                                                & ( householdsWithInaccessible$treatmentRand >= percentControl ) )  ] = "treatment"
  } else {
    householdsWithInaccessible %<>% arrange( category, PRECINCT_NAME,  predicted2021turnout)
    
    denominator = 6 
    
    numBlocksOfSix = ceiling( nrow( householdsWithInaccessible) / denominator ) 
    offsetWithinBlock = floor( runif(numBlocksOfSix)* denominator  )
    allControlIndices = ((1:numBlocksOfSix)-1)* denominator + offsetWithinBlock
    notSetIndices = which( householdsWithInaccessible$category == "not set")
    
    controlIndices = intersect(notSetIndices,  allControlIndices)
    treatmentIndices = setdiff(notSetIndices,  allControlIndices)
    householdsWithInaccessible$category[ controlIndices ] = "control"
    householdsWithInaccessible$category[ treatmentIndices ] = "treatment"
    
  }
  
  OH15WithHouseholdAssign = merge( oh15ForPowerTest, 
                                   householdsWithInaccessible[,c("PRECINCT_NAME" ,"household" ,"CONGRESSIONAL_DISTRICT",  
                                                                 "numAptsInBldg", "inaccessible" , "avoid" ,
                                                                 "category" )],
                                   by=c("PRECINCT_NAME" ,"household" ,"CONGRESSIONAL_DISTRICT"))
  
  
  
  votersCategoryByPrecinct = group_by( OH15WithHouseholdAssign, PRECINCT_NAME ) %>% 
    summarise( always = sum( category=="always"),
               treatment = sum( category=="treatment"),
               control = sum( category=="control"),
               leanR = sum( category=="leanR"),
               inaccessible = sum( category=="inaccessible"),
               noVoters = sum( category=="noVoters"),
               notSet = sum( category=="not set"), allCategories=n(), .groups = 'drop' )
  
  # categoryByPrecinct$total = rowSums( votersCategoryByPrecinct[,c(2:8)])
  
  votersCategoryAndPrec = group_by( OH15WithHouseholdAssign, PRECINCT_NAME,category ) %>% summarise(count=n(), .groups = 'drop')
  
  
  OH15withBothAssigns  = merge( OH15WithHouseholdAssign, 
                                experimentalPrecints[,c("PRECINCT_NAME"  ,  "Order"  )], 
                                by="PRECINCT_NAME" )
  
  
  
  
  
  OH15withBothAssigns$treatmentPrec = OH15withBothAssigns$Order <= numPrecinctsInTreatment
  
  
  
  turnoutPvalue = rep(-13,numCanvasses)
  turnoutImpact = rep(-13,numCanvasses)
  netDemImpact = rep(-13,numCanvasses)
  netDemPvalue = rep(-13,numCanvasses)
  addedVote = rep(-13,numCanvasses)
  
  for (canvassIndex in 1:numCanvasses ) { 
    # 
    # OH15withBothAssigns$turnoutRand = runif( nrow( OH15withBothAssigns))
    # OH15withBothAssigns$partisanshipRand = runif( nrow( OH15withBothAssigns))
    # 
    OH15withBothAssignsTreatPrecsOnly = OH15withBothAssigns %>% filter( treatmentPrec )
    
    OH15all = OH15withBothAssigns
    
    OH15all$affectedTurnout = OH15all$predicted2021turnout
    
    
    turnoutRandResultsPre = t.test( OH15all$affectedTurnout[which(OH15all$category=="treatment" & OH15all$treatmentPrec)],
                                    OH15all$affectedTurnout[which(OH15all$category=="control"& OH15all$treatmentPrec  )])
    
    
    OH15all$affectedTurnout[which(OH15all$category=="treatment" & OH15all$treatmentPrec)] = 
      OH15all$affectedTurnout[which(OH15all$category=="treatment" & OH15all$treatmentPrec)] + accessibleAffectSize
    
    # 
    turnoutRandResultsPost = t.test( OH15all$affectedTurnout[which(OH15all$category=="treatment" & OH15all$treatmentPrec)],
                                     OH15all$affectedTurnout[which(OH15all$category=="control"& OH15all$treatmentPrec  )])
    
    
    OH15all$percentDem  = OH15all$percentUnaffiliatedVoteD
    OH15all$percentDem[which(OH15all$DemVoter)] = 1 
    OH15all$percentDem[which(OH15all$RepVoter)] = 0 
    
    OH15all$turnoutRand = runif( nrow(OH15all))
    OH15all$partisanshipRand = runif( nrow(OH15all))
    OH15all$voted2021 = OH15all$turnoutRand < OH15all$affectedTurnout
    OH15all$votedDem = OH15all$partisanshipRand < OH15all$percentDem
    
    # voteRelatedCols = c("PRECINCT_NAME"    ,  )  
    
    OH15allPrecinctResults = group_by( OH15all, treatmentPrec, PRECINCT_NAME ) %>% 
      summarise( precVotedIn2021=sum(voted2021), 
                 precDemVoteIn2021=sum( voted2021 * votedDem ), .groups = 'drop' )
    
    
    OH15allPrecinctResults$repVote = OH15allPrecinctResults$precVotedIn2021 - 
      OH15allPrecinctResults$precDemVoteIn2021
    
    OH15allPrecinctResults$netDem = OH15allPrecinctResults$precDemVoteIn2021 - 
      OH15allPrecinctResults$repVote
    
    
    totalVoted2021InExperiment = sum( OH15allPrecinctResults$precDemVoteIn2021 )
    
    if ( "Percent D 2020" %in% colnames( experimentalPrecints)) {
      experimentalPrecints %<>% dplyr::rename( percentD2020 = `Percent D 2020`)
    }
    if ( "2020 Voters" %in% colnames( experimentalPrecints)) {
      experimentalPrecints %<>% dplyr::rename( prec2020vote = `2020 Voters`)
    }
    if ( "Voted 2019" %in% colnames( experimentalPrecints)) {
      experimentalPrecints %<>% dplyr::rename( prec2019vote = `Voted 2019`)
    }
    # totalVoted2020InExperiment = sum(experimentalPrecints$prec2020vote)
    totalVoted2019InExperiment = sum(experimentalPrecints$prec2019vote)
    
    experimentalPrecints$netDemVote2020 = round( (2 * experimentalPrecints$percentD2020  - 1 ) * 
                                                   experimentalPrecints$prec2020vote, 1 ) 
    experimentalPrecints$expected2021NetDemVote = round( experimentalPrecints$netDemVote2020 *
                                                           experimentalPrecints$prec2019vote / experimentalPrecints$prec2020vote * 
                                                           totalVoted2021InExperiment / totalVoted2019InExperiment , 1 ) 
    
    
    OH15allPrecinctResultsVsExpected = merge(OH15allPrecinctResults,
                                             experimentalPrecints[,c("PRECINCT_NAME","expected2021NetDemVote", 
                                                                     "prec2019vote", "prec2020vote","netDemVote2020")],
                                             by="PRECINCT_NAME")
    
    toplineResults = group_by( OH15allPrecinctResultsVsExpected, treatmentPrec ) %>%
      summarise( precNetDem2021=sum(netDem), expected2021NetDemVote=sum(expected2021NetDemVote),
                 precDemVoteIn2021=sum(precDemVoteIn2021), 
                 precVotedIn2021 = sum(precVotedIn2021), .groups = 'drop')
    
    # masterPrecinctInacccesibleListSS = "https://docs.google.com/spreadsheets/d/1ip2qjg0-wYb_4I5sq9rhznUlKilcFu_hasWmEo0_SPI/edit#gid=137738856"
    # # write_sheet_nice(toDisplay, ss=masterPrecinctInacccesibleListSS, sheet="allDoors")
    # 
    
    
    turnoutResults = t.test( OH15all$voted2021[which(OH15all$category=="treatment" & OH15all$treatmentPrec)],
                             OH15all$voted2021[which(OH15all$category=="control"& OH15all$treatmentPrec  )])
    
    treatmentVoteRate = sum(OH15all$voted2021[which(OH15all$category=="treatment" & OH15all$treatmentPrec)]) / 
      sum(OH15all$category=="treatment" & OH15all$treatmentPrec)
    controlVoteRate = sum(OH15all$voted2021[which(OH15all$category=="control" & OH15all$treatmentPrec)]) / 
      sum(OH15all$category=="control" & OH15all$treatmentPrec)
    
    addedVote[canvassIndex] = sum(OH15all$voted2021[which(OH15all$category=="treatment" & OH15all$treatmentPrec)]) -
      sum(OH15all$category=="treatment" & OH15all$treatmentPrec) * controlVoteRate
    
    turnoutImpact[canvassIndex] = turnoutResults$estimate[1] - turnoutResults$estimate[2]
    turnoutPvalue[canvassIndex] = turnoutResults$p.value
    
    overEstimatePerVote = ( sum( OH15allPrecinctResultsVsExpected$netDem ) - 
                              sum( OH15allPrecinctResultsVsExpected$expected2021NetDemVote) ) / 
      sum( OH15allPrecinctResultsVsExpected$precVotedIn2021)
    
    OH15allPrecinctResultsVsExpected$netImprovement = OH15allPrecinctResultsVsExpected$netDem - 
      OH15allPrecinctResultsVsExpected$expected2021NetDemVote - 
      OH15allPrecinctResultsVsExpected$precVotedIn2021 * overEstimatePerVote
    
    
    netImprovementInTreatment = sum( OH15allPrecinctResultsVsExpected$netImprovement[
      which( OH15allPrecinctResultsVsExpected$treatmentPrec)
    ])
    
    
    beatActualTreatment = 0 
    for (experimentNumber in 1:numTrials) { 
      OH15allPrecinctResultsVsExpected$randomTreatOrder = runif( nrow( OH15allPrecinctResultsVsExpected ))
      nthValue = sort( OH15allPrecinctResultsVsExpected$randomTreatOrder)[numPrecinctsInTreatment]
      OH15allPrecinctResultsVsExpected$experimentTreatment = 
        OH15allPrecinctResultsVsExpected$randomTreatOrder <= nthValue
      stopifnot( sum(OH15allPrecinctResultsVsExpected$experimentTreatment) == numPrecinctsInTreatment ) 
      
      
      netImprovementInExperimentTreatment = sum( OH15allPrecinctResultsVsExpected$netImprovement[
        which( OH15allPrecinctResultsVsExpected$experimentTreatment)
      ])
      
      if ( netImprovementInExperimentTreatment >= netImprovementInTreatment ) {
        beatActualTreatment = beatActualTreatment+ 1 
      }
    }
    
    
    netDemImpact[canvassIndex] = netImprovementInTreatment
    netDemPvalue[canvassIndex] =  beatActualTreatment / numTrials
    
    
  }
  
  turnoutPreDiff[numPrecinctsInTreatment] =  turnoutRandResultsPre$estimate[1] - turnoutRandResultsPre$estimate[2]
  
  meanTrunoutImpact[numPrecinctsInTreatment] = mean(turnoutImpact) * sum(OH15all$category=="treatment" & OH15all$treatmentPrec) 
  meanNetDemImpact[numPrecinctsInTreatment] = mean(netDemImpact)
  addedVotes[numPrecinctsInTreatment] = mean(addedVote)
  turnoutPower[numPrecinctsInTreatment] = sum(turnoutPvalue <= .025) / numCanvasses
  netDemPower[numPrecinctsInTreatment] = sum(netDemPvalue <= .025) / numCanvasses
  
  anyPvalue[numPrecinctsInTreatment] = sum(netDemPvalue <= .025 | turnoutPvalue <= .025) / numCanvasses
}

toc()
round(meanTrunoutImpact[6:16])
round(meanNetDemImpact[6:16])
turnoutPower[6:16] 
netDemPower[6:16] 
anyPvalue[6:16] 
round(addedVotes[6:16])

# accessibleAffectSize = .04 # This should be enough to give us a good shot at a result in within-precinct control 
# sansStratification = FALSE 
# numCanvasses = 500
# numTrials = 200
# minimunNumPrecinctsInTreatment = 6
# maximunNumPrecinctsInTreatment = 16
#     GENERATED:
# round(meanTrunoutImpact[6:16])
# [1] 122 128 150 139 195 213 236 222 292 297 309
# > round(meanNetDemImpact[6:16])
# [1] 85 93 88 94 99 91 81 90 91 69 60
# > turnoutPower[6:16] 
# [1] 0.610 0.580 0.720 0.600 0.868 0.878 0.912 0.842 0.978 0.948 0.966
# > netDemPower[6:16] 
# [1] 0.778 0.836 0.754 0.830 0.844 0.728 0.606 0.726 0.758 0.474 0.364
# > anyPvalue[6:16] 
# [1] 0.904 0.928 0.924 0.926 0.982 0.970 0.964 0.958 0.998 0.974 0.978
# > round(addedVotes[6:16])
# [1] 122 128 150 139 195 213 236 222 292 297 309



# accessibleAffectSize = .04 # This should be enough to give us a good shot at a result in within-precinct control 
# sansStratification = TRUE 
# numCanvasses = 500
# numTrials = 200
# minimunNumPrecinctsInTreatment = 6
# maximunNumPrecinctsInTreatment = 16
#     GENERATED:
# > round(meanTrunoutImpact[6:16])
# [1] 100  99 169 200 160 252 230 241 260 303 257
# > round(meanNetDemImpact[6:16])
# [1] 85 92 92 94 97 93 82 89 90 69 56
# > turnoutPower[6:16] 
# [1] 0.374 0.374 0.824 0.912 0.640 0.956 0.894 0.876 0.938 0.966 0.884
# > netDemPower[6:16] 
# [1] 0.802 0.864 0.778 0.794 0.828 0.768 0.600 0.708 0.764 0.490 0.316
# > anyPvalue[6:16] 
# [1] 0.864 0.914 0.966 0.974 0.932 0.984 0.958 0.964 0.994 0.978 0.916
# > round(addedVotes[6:16])
# [1] 100  99 169 200 160 252 230 241 260 303 257

