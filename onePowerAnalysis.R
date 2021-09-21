#
#  onePowerAnalysis   
#
#  OH15withBothAssigns has all the data for a single randomization into 
#  treatment and control precincts
#


rm( list=ls())
tic()
franklinCountyPrecinctsSS = "https://docs.google.com/spreadsheets/d/1-m5VsqfXOS0Ox1rdC6UsL9pLMuCFDcdLipP22sdelTI/edit#gid=1301414353"
source("sheet_ops_nice.R")
load( file="OH15withBothAssigns.rds", verbose=TRUE )
load( file="experimentalPrecints.rds", verbose = TRUE )

experimentalPrecints %<>%
  dplyr::rename( alwaysVoteHouseholds = `number of households where all voters vote consistently`) %<>%
  dplyr::rename( leanDems = `number of lean dem voters (i.e. voted in 2020 or 2021 or registered since 2020)`) %<>%
  dplyr::rename( leanDemHouseholds = `number of households containing at least one lean Dem occasional voter`) %<>%
  dplyr::rename( sampleAddress = `sample mappable Address (in case you want to see where they are on the map)`)


numPrecinctsInTreatment = 8

OH15withBothAssigns$treatmentPrec = OH15withBothAssigns$Order <= numPrecinctsInTreatment



# instantPowerTest 

# what is the standard error of the mean for 
# 700 coin flips with an expected heads of .15 

numControlCoinFlips = 420 # 700 # 400
headsProb = .15 

controlResults = rbinom( n=10000, size=numControlCoinFlips, prob=headsProb)
sd(controlResults)  # 9 at .15, 13 at .5 
numTreatCoinFlips = 3780 # 3500 # 3600
treatmentResults = rbinom( n=10000, size=numTreatCoinFlips, prob=headsProb)
sd(treatmentResults) # 21 at .15, 30 at .5 

# at 700 and 3500, we need 135 votes or 3.9% to get the result we want at .15 
# at 420 and 3780, we need 177 votes or 4.7% 
numVotesNeeded = 2 * ( sd(treatmentResults) + sd(controlResults) * numTreatCoinFlips / numControlCoinFlips )

numVotesNeeded / numTreatCoinFlips

#
#  Now for the much tougher part 
#

# We will put this into a double for loop, in the first we will randomize
#   the outcome and in the second we will do the analysis 
#

inaccessibleAffectSize = .005
accessibleAffectSize = .04 # This should be enough to give us a good shot at a result in within-precinct control 

numCanvasses = 100

turnoutPvalue = rep(-13,numCanvasses)
turnoutImpact = rep(-13,numCanvasses)
netDemImpact = rep(-13,numCanvasses)
netDemPvalue = rep(-13,numCanvasses)

for (canvassIndex in 1:numCanvasses ) { 
  
  
  OH15withBothAssigns$turnoutRand = runif( nrow( OH15withBothAssigns))
  OH15withBothAssigns$partisanshipRand = runif( nrow( OH15withBothAssigns))
  
  OH15withBothAssignsTreatPrecsOnly = OH15withBothAssigns %>% filter( treatmentPrec )
  
  OH15all = OH15withBothAssigns
  
  OH15all$affectedTurnout = OH15all$predicted2021turnout
  
  
  turnoutRandResultsPre = t.test( OH15all$affectedTurnout[which(OH15all$category=="treatment" & OH15all$treatmentPrec)],
                                  OH15all$affectedTurnout[which(OH15all$category=="control"& OH15all$treatmentPrec  )])
  
  
  OH15all$affectedTurnout[which(OH15all$category=="treatment" & OH15all$treatmentPrec)] = 
    OH15all$affectedTurnout[which(OH15all$category=="treatment" & OH15all$treatmentPrec)] + accessibleAffectSize
  
  # 
  # turnoutRandResultsPost = t.test( OH15all$affectedTurnout[which(OH15all$category=="treatment" & OH15all$treatmentPrec)],
  #                                 OH15all$affectedTurnout[which(OH15all$category=="control"& OH15all$treatmentPrec  )])
  
  
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
               precDemVoteIn2021=sum( voted2021 * votedDem ) )
  
  
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
               precVotedIn2021 = sum(precVotedIn2021))
  
  # masterPrecinctInacccesibleListSS = "https://docs.google.com/spreadsheets/d/1ip2qjg0-wYb_4I5sq9rhznUlKilcFu_hasWmEo0_SPI/edit#gid=137738856"
  # # write_sheet_nice(toDisplay, ss=masterPrecinctInacccesibleListSS, sheet="allDoors")
  # 
  
  
  turnoutResults = t.test( OH15all$voted2021[which(OH15all$category=="treatment" & OH15all$treatmentPrec)],
                           OH15all$voted2021[which(OH15all$category=="control"& OH15all$treatmentPrec  )])
  
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
  
  numTrials = 100
  numTreatmentPrecincts = 8 
  beatActualTreatment = 0 
  for (experimentNumber in 1:numTrials) { 
    OH15allPrecinctResultsVsExpected$randomTreatOrder = runif( nrow( OH15allPrecinctResultsVsExpected ))
    nthValue = sort( OH15allPrecinctResultsVsExpected$randomTreatOrder)[numTreatmentPrecincts]
    OH15allPrecinctResultsVsExpected$experimentTreatment = 
      OH15allPrecinctResultsVsExpected$randomTreatOrder <= nthValue
    stopifnot( sum(OH15allPrecinctResultsVsExpected$experimentTreatment) == numTreatmentPrecincts ) 
    
    
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

turnoutRandResultsPre
mean(turnoutImpact) * 3700 
mean(netDemImpact)
sum(turnoutPvalue < .05)
sum(netDemPvalue < .05)

toc()

