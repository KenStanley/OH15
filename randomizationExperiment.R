#
#  randomizationExperiment 
#

now obsolete, folded back into onePowerAnalysis.R 

rm( list = ls())
load(file="OH15allPrecinctResultsVsExpected.rds", verbose = TRUE )


#
#
#

# toplineResults = group_by( OH15allPrecinctResultsVsExpected, treatmentPrec ) %>%
#   summarise( precNetDem2021=sum(netDem), expected2021NetDemVote=sum(expected2021NetDemVote),
#              precDemVoteIn2021=sum(precDemVoteIn2021), 
#              precVotedIn2021 = sum(precVotedIn2021))

overEstimatePerVote = ( sum( OH15allPrecinctResultsVsExpected$netDem ) - 
                          sum( OH15allPrecinctResultsVsExpected$expected2021NetDemVote) ) / 
  sum( OH15allPrecinctResultsVsExpected$precVotedIn2021)

OH15allPrecinctResultsVsExpected$netImprovement = OH15allPrecinctResultsVsExpected$netDem - 
  OH15allPrecinctResultsVsExpected$expected2021NetDemVote - 
  OH15allPrecinctResultsVsExpected$precVotedIn2021 * overEstimatePerVote


# toplineResults = group_by( OH15allPrecinctResultsVsExpected, treatmentPrec ) %>%
#   summarise( precNetDem2021=sum(netDem), expected2021NetDemVote=sum(expected2021NetDemVote),
#              precDemVoteIn2021=sum(precDemVoteIn2021), 
#              precVotedIn2021 = sum(precVotedIn2021),
#              netImprovement=sum(netImprovement))

netImprovementInTreatment = sum( OH15allPrecinctResultsVsExpected$netImprovement[
  which( OH15allPrecinctResultsVsExpected$treatmentPrec)
])

numTrials = 1000 
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