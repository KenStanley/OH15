#
# dualRandomizationPowerAnalysis
#
tic()
rm( list= ls())
load( file="oh15ForPowerTest.rds", verbose = TRUE )
load( file="experimentalPrecints.rds", verbose = TRUE )
franklinCountyPrecinctsSS = "https://docs.google.com/spreadsheets/d/1-m5VsqfXOS0Ox1rdC6UsL9pLMuCFDcdLipP22sdelTI/edit#gid=1301414353"
source("sheet_ops_nice.R")

experimentalPrecints %<>% 
  dplyr::rename( alwaysVoteHouseholds = `number of households where all voters vote consistently`) %<>% 
  dplyr::rename( leanDems = `number of lean dem voters (i.e. voted in 2020 or 2021 or registered since 2020)`) %<>% 
  dplyr::rename( leanDemHouseholds = `number of households containing at least one lean Dem occasional voter`) %<>% 
  dplyr::rename( sampleAddress = `sample mappable Address (in case you want to see where they are on the map)`) 

turnoutByPrecTop = group_by( oh15ForPowerTest, PRECINCT_NAME ) %>% 
  summarise( expectedTurnout = sum( turnout),
             expectedDemVote = sum(turnout * demVote) )


turnoutByPrecTop$fracDem = turnoutByPrecTop$expectedDemVote / turnoutByPrecTop$expectedTurnout

compareExpToActual = merge( turnoutByPrecTop, experimentalPrecints, 
                            by="PRECINCT_NAME")

colsOfInterest = c( "PRECINCT_NAME"  , "expectedTurnout"   ,
                    "expectedDemVote" , "Voted 2019"   , "fracDem", "Percent D 2020"   )

# View( compareExpToActual[,colsOfInterest ])

#
#  That passed the baisc test, next we see if we can randomly 
#





numResults = 100
numericalExperimentSize = 100 

numPrecinctsInTreatment = 8
percentControl = 1/6 

# for ( result in 1:numResults) { 

#
# step 1 - we have to randomize by household - bozo 
#


if ( 1 ) {
  oh15ForPowerTest$predicted2021turnout[ which(oh15ForPowerTest$newRegs) ] = 
    pmin( 1, oh15ForPowerTest$predicted2021turnout[ which(oh15ForPowerTest$newRegs) ] + 0.1 )
}

# hist(oh15ForPowerTest$predicted2021turnout[which(oh15ForPowerTest$anyVoter &
#                                                    (oh15ForPowerTest$predicted2021turnout < 2))])


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
              voteScore=mean(voteScore * anyVoter * 10 + voteScore ) )

load( file="householdApts.rds",verbose=TRUE)
load( file="allLockedDoors.rds",verbose=TRUE)

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


# View( householdsWithInaccessible[,colsToView ] )


householdsWithInaccessible$category = "not set"
householdsWithInaccessible$category[which( householdsWithInaccessible$alwaysHouse == 1)] = "always"
householdsWithInaccessible$category[which( householdsWithInaccessible$avoid )] = "inaccessible" # Leave out of the randomization pool     
householdsWithInaccessible$category[which( householdsWithInaccessible$voteScore < 0 )] = "leanR"
householdsWithInaccessible$category[which( householdsWithInaccessible$anyVoter == 0  )] = "noVoters"

householdsWithInaccessible$treatmentRand = runif(nrow( householdsWithInaccessible))

householdsWithInaccessible$category[ which( ( householdsWithInaccessible$category=="not set")
                                            & ( householdsWithInaccessible$treatmentRand < percentControl ) )  ] = "control"

householdsWithInaccessible$category[ which( ( householdsWithInaccessible$category=="not set")
                                            & ( householdsWithInaccessible$treatmentRand >= percentControl ) )  ] = "treatment"





OH15WithHouseholdAssign = merge( oh15ForPowerTest, 
                                 householdsWithInaccessible[,c("PRECINCT_NAME" ,"household" ,"CONGRESSIONAL_DISTRICT",  
                                                               "numAptsInBldg", "inaccessible" , "avoid" ,
                                                               "category"  , "treatmentRand")],
                                 by=c("PRECINCT_NAME" ,"household" ,"CONGRESSIONAL_DISTRICT"))



categoryByPrecinct = group_by( OH15WithHouseholdAssign, PRECINCT_NAME ) %>% 
  summarise( always = sum( category=="always"),
             treatment = sum( category=="treatment"),
             control = sum( category=="control"),
             leanR = sum( category=="leanR"),
             inaccessible = sum( category=="inaccessible"),
             noVoters = sum( category=="noVoters"),
             notSet = sum( category=="not set"), allCategories=n() )

categoryByPrecinct$total = rowSums( categoryByPrecinct[,c(2:8)])



votersCategoryByPrecinct = group_by( OH15WithHouseholdAssign, PRECINCT_NAME ) %>% 
  summarise( always = sum( category=="always"),
             treatment = sum( category=="treatment"),
             control = sum( category=="control"),
             leanR = sum( category=="leanR"),
             inaccessible = sum( category=="inaccessible"),
             noVoters = sum( category=="noVoters"),
             notSet = sum( category=="not set"), allCategories=n() )

categoryByPrecinct$total = rowSums( votersCategoryByPrecinct[,c(2:8)])


votersCategoryAndPrec = group_by( OH15WithHouseholdAssign, PRECINCT_NAME,category ) %>% summarise(count=n())



# write_sheet_nice(  categoryByPrecinct , ss=franklinCountyPrecinctsSS,
#                    sheet="categoryByPrecinct")

OH15withBothAssigns  = merge( OH15WithHouseholdAssign, 
                              experimentalPrecints[,c("PRECINCT_NAME"  ,  "Order"  )], 
                              by="PRECINCT_NAME" )

toc()
# intersect( colnames( oh15ForPowerTest ), 
#            colnames( householdsWithInaccessible ) )


save(OH15withBothAssigns,file="OH15withBothAssigns.rds")


