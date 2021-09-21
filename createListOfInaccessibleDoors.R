#
#  createListOfInaccessibleDoors 
#

rm( list=ls())
source("sheet_ops_nice.R")

masterPrecinctInacccesibleListSS = "https://docs.google.com/spreadsheets/d/1ip2qjg0-wYb_4I5sq9rhznUlKilcFu_hasWmEo0_SPI/edit#gid=137738856"
masterPrecinctInacccesibleListSheet = "allInfoForRandomization"
# 
load( file="oh15ForPowerTest.rds", verbose = TRUE )
masterPrecinctInacccesibleList = read_sheet_nice( ss=masterPrecinctInacccesibleListSS,
                                                  sheet=masterPrecinctInacccesibleListSheet )

if ( exists("allDoorsWithAcc" )) { rm( list=c("allDoorsWithAcc") )  } 
for ( precinctIndex in 1:26 ) {
  if  ( !is.na( masterPrecinctInacccesibleList$InaccessibleSpreadsheet[precinctIndex]) &
        !is.na( masterPrecinctInacccesibleList$InaccessibleSheet[precinctIndex]) ) {
    thisPrecinctInaccessibleList = read_sheet_nice( ss=masterPrecinctInacccesibleList$InaccessibleSpreadsheet[precinctIndex], 
                                                    sheet=masterPrecinctInacccesibleList$InaccessibleSheet[precinctIndex] ) 
    if ( exists("allDoorsWithAcc" )) {
      allDoorsWithAcc = rbind(allDoorsWithAcc, 
                                  thisPrecinctInaccessibleList[,c("PRECINCT_NAME", "mappableAddress",
                                                                  "inaccessible")] )
    } else {
      allDoorsWithAcc = thisPrecinctInaccessibleList[,c("PRECINCT_NAME", "mappableAddress",
                                                            "inaccessible")]
    }
  }
}

allDoorsWithAcc$RESIDENTIAL_ADDRESS1 = as.character(lapply(allDoorsWithAcc$mappableAddress,
                                                   function(x) strsplit(as.character(x)," Col")[[1]][1] ) )
#
#  Now we look for incomplete addresses, addresses that need an apartment number
#  but which do not supply one
#
allLockedDoors = allDoorsWithAcc[which(allDoorsWithAcc$inaccessible=="l"),]

households = group_by( oh15ForPowerTest %>% filter(anyVoter),
                                        PRECINCT_NAME, CONGRESSIONAL_DISTRICT,
                                        household , RESIDENTIAL_ADDRESS1,
                                        RESIDENTIAL_SECONDARY_ADDR ) %>%
  summarise(  count=n() )

householdApts = group_by( households, PRECINCT_NAME, RESIDENTIAL_ADDRESS1 ) %>%
  summarise( numAptsInBldg = n() )

# householdsWithNumApts = merge ( households, householdApts, by="RESIDENTIAL_ADDRESS1" )

# householdsWithNumApts$incompleteAddress = householdsWithNumApts$numAptsInBldg > 1 & 
#   ( nchar(householdsWithNumApts$RESIDENTIAL_SECONDARY_ADDR )==0)
# 
# householdsWithNumApts$nearIncompleteAddress = householdsWithNumApts$numAptsInBldg > 1 & 
#   ( nchar(householdsWithNumApts$RESIDENTIAL_SECONDARY_ADDR )<2)

save( householdApts, file="householdApts.rds")
save(allLockedDoors, file="allLockedDoors.rds")



