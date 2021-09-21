#
#  inaccessibleDoors 
#    Consider one precinct at a time - identifying inaccessible doors
#

# rm( list= ls())
# load( file="oh15ForPowerTest.rds", verbose = TRUE )
# load( file="experimentalPrecints.rds", verbose = TRUE )
# source("sheet_ops_nice.R")

inaccessibleDoorListSS = "https://docs.google.com/spreadsheets/d/1-m5VsqfXOS0Ox1rdC6UsL9pLMuCFDcdLipP22sdelTI/edit#gid=742638638"
inaccessibleDoorListSheet = "Cols59DApts"

col59inaccessibleDoorList = read_sheet_nice( ss=inaccessibleDoorListSS, 
                                             sheet=inaccessibleDoorListSheet)

oh15ForPowerTest$addressSansOnehalf = gsub( "1/2 ","",oh15ForPowerTest$RESIDENTIAL_ADDRESS1)


oh15ForPowerTest$streetNum = as.numeric(lapply(oh15ForPowerTest$addressSansOnehalf,
                                                function(x) strsplit(as.character(x)," ")[[1]][1] ) )
oh15ForPowerTest$streetName = as.character(lapply(oh15ForPowerTest$addressSansOnehalf,
                                                   function(x) strsplit(as.character(x)," ")[[1]][2] ) )



Cols59D = oh15ForPowerTest %>% filter( PRECINCT_NAME == "COLS 59-D")


householdsInCols59D = group_by( Cols59D %>% filter(anyVoter),
                                PRECINCT_NAME, CONGRESSIONAL_DISTRICT,
                                household , mappableAddress, RESIDENTIAL_ADDRESS1, 
                                RESIDENTIAL_SECONDARY_ADDR ) %>%
  summarise(  count=n(), alwaysHouse=mean(alwaysVoter), occHouse=sum(occasionalVote),
              anyVoter=sum(anyVoter),
              voted2018=sum(voted2018),
              voted2019=sum(voted2019),
              voted2020=sum(voted2020),
              newRegs=sum(newRegs),
              predicted2021turnout=sum(predicted2021turnout),
              voteScore=mean(voteScore) )

householdsInCols59D$mappableAddress = gsub("COLUMBUS  OH",
                                           "Columbus OH", householdsInCols59D$mappableAddress )

householdsInCols59DWinaccessible = merge( householdsInCols59D, 
                                          col59inaccessibleDoorList[,c("mappableAddress","inaccessible") ],
                                                                    by="mappableAddress",all.x=TRUE )

View( householdsInCols59DWinaccessible[which(is.na(householdsInCols59DWinaccessible$inaccessible)),])

householdApts = group_by( householdsInCols59DWinaccessible, mappableAddress ) %>%
  summarise( numAptsInBldg = n() )



# dim(householdsInTargetPrecincts %>% filter( RESIDENTIAL_ADDRESS1 == "626 LEHMAN ST"))




