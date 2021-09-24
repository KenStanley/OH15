# 
#  cutTurfIn33D Cut turf in 33-D that has outside doors under canopies 
#

redo = FALSE 
if ( redo ) 
{ 
  
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
  
  unique(FranklinVoterFile$MyWard)
  
  COLS33Voters = FranklinVoterFile %>% filter( MyWard == "COLS 33")
  save(COLS33Voters, file="COLS33Voters.rds")
}
rm( list=ls())
load( file="COLS33Voters.rds")
source("sheet_ops_nice.R")

COLS33VotersOH15 = COLS33Voters %>% filter( CONGRESSIONAL_DISTRICT==15)

unique(COLS33VotersOH15$PRECINCT_NAME)

col33Households = group_by( COLS33VotersOH15, PRECINCT_NAME, RESIDENTIAL_ADDRESS1, household, mappableAddress ) %>% 
  summarise( numVotersInHouse= n() )

col33ByBldg = group_by( col33Households, PRECINCT_NAME, RESIDENTIAL_ADDRESS1 ) %>% 
  summarise( numUnitsInBldg = n(), .groups='drop'  )

col33Apts = col33ByBldg %>% filter( numUnitsInBldg > 1  )

aptsByPrecinct =  group_by( col33Apts, PRECINCT_NAME ) %>% 
  summarise( numBldgs = n(), numUnitsInBldgs = sum(numUnitsInBldg), .groups='drop'  )

franklinCountyPrecinctsSS = "https://docs.google.com/spreadsheets/d/1-m5VsqfXOS0Ox1rdC6UsL9pLMuCFDcdLipP22sdelTI/edit#gid=1301414353"

write_sheet_nice( col33Apts %>% filter( PRECINCT_NAME == "COLS 33-I"), 
                  ss=franklinCountyPrecinctsSS, 
                  sheet="COLS 33-I" )

precinctAndRussoSR = group_by(COLS33VotersOH15, PRECINCT_NAME, STATE_REPRESENTATIVE_DISTRICT ) %>% summarise( count=n())

