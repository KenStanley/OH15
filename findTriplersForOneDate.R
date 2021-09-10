#
# findTriplersForOneDate
#
#  This code updates google spreadsheets, adding tripler and triplee
#  information - see findFirstAndSecondBestMatches and
#  findFirstAndSecondBestTripleeMatches for details 
#  
#
setwd("/Users/kenstanley/Google Drive/Rcode/mansfield21Rcode")
setwd("/Users/kenstanley/Google Drive/Rcode/OH15Rcode/OH15")
source("directory_path.R")

library(dplyr)
library(tictoc)
library(magrittr)

recreateVoterFile = FALSE 
tic()
if ( recreateVoterFile ) {
  
  rm( list=ls())
  source("directory_path.R")
  library(dplyr)
  library(tictoc)
  library(magrittr)
  
  #
  #   FRANKLIN_SEP2021.txt can be downloaded as FRANKLIN.txt from 
  #   https://www6.ohiosos.gov/ords/f?p=VOTERFTP:HOME::::::
  #
  FranklinVoterFile = read.csv(file=file.path(data_directory,"FRANKLIN_SEP2021.txt"), stringsAsFactors= FALSE )
  
  FranklinVoterFile$addressSansOnehalf = gsub( "1/2 ","",FranklinVoterFile$RESIDENTIAL_ADDRESS1)
  FranklinVoterFile$addressSansOnehalf = gsub( "918Q","918",FranklinVoterFile$addressSansOnehalf) # Deals with one voter
  
  FranklinVoterFile$mappableAddress = paste(FranklinVoterFile$addressSansOnehalf, FranklinVoterFile$RESIDENTIAL_CITY, " OH", FranklinVoterFile$RESIDENTIAL_ZIP  )   
  
  FranklinVoterFile$streetNum = as.numeric(lapply(FranklinVoterFile$addressSansOnehalf,
                                                  function(x) strsplit(as.character(x)," ")[[1]][1] ) )
  FranklinVoterFile$streetName = as.character(lapply(FranklinVoterFile$addressSansOnehalf,
                                                     function(x) strsplit(as.character(x)," ")[[1]][2] ) )
  
  # FranklinVoterFile = merge( FranklinVoterFile, MansfieldAlloyLatLong, by="mappableAddress", all.x=TRUE)
  
  
  
  streetTooShort = nchar(FranklinVoterFile$streetName) < 3
  
  FranklinVoterFile$streetName[streetTooShort] = paste(FranklinVoterFile$streetName[streetTooShort],as.character(lapply(FranklinVoterFile$addressSansOnehalf[streetTooShort],
                                                                                                                        function(x) strsplit(as.character(x)," ")[[1]][3] ) ) )
  maxStreetNum = 100000
  FranklinVoterFile$odd = mod(FranklinVoterFile$streetNum,2)
  # sort ascending by funkyStreetNumber to march up the even numbers and back down the odd numbers. 
  FranklinVoterFile$funkyStreetNumber = FranklinVoterFile$odd * ( maxStreetNum - FranklinVoterFile$streetNum ) +
    ( 1- FranklinVoterFile$odd) * ( FranklinVoterFile$streetNum )
  
  # voteTriplingMansfield2021SS = "https://docs.google.com/spreadsheets/d/1ugs8EBCvVbmdsEwHYH6JyZsvIFPYnROLxxDktTHgQMc/edit#gid=2028028809"
  
  # #
  # #  alloyPlusCF is created in the first 61 lines of createWalkListsForSam.R
  # #
  # FranklinVoterFile$streetName = as.character(lapply(FranklinVoterFile$addressSansOnehalf,
  #                                              function(x) strsplit(as.character(x)," ")[[1]][2] ) )
  # 
  # 
  #   FranklinVoterFile$streetNum = eval( as.numeric(lapply(FranklinVoterFile$addressSansOnehalf,
  #                                                         function(x) strsplit(as.character(x)," ")[[1]][1] ) ) ) 
  #   FranklinVoterFile$streetName = eval( as.character(lapply(FranklinVoterFile$addressSansOnehalf,
  #                                                            function(x) strsplit(as.character(x)," ")[[1]][2] ) ) ) 
  #   
  #   streetTooShort = nchar(FranklinVoterFile$streetName) < 3   
  #   
  #   # This replaces E, N, W and S with the non-dreictional street name 
  #   FranklinVoterFile$streetName[streetTooShort] = as.character(lapply(FranklinVoterFile$addressSansOnehalf[streetTooShort], function(x) strsplit(as.character(x)," ")[[1]][3] ) ) 
  #   
  allStreetNames = group_by(FranklinVoterFile, streetName) %>% summarize( numVotersThisStreet = n() )
  
  save(FranklinVoterFile,file="FranklinVoterFile.rds",version=2)
}
toc()
tic()

clearData = FALSE
if ( clearData ) {
  rm ( list=ls())
  
  load(file="FranklinVoterFile.rds",verbose = TRUE)
  toc()
  tic()
}

source("sheet_ops_nice.R")
source("countBallots.R")
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

# write_sheet_nice(allStreetNames, ss=TriplersSS, sheet="allStreetNames")


# TriplersSS = "https://docs.google.com/spreadsheets/d/1C5DKMdtRCm6Mw8GsmF-oaMPexy-z75SFlZIuBdN21Zg/edit#gid=0"
TriplersSS =  "https://docs.google.com/spreadsheets/d/1v48k6F5a_FurPG1sP81u0-AhSud6t47QCy4uvUin_NA/edit#gid=0"
thisDateSheet= "VoteTriplers" # "June 19th"

toc()
tic()

source("findFirstAndSecondBestMatches.R")
load(file=file.path(data_directory,"allNicknames.rdata"),verbose=TRUE)

triplers = read_sheet_nice( ss=TriplersSS, sheet=thisDateSheet, skip=0)

toc()

tic()

expectedColumns = c(  "TriplerID"  ,  "Date",         "Canvass.Precinct"          
                      ,       "Thanked",      "Door",         "Canvasser",   
                      "Name.First",   "Name.Middle",  "Name.Last",   
                      "Name.Suffix",  "TriplerAddress"   ,   "Phone",       
                      "Zip and Notes","Street found", "streetNumber",
                      "Street Name bogus"  ,     "index",        "Street.Name", 
                      "TriplerSosId", "Tripler.in.voter.database" , "Precinct",    
                      "First1",       "MI1",          "Last1",       
                      "Suffix1",      "Triplee1ID",   "Triplee1.in.voter.database" ,
                     "Precinct1",    "First2",       "MI2",         
                      "Last2",        "Suffix2",      "Triplee2ID",  
                      "Triplee2.in.voter.database", "Precinct2",    "First3",      
                      "MI3",          "Last3",        "Suffix3",     
                      "Triplee3ID",   "Triplee3.in.voter.database", "Precinct3",   
                      "SecondTripler","SecondTriplee1"   , "SecondTriplee2"   ,         
                      "SecondTriplee3"        )

stopifnot( setequal( expectedColumns, colnames(triplers )))

triplers$new = is.na(triplers$TriplerSosId)
triplersOut = findFirstAndSecondBestMatches( allNicknames=allNicknames, 
                                             voterFile=FranklinVoterFile,
                                             triplers = triplers [which(triplers$new),])


triplerColumns = colnames(triplers )

intersect( colnames( triplers), colnames( triplersOut))

triplersWithNewTriplers = merge( triplers, 
                                 triplersOut, 
                                 by="TriplerID", all.x=TRUE)

newTriplers = which(triplersWithNewTriplers$new) 
triplersWithNewTriplers$TriplerSosId[newTriplers] = triplersWithNewTriplers$SOS_VOTERID[newTriplers]
triplersWithNewTriplers$Precinct[newTriplers] = triplersWithNewTriplers$PRECINCT_NAME[newTriplers]
triplersWithNewTriplers$Triplee1.in.voter.database[newTriplers] = triplersWithNewTriplers$NameAgeAddressNew[newTriplers]
triplersWithNewTriplers$SecondTripler[newTriplers] = triplersWithNewTriplers$SecondNameAgeAddressNew[newTriplers]

# triplersWithNewTriplers$newTriplerID = !is.na(triplersWithNewTriplers)

toc()

source("findFirstAndSecondBestTripleeMatches.R")

tic()
tripleeMatches = findFirstAndSecondBestTripleeMatches( allNicknames=allNicknames, 
                                                       voterFile=FranklinVoterFile,
                                                       triplers = triplers [which(triplers$new),] ) 



triplersWithNewTriplersAndTriplees = merge( triplersWithNewTriplers, 
                                            tripleeMatches, 
                                            by.x="TriplerID", by.y="TriplerIDout",all.x=TRUE)



newTriplers = which(triplersWithNewTriplersAndTriplees$new) 

triplersWithNewTriplersAndTriplees$Tripler.in.voter.database[newTriplers] = triplersWithNewTriplersAndTriplees$NameAgeAddress[newTriplers]

triplersWithNewTriplersAndTriplees$Triplee1ID[newTriplers] = triplersWithNewTriplersAndTriplees$Triplee1IDNew[newTriplers]
triplersWithNewTriplersAndTriplees$Triplee1.in.voter.database[newTriplers] = triplersWithNewTriplersAndTriplees$Triplee1.in.voter.databaseNew[newTriplers]
triplersWithNewTriplersAndTriplees$Precinct1[newTriplers] = triplersWithNewTriplersAndTriplees$Precinct1New[newTriplers]
triplersWithNewTriplersAndTriplees$SecondTriplee1[newTriplers] = triplersWithNewTriplersAndTriplees$SecondTriplee1New[newTriplers]

triplersWithNewTriplersAndTriplees$Triplee2ID[newTriplers] = triplersWithNewTriplersAndTriplees$Triplee2IDNew[newTriplers]
triplersWithNewTriplersAndTriplees$Triplee2.in.voter.database[newTriplers] = triplersWithNewTriplersAndTriplees$Triplee2.in.voter.databaseNew[newTriplers]
triplersWithNewTriplersAndTriplees$Precinct2[newTriplers] = triplersWithNewTriplersAndTriplees$Precinct2New[newTriplers]
triplersWithNewTriplersAndTriplees$SecondTriplee2[newTriplers] = triplersWithNewTriplersAndTriplees$SecondTriplee2New[newTriplers]

triplersWithNewTriplersAndTriplees$Triplee3ID[newTriplers] = triplersWithNewTriplersAndTriplees$Triplee3IDNew[newTriplers]
triplersWithNewTriplersAndTriplees$Triplee3.in.voter.database[newTriplers] = triplersWithNewTriplersAndTriplees$Triplee3.in.voter.databaseNew[newTriplers]
triplersWithNewTriplersAndTriplees$Precinct3[newTriplers] = triplersWithNewTriplersAndTriplees$Precinct3New[newTriplers]
triplersWithNewTriplersAndTriplees$SecondTriplee3[newTriplers] = triplersWithNewTriplersAndTriplees$SecondTriplee3New[newTriplers]

setdiff( triplerColumns, colnames( triplersWithNewTriplersAndTriplees) )
setdiff(  colnames( triplersWithNewTriplersAndTriplees), triplerColumns )


browser()
write_sheet_nice(triplersWithNewTriplersAndTriplees[,expectedColumns], 
                 ss=TriplersSS, sheet=thisDateSheet)


toc()