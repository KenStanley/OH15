#
# runAllPledgeChecks
#

allTriplingPledgeSheetsSS = "https://docs.google.com/spreadsheets/d/1aIFHWl9R4ZCp09UhGgXpVGNtXv-wKBrcagofJVaN1a8/edit#gid=0"
setwd("/Users/kenstanley/Google Drive/Rcode/mansfield21Rcode")
setwd("/Users/kenstanley/Google Drive/Rcode/OH15Rcode/OH15")
source("directory_path.R")

library(dplyr)
library(tictoc)
library(magrittr)

recreateVoterFile = TRUE 
tic()
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
browser()
clearData = FALSE
if ( clearData ) {
  rm ( list=ls())
  source("directory_path.R")
  library(dplyr)
  library(tictoc)
  library(magrittr)
  smallEqualsFast = TRUE 
  if (smallEqualsFast) {
    load(file="SmallerVoterFile.rds",verbose = TRUE)
    FranklinVoterFile=SmallerVoterFile
  } else {
    load(file="FranklinVoterFile.rds",verbose = TRUE)
  }
  
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

source( "findTriplersForOneDate.R")

allVoteTriplingPledgeSheetsSS = "https://docs.google.com/spreadsheets/d/1aIFHWl9R4ZCp09UhGgXpVGNtXv-wKBrcagofJVaN1a8/edit#gid=0"
sheetName = "listOfPledgeSheets"

allVoteTriplingPledgeSheets = read_sheet_nice( ss=allVoteTriplingPledgeSheetsSS, sheet=sheetName )

for ( sheetNumber in 1:nrow(allVoteTriplingPledgeSheets))  {
  findTriplersForOneDate(  TriplersSS=allVoteTriplingPledgeSheets$`Spreadsheet URL`,
                           thisDateSheet=allVoteTriplingPledgeSheets$`Sheet name`,
                           SSname=allVoteTriplingPledgeSheets$`Spreadsheet name`)
}

toc()
