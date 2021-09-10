#
# matchTripleesToVoterFile 
#
library( tidyverse)
source("splitNameIntoFirstMiddleLastKen.R")

matchTripleesToVoterFile <- function(namesToMatch, masterList, allNicknames, 
                                     IDfield="Original.order", voterFileID="SOS_VOTERID",
                                     state="OH", useFullName=FALSE,
                                     useHyphens=FALSE,
                                     allowMultipleVoterRecords=TRUE, 
                                     allowMultipleNameMatches=FALSE, 
                                     birthYearField=NA,
                                     birthMonthField=NA,
                                     birthDayField=NA,
                                     houseNumField=NA, # If houseNumField is NA, we skip address and nickname matches
                                     streetNameField=NA, # If houseNumField is NA, we skip address and nickname matches
                                     fullNameField=NA ,
                                     facebook=FALSE,
                                     cutoffScore=1e10) {
  # namesToMatch$Name.Middle =""
  namesToMatch %<>% filter( !is.na(Name.First) & !is.na(Name.Last))
  source("matchTriplersToVoterFile.R")
  matchedFriends <- matchTriplersToVoterFile(namesToMatch, masterList, allNicknames, 
                                     IDfield=IDfield, voterFileID=voterFileID,
                                     state="OH", useFullName=FALSE,
                                     useHyphens=useHyphens,
                                     allowMultipleVoterRecords=allowMultipleVoterRecords, 
                                     allowMultipleNameMatches=allowMultipleNameMatches, 
                                     birthYearField=birthYearField,
                                     birthMonthField=birthMonthField,
                                     birthDayField=birthDayField,
                                     houseNumField=houseNumField, # If houseNumField is NA, we skip address and nickname matches
                                     streetNameField=streetNameField, # If houseNumField is NA, we skip address and nickname matches
                                     fullNameField=NA ,
                                     facebook=facebook,
                                     cutoffScore=cutoffScore)

  June30_2021 = as.Date("2021-06-30")
  matchedFriends$age =   floor( as.integer(June30_2021  - as.Date(matchedFriends[,"DATE_OF_BIRTH"]))  / 365.25 )
  
  matchedFriends$NameAgeAddress = paste(proper(matchedFriends$FIRST_NAME),
                                        " ",proper(matchedFriends$MIDDLE_NAME),
                                        " ",proper(matchedFriends$LAST_NAME),
                                        " ",proper(matchedFriends$SUFFIX),
                                        " [",matchedFriends$age,"] ",proper(matchedFriends$RESIDENTIAL_ADDRESS1),
                                        " ",proper(matchedFriends$RESIDENTIAL_SECONDARY_ADDR), sep="" )
  
  
  matchedFriends$checkAddress = ""
  matchedFriends$correctVoter = ""
  matchedFriends$checkAddress[matchedFriends$addressScore > .05] = "Check Addr"
  
  bestMatches = matchedFriends %>% filter( totalScoreRand == minTotalScore & totalScoreRand < 10 )
  
  otherMatches = anti_join(matchedFriends,bestMatches, by=c("SOS_VOTERID" ,    "TriplerID") ) %>%
    filter( totalScoreRand <= minTotalScore * 1000 & 
              totalScoreRand < 100 )
  
  otherMatches %<>% arrange(totalScoreRand)
  secondBestMatch = otherMatches[!duplicated(otherMatches[,c( "TriplerID") ]),]
  
  colNamesToSave = intersect( c(     "TriplerIDout" , "SOS_VOTERID"  , "Name.First"  ,     "Name.Last"  , "Name.Middle" , "Name.Suffix" , 
                                     "FIRST_NAME"    , "LAST_NAME" , "MIDDLE_NAME", "SUFFIX", "streetNumber"   , "Street name"  ,
                                     "NameAgeAddress"  ,  "PRECINCT_NAME"    ,    "nameAndAddressScore"  , 
                                     "totalScore", "firstNameScore" , "lastNameScore"  ,
                                     "houseNumMatch"  , "streetNameMatch"  ,    "addressScore"  ,
                                     "BirthMatchScore"            ,      "nameAndBirthScore"   ,       
                                     "totalScore"  , "totalScoreRand"   ,  "minBirthAndAddressScore" ), colnames(matchedFriends) ) 
  # 
  
  # View( matchedFriends[,colNamesToSave])
  # View( bestMatches[,intersect(colNamesToSave,colnames(bestMatches))])
  # View( secondBestMatch[,colNamesToSave])
  # 
  bestMatches %<>% dplyr::rename( bestTripleeID = SOS_VOTERID )
  # bestMatches %<>% dplyr::rename( BestNameAgeAddress = NameAgeAddress )
  
  secondBestMatch %<>% dplyr::rename( secondTripleeID = SOS_VOTERID )
  secondBestMatch %<>% dplyr::rename( SecondNameAgeAddress = NameAgeAddress )
  secondBestMatch %<>% dplyr::rename( check2ndAddress = checkAddress )
  secondBestMatch %<>% dplyr::rename( correct2ndVoter = correctVoter )
  # secondBestMatch %<>% dplyr::rename(  ward2ndVoter = WARD )
  
  #
  # Now create the first four columns to add and use range_write() 
  #
  IDandBestNatch = merge( namesToMatch[which(!is.na(namesToMatch$Name.First)),c("TriplerID", "Tripler.First", "Tripler.Last","Name.First", "Name.Middle","Name.Last","Name.Suffix")], bestMatches[,c("TriplerID","bestTripleeID","NameAgeAddress","checkAddress","PRECINCT_NAME","correctVoter")], by="TriplerID", all.x=TRUE )
  IDandBestNatches = merge( IDandBestNatch, secondBestMatch[,c("TriplerID","secondTripleeID","SecondNameAgeAddress","check2ndAddress","correct2ndVoter")], by="TriplerID", all.x=TRUE )
  
  IDandBestNatches %<>% dplyr::rename( TriplerIDout = TriplerID )

  colsToWrite = c("TriplerIDout", "Tripler.First", "Tripler.Last", "Name.First", "Name.Middle","Name.Last","Name.Suffix","bestTripleeID","NameAgeAddress" , "PRECINCT_NAME","correctVoter" ,"secondTripleeID","SecondNameAgeAddress","correct2ndVoter" )
  
  # browser()
  return <- IDandBestNatches[,colsToWrite]
}

