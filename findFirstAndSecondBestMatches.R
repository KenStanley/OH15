#
#  matchAllTriplersToRichlandVF 
#  

findFirstAndSecondBestMatches <- function( voterFile=RichlandVoterFile, 
                                           TriplersSS, oneDateSheet ) {
  
  load(file="allNicknames.rdata",verbose=TRUE)
  
  triplers = read_sheet( ss=TriplersSS, sheet=oneDateSheet, skip=1)
  
  # triplers$Name.Middle = ""
  
  if ( "Street num" %in% colnames( triplers)) { 
    triplers %<>% dplyr::rename( streetNumber = `Street num` )
  }
  
  colnames(triplers)[1:11]=paste(colnames(triplers)[1:11],"in")
  source("matchTriplersToVoterFile.R")
   
  matchedFriends = matchTriplersToVoterFile (namesToMatch=triplers, masterList=voterFile, allNicknames=allNicknames, 
                                             IDfield="TriplerID", voterFileID="SOS_VOTERID",
                                             state="OH", useFullName=FALSE,
                                             useHyphens=FALSE,
                                             allowMultipleVoterRecords=TRUE, 
                                             allowMultipleNameMatches=TRUE, 
                                             birthYearField=NA,
                                             birthMonthField=NA,
                                             birthDayField=NA,
                                             houseNumField="streetNumber", # If houseNumField is NA, we skip address and nickname matches
                                             streetNameField="Street.Name",
                                             fullNameField=NA ,
                                             facebook=FALSE,
                                             cutoffScore=1e10) 
  
  
  
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
  
  bestMatches = matchedFriends %>% filter( totalScoreRand == minTotalScore & totalScoreRand < 0.1 )
  
  otherMatches = anti_join(matchedFriends,bestMatches, by=c("SOS_VOTERID" ,    "TriplerID") ) %>%
    filter( totalScoreRand <= minTotalScore * 10000 & 
              totalScoreRand < 10 )
  
  # View(matchedFriends[,c("SOS_VOTERID" , "LAST_NAME"  ,"Name.Last"   ,
  #                        "FIRST_NAME" ,"Name.First" ,  "MIDDLE_NAME"   , "Name.Middle"    , "TriplerID"   ,"totalScoreRand")])

  otherMatches %<>% arrange(totalScoreRand)
  secondBestMatch = otherMatches[!duplicated(otherMatches[,c( "TriplerID") ]),]
  
  colNamesToSave = intersect( c(     "TriplerIDout" , "SOS_VOTERID"  , "Name.First"  ,     "Name.Last"  ,
                                     "FIRST_NAME"    , "LAST_NAME" , "streetNumber"   , "Street name"  ,"Street.name"  ,
                                     "NameAgeAddress"  ,  "WARD"    ,    "nameAndAddressScore"  , 
                                     "totalScore", "firstNameScore" , "lastNameScore"  ,
                                     "houseNumMatch"  , "streetNameMatch"  ,    "addressScore"  ,
                                     "BirthMatchScore"            ,      "nameAndBirthScore"   ,       
                                     "totalScore"  , "totalScoreRand"   ,  "minBirthAndAddressScore" ), colnames(matchedFriends) ) 
  # 
  View( matchedFriends[,colNamesToSave])
  # View( bestMatches[,colNamesToSave])
  # View( secondBestMatch[,colNamesToSave])
  # 
  bestMatches %<>% dplyr::rename( BEST_VOTERID = SOS_VOTERID )
  # bestMatches %<>% dplyr::rename( BestNameAgeAddress = NameAgeAddress )
  
  
  interestingColumns =  intersect( c("BEST_VOTERID" ,    "TriplerID" , "nameAndAddressScore",
                                     "streetNum",
                                     "Street.Name","streetName","RESIDENTIAL_ADDRESS1","REGISTRATION_ADDRESS_LINE_1",
                                     "streetNameScore", "addressScore",   "houseNumMatch"  , "streetNameMatch"   ,
                                     "nameAndBirthScore",
                                     "Name.First","FIRST_NAME",
                                     "Name.Last", "LAST_NAME","firstNameScore","lastNameScore","nickNameScore"),
                                   colnames(bestMatches) )
  View( bestMatches[,interestingColumns])
  
  
  iCmatchedFriends =  intersect( c("SOS_VOTERID" ,    "TriplerID" , "nameAndAddressScore",
                                     "streetNum",
                                     "Street.Name","streetName","RESIDENTIAL_ADDRESS1","REGISTRATION_ADDRESS_LINE_1",
                                     "streetNameScore", "addressScore",   "houseNumMatch"  , "streetNameMatch"   ,
                                     "nameAndBirthScore",
                                     "Name.First","FIRST_NAME",
                                     "Name.Last", "LAST_NAME","firstNameScore","lastNameScore","nickNameScore"),
                                   colnames(matchedFriends) )
  View( matchedFriends[,iCmatchedFriends])
  
  
  
  secondBestMatch %<>% dplyr::rename( SECOND_VOTERID = SOS_VOTERID )
  secondBestMatch %<>% dplyr::rename( SecondNameAgeAddress = NameAgeAddress )
  secondBestMatch %<>% dplyr::rename( check2ndAddress = checkAddress )
  secondBestMatch %<>% dplyr::rename( correct2ndVoter = correctVoter )
  secondBestMatch %<>% dplyr::rename( ward2ndVoter = WARD )
  
  #
  # Now create the first four columns to add and use range_write() 
  #
  
  IDandBestNatch = merge( triplers[,c("TriplerID","Name.First")], bestMatches[,c("TriplerID","BEST_VOTERID","NameAgeAddress","checkAddress","correctVoter","WARD")], by="TriplerID", all.x=TRUE )
  IDandBestNatches = merge( IDandBestNatch, secondBestMatch[,c("TriplerID","SECOND_VOTERID","SecondNameAgeAddress","check2ndAddress","correct2ndVoter","ward2ndVoter")], by="TriplerID", all.x=TRUE )
  
  IDandBestNatches %<>% dplyr::rename( TriplerIDout = TriplerID )
  
  colsToWrite = c("TriplerIDout", "SECOND_VOTERID","SecondNameAgeAddress","check2ndAddress","ward2ndVoter","correct2ndVoter","BEST_VOTERID","NameAgeAddress" ,"checkAddress", "WARD","correctVoter" )
  
  # browser()
  range_write( IDandBestNatches[,colsToWrite],ss=TriplersSS, sheet=oneDateSheet, range="A2", col_names = TRUE)
}