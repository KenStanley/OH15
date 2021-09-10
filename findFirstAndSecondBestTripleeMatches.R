#
#  findFirstAndSecondBestTripleeMatches 
#  

findFirstAndSecondBestTripleeMatches <- function( allNicknames=allNicknames, voterFile=RichlandVoterFile, 
                                                  triplers=triplers  ) {
  
  
  
  # triplers %<>% dplyr::rename( streetNumber = `Street num` )
  
  source("matchTripleesToVoterFile.R")
  
  tripleeCommonColnames = c ( "TriplerID" , "Name.First", "Name.Last","First1", "Last1","MI1","Suffix1" )
  triplees1 = triplers[,tripleeCommonColnames ] %>%  dplyr::rename( Tripler.First = Name.First )  %>% dplyr::rename( Tripler.Last = Name.Last ) %>%
    dplyr::rename( Name.First = First1)  %>% dplyr::rename( Name.Last = Last1) %>% 
    dplyr::rename( Name.Middle = MI1)  %>% dplyr::rename( Name.Suffix = Suffix1)
  
  
  matchedFriends1 = matchTripleesToVoterFile (namesToMatch=triplees1, masterList=voterFile, allNicknames=allNicknames, 
                                              IDfield="TriplerID", voterFileID="SOS_VOTERID" ) 
  
  matchedFriends1A = matchedFriends1 %>% dplyr::rename( Triplee1IDNew = bestTripleeID )  %>%
    dplyr::rename( Triplee1.in.voter.databaseNew = NameAgeAddress )  %>%
    dplyr::rename( Precinct1New = PRECINCT_NAME )  %>%
    dplyr::rename( SecondTriplee1New = SecondNameAgeAddress ) 
  
  matchedFriends1B = matchedFriends1A[ ,c("TriplerIDout", "Triplee1IDNew", "Triplee1.in.voter.databaseNew", 
                                          "Precinct1New", "SecondTriplee1New")]


    
    tripleeCommonColnames = c ( "TriplerID" , "Name.First", "Name.Last","First2", "Last2","MI2","Suffix2" )
    triplees2 = triplers[,tripleeCommonColnames ] %>%  dplyr::rename( Tripler.First = Name.First )  %>% dplyr::rename( Tripler.Last = Name.Last ) %>%
      dplyr::rename( Name.First = First2)  %>% dplyr::rename( Name.Last = Last2) %>% 
      dplyr::rename( Name.Middle = MI2)  %>% dplyr::rename( Name.Suffix = Suffix2)
    
    
    matchedFriends2 = matchTripleesToVoterFile (namesToMatch=triplees2, masterList=voterFile, allNicknames=allNicknames, 
                                                IDfield="TriplerID", voterFileID="SOS_VOTERID" ) 
    
    matchedFriends2A = matchedFriends2 %>% dplyr::rename( Triplee2IDNew = bestTripleeID )  %>%
      dplyr::rename( Triplee2.in.voter.databaseNew = NameAgeAddress )  %>%
      dplyr::rename( Precinct2New = PRECINCT_NAME )  %>%
      dplyr::rename( SecondTriplee2New = SecondNameAgeAddress ) 
    
    matchedFriends2B = matchedFriends2A[ ,c("TriplerIDout", "Triplee2IDNew", "Triplee2.in.voter.databaseNew", 
                                            "Precinct2New", "SecondTriplee2New")]
    

    
    tripleeCommonColnames = c ( "TriplerID" , "Name.First", "Name.Last","First3", "Last3","MI3","Suffix3" )
    triplees3 = triplers[,tripleeCommonColnames ] %>%  dplyr::rename( Tripler.First = Name.First )  %>% dplyr::rename( Tripler.Last = Name.Last ) %>%
      dplyr::rename( Name.First = First3)  %>% dplyr::rename( Name.Last = Last3) %>% 
      dplyr::rename( Name.Middle = MI3)  %>% dplyr::rename( Name.Suffix = Suffix3)
    
    
    matchedFriends3 = matchTripleesToVoterFile (namesToMatch=triplees3, masterList=voterFile, allNicknames=allNicknames, 
                                                IDfield="TriplerID", voterFileID="SOS_VOTERID" ) 
    
    matchedFriends3A = matchedFriends3 %>% dplyr::rename( Triplee3IDNew = bestTripleeID )  %>%
      dplyr::rename( Triplee3.in.voter.databaseNew = NameAgeAddress )  %>%
      dplyr::rename( Precinct3New = PRECINCT_NAME )  %>%
      dplyr::rename( SecondTriplee3New = SecondNameAgeAddress ) 
    
    matchedFriends3B = matchedFriends3A[ ,c("TriplerIDout", "Triplee3IDNew", "Triplee3.in.voter.databaseNew", 
                                            "Precinct3New", "SecondTriplee3New")]
    

  #
  # merge them all together 
  #
  intersect( colnames(matchedFriends1B) , colnames(matchedFriends3B)  )
  matchedFriends1and2 = merge( matchedFriends1B, matchedFriends2B, by="TriplerIDout")
  matchedFriends1_2and3 = merge( matchedFriends1and2, matchedFriends3B, by="TriplerIDout")
  
  columnsToReturn = paste(c(  "Triplee1ID" , "Triplee1.in.voter.database", "Precinct1" ,
                              "Triplee2ID" , "Triplee2.in.voter.database", "Precinct2" ,                
                              "Triplee3ID" , "Triplee3.in.voter.database", "Precinct3" ,            
                              "SecondTriplee1", "SecondTriplee2"  , "SecondTriplee3"   ),"New", sep="") 
  
  
  return <- matchedFriends1_2and3
}