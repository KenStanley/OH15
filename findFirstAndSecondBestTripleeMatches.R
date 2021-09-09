#
#  findFirstAndSecondBestTripleeMatches 
#  

findFirstAndSecondBestTripleeMatches <- function( voterFile=RichlandVoterFile, 
                                           TriplersSS, oneDateSheet ) {
  
  load(file=file.path(data_directory,"allNicknames.rdata"),verbose=TRUE)
  
  
  # triplers = read_sheet( ss=TriplersSS, sheet=oneDateSheet, skip=1)
  triplers = read_sheet( ss=TriplersSS, sheet=oneDateSheet, skip=1)
  
  triplers$Name.Middle = ""
  
  
  # triplers %<>% dplyr::rename( streetNumber = `Street num` )
  
  source("matchTripleesToVoterFile.R")
  
  tripleeCommonColnames = c ( "TriplerID" , "Name.First", "Name.Last","First1", "Last1","MI1","Suffix1" )
  triplees1 = triplers[,tripleeCommonColnames ] %>%  dplyr::rename( Tripler.First = Name.First )  %>% dplyr::rename( Tripler.Last = Name.Last ) %>%
    dplyr::rename( Name.First = First1)  %>% dplyr::rename( Name.Last = Last1) %>% 
    dplyr::rename( Name.Middle = MI1)  %>% dplyr::rename( Name.Suffix = Suffix1)
  
  matchedFriends1 = matchTripleesToVoterFile (namesToMatch=triplees1, masterList=voterFile, allNicknames=allNicknames, 
                                              IDfield="TriplerID", voterFileID="SOS_VOTERID" ) %>% 
    dplyr::rename( triplee.Last = Name.Last )%>%   dplyr::rename( triplee.First = Name.First ) 
  
    tripleeCommonColnames = c ( "TriplerID" , "Name.First", "Name.Last","First2", "Last2","MI2","Suffix2" )
  triplees2 = triplers[,tripleeCommonColnames ] %>%  dplyr::rename( Tripler.First = Name.First )  %>% dplyr::rename( Tripler.Last = Name.Last ) %>%
    dplyr::rename( Name.First = First2)  %>% dplyr::rename( Name.Last = Last2) %>% 
    dplyr::rename( Name.Middle = MI2)  %>% dplyr::rename( Name.Suffix = Suffix2)
  
  matchedFriends2 = matchTripleesToVoterFile (namesToMatch=triplees2, masterList=voterFile, allNicknames=allNicknames, 
                                              IDfield="TriplerID", voterFileID="SOS_VOTERID" ) %>% 
    dplyr::rename( triplee.Last = Name.Last )%>%   dplyr::rename( triplee.First = Name.First )
  
  
  tripleeCommonColnames = c ( "TriplerID" , "Name.First", "Name.Last","First3", "Last3","MI3","Suffix3" )
  triplees3 = triplers[,tripleeCommonColnames ] %>%  dplyr::rename( Tripler.First = Name.First )  %>% dplyr::rename( Tripler.Last = Name.Last ) %>%
    dplyr::rename( Name.First = First3)  %>% dplyr::rename( Name.Last = Last3)%>% 
    dplyr::rename( Name.Middle = MI3)  %>% dplyr::rename( Name.Suffix = Suffix3)
  
  matchedFriends3 = matchTripleesToVoterFile (namesToMatch=triplees3, masterList=voterFile, allNicknames=allNicknames, 
                                              IDfield="TriplerID", voterFileID="SOS_VOTERID" ) %>% 
    dplyr::rename( triplee.Last = Name.Last )%>%   dplyr::rename( triplee.First = Name.First ) 
  
  
  allMatchedTriplees = rbind( matchedFriends1, matchedFriends2, matchedFriends3  )
  # allMatchedTriplees %<>% select( -check2ndAddress ) %>% select( -checkAddress)
 
  write_sheet_nice(allMatchedTriplees,  ss=TriplersSS, sheet=paste(oneDateSheet,"triplees") ) 
  
  
  # range_write( IDandBestNatches[,colsToWrite],ss=TriplersSS, sheet=oneDateSheet, range="A2", col_names = TRUE)
}