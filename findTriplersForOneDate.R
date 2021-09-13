#
# findTriplersForOneDate
#
#  This code updates google spreadsheets, adding tripler and triplee
#  information - see findFirstAndSecondBestMatches and
#  findFirstAndSecondBestTripleeMatches for details 
#  
#

findTriplersForOneDate <- function( TriplersSS=NULL, thisDateSheet=NULL ) {
  
  
  triplers = read_sheet_nice( ss=TriplersSS, sheet=thisDateSheet, skip=0)
  
  triplers %<>% filter( !is.na(TriplerID))
  
  stopifnot( length( unique(triplers$TriplerID)) == nrow(triplers))
  
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
  setdiff( expectedColumns, colnames(triplers ))
  setdiff( colnames(triplers ), expectedColumns)
  
  
  triplers$new = is.na(triplers$Tripler.in.voter.database) | ( nchar(triplers$Tripler.in.voter.database) < 5 ) 
  triplersOut = findFirstAndSecondBestMatches( allNicknames=allNicknames, 
                                               voterFile=FranklinVoterFile,
                                               triplers = triplers [which(triplers$new),],
                                               cutoffScoreToIncludeInTheTopList = 0.1, 
                                               cutoffScoreToIncludeInTheSecondaryList = 100, 
                                               cutoffMultiple = 1000  )
  
  triplerColumns = colnames(triplers )
  
  intersect( colnames( triplers), colnames( triplersOut))
  
  triplersWithNewTriplers = merge( triplers, 
                                   triplersOut, 
                                   by="TriplerID", all.x=TRUE)
  
  stopifnot( nrow(triplersWithNewTriplers) == nrow( triplers))
  
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
  
  stopifnot( nrow(triplersWithNewTriplers) == nrow( triplersWithNewTriplersAndTriplees))
  
  
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
  
  
  # lastColumnInFirstSet = 13
  # FirstRange= "A3"
  firstColumnInSecondSet = 19
  SecondRange= "S3"
  # 
  #    There is nothing here that we need to update 
  #
  # range_write_nice(triplersWithNewTriplersAndTriplees[,expectedColumns[1:lastColumnInFirstSet]], 
  #                  ss=TriplersSS, sheet=thisDateSheet, range=FirstRange, col_names=TRUE)
  
  range_write_nice(triplersWithNewTriplersAndTriplees[2:nrow(triplersWithNewTriplersAndTriplees),expectedColumns[firstColumnInSecondSet:length(expectedColumns)]], 
                   ss=TriplersSS, sheet=thisDateSheet, range=SecondRange, col_names=FALSE)
  
  
  toc()
  toc()
  
  triplersWithNewTriplersAndTriplees$cleanPhone = as.numeric( gsub( "[^0-9]", "", triplersWithNewTriplersAndTriplees$Phone ) ) 
  
   
  triplersWithNewTriplersAndTriplees$triplerInOH15 = triplersWithNewTriplersAndTriplees$Precinct == toupper(triplersWithNewTriplersAndTriplees$Precinct)
  
  triplersWithNewTriplersAndTriplees$commitToVote = ( triplersWithNewTriplersAndTriplees$triplerInOH15 & 
                                                        triplersWithNewTriplersAndTriplees$cleanPhone > 100000000 ) 
  
  triplersWithNewTriplersAndTriplees$fullTriplers = triplersWithNewTriplersAndTriplees$commitToVote & 
    ( nchar(triplersWithNewTriplersAndTriplees$First1) > 2 ) &
    ( nchar(triplersWithNewTriplersAndTriplees$First2) > 2 ) &
    ( nchar(triplersWithNewTriplersAndTriplees$First3) > 2 ) 
  
   
  triplersWithNewTriplersAndTriplees$triplerFound = nchar(triplersWithNewTriplersAndTriplees$Precinct) > 2  & triplersWithNewTriplersAndTriplees$commitToVote
  triplersWithNewTriplersAndTriplees$triple1Found = nchar(triplersWithNewTriplersAndTriplees$Precinct1) > 2 & triplersWithNewTriplersAndTriplees$commitToVote
  triplersWithNewTriplersAndTriplees$triple2Found = nchar(triplersWithNewTriplersAndTriplees$Precinct2) > 2 & triplersWithNewTriplersAndTriplees$commitToVote
  triplersWithNewTriplersAndTriplees$triple3Found = nchar(triplersWithNewTriplersAndTriplees$Precinct3) > 2 & triplersWithNewTriplersAndTriplees$commitToVote
  
  
  triplersWithNewTriplersAndTriplees$triple1Found[is.na(triplersWithNewTriplersAndTriplees$triple1Found)] = 0 
  triplersWithNewTriplersAndTriplees$triple2Found[is.na(triplersWithNewTriplersAndTriplees$triple2Found)] = 0 
  triplersWithNewTriplersAndTriplees$triple3Found[is.na(triplersWithNewTriplersAndTriplees$triple3Found)] = 0 
  triplersWithNewTriplersAndTriplees$numTriplesFound = triplersWithNewTriplersAndTriplees$triple1Found + 
    triplersWithNewTriplersAndTriplees$triple2Found + 
    triplersWithNewTriplersAndTriplees$triple3Found
  
  triplersWithNewTriplersAndTriplees$triple1InOH15 = triplersWithNewTriplersAndTriplees$Precinct1 == toupper(triplersWithNewTriplersAndTriplees$Precinct1)
  triplersWithNewTriplersAndTriplees$triple2InOH15 = triplersWithNewTriplersAndTriplees$Precinct2 == toupper(triplersWithNewTriplersAndTriplees$Precinct2)
  triplersWithNewTriplersAndTriplees$triple3InOH15 = triplersWithNewTriplersAndTriplees$Precinct3 == toupper(triplersWithNewTriplersAndTriplees$Precinct3)
  
  triplersWithNewTriplersAndTriplees$triple1InOH15[is.na(triplersWithNewTriplersAndTriplees$triple1InOH15)] = 0 
  triplersWithNewTriplersAndTriplees$triple2InOH15[is.na(triplersWithNewTriplersAndTriplees$triple2InOH15)] = 0 
  triplersWithNewTriplersAndTriplees$triple3InOH15[is.na(triplersWithNewTriplersAndTriplees$triple3InOH15)] = 0 
  triplersWithNewTriplersAndTriplees$numTripleesInOH15 = triplersWithNewTriplersAndTriplees$triple1InOH15 + 
    triplersWithNewTriplersAndTriplees$triple2InOH15 + 
    triplersWithNewTriplersAndTriplees$triple3InOH15
  
  
  triplersWithNewTriplersAndTriplees$numTriplee1 = ( nchar(triplersWithNewTriplersAndTriplees$First1 ) > 2 ) 
  triplersWithNewTriplersAndTriplees$numTriplee1[is.na(triplersWithNewTriplersAndTriplees$numTriplee1)] = 0 
  triplersWithNewTriplersAndTriplees$numTriplee2 = ( nchar(triplersWithNewTriplersAndTriplees$First2 ) > 2 ) 
  triplersWithNewTriplersAndTriplees$numTriplee2[is.na(triplersWithNewTriplersAndTriplees$numTriplee2)] = 0 
  triplersWithNewTriplersAndTriplees$numTriplee3 = ( nchar(triplersWithNewTriplersAndTriplees$First3 ) > 2 ) 
  triplersWithNewTriplersAndTriplees$numTriplee3[is.na(triplersWithNewTriplersAndTriplees$numTriplee3)] = 0 
  
  triplersWithNewTriplersAndTriplees$numTriples = triplersWithNewTriplersAndTriplees$numTriplee1 + 
    triplersWithNewTriplersAndTriplees$numTriplee2 + 
    triplersWithNewTriplersAndTriplees$numTriplee3
  
  return <- triplersWithNewTriplersAndTriplees
  
  
  triplersWithNewTriplersAndTripleesSansTop = triplersWithNewTriplersAndTriplees[2:nrow(triplersWithNewTriplersAndTriplees),]
  triplersWithNewTriplersAndTripleesSansTop = triplersWithNewTriplersAndTriplees[2:14,]
  print( paste( "numPledges =", sum(nchar(triplersWithNewTriplersAndTripleesSansTop$Name.First)>2 ,na.rm=TRUE)))
  print( paste( "numCTVs =", sum(triplersWithNewTriplersAndTripleesSansTop$commitToVote,na.rm=TRUE)))
  print( paste( "numFullTriplers =", sum(triplersWithNewTriplersAndTripleesSansTop$fullTriplers,na.rm=TRUE)))
  
  print( paste( "numTriplees =", sum(triplersWithNewTriplersAndTripleesSansTop$numTriples[1:14],na.rm=TRUE)))
  print( paste( "numTripleesFound =", sum(triplersWithNewTriplersAndTripleesSansTop$numTriplesFound[1:14],na.rm=TRUE)))
  print( paste( "numTripleesFoundInOh15 =", sum(triplersWithNewTriplersAndTripleesSansTop$numTripleesInOH15[1:14],na.rm=TRUE)))
  
  
  print( paste( "numTripleesFoundIn COLS 33-D =", sum(triplersWithNewTriplersAndTripleesSansTop$numTripleesInOH15[
    which(triplersWithNewTriplersAndTripleesSansTop$Precinct=="COLS 33-D")],na.rm=TRUE)))
  
  print( paste( "numTripleesFoundIn UA 1-A =", sum(triplersWithNewTriplersAndTripleesSansTop$numTripleesInOH15[
    which(triplersWithNewTriplersAndTripleesSansTop$Precinct=="UA 1-A")],na.rm=TRUE)))
  

   
}