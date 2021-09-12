library( tidyverse)
source("splitNameIntoFirstMiddleLastKen.R")

matchTriplersToVoterFile <- function(namesToMatch, masterList, allNicknames, 
                                     IDfield="Original.order", voterFileID="SOS_VOTERID",
                                     state="OH", useFullName=TRUE,
                                     useHyphens=FALSE,
                                     allowMultipleVoterRecords=FALSE, 
                                     allowMultipleNameMatches=FALSE, 
                                     birthYearField="Date.of.Birth.Year",
                                     birthMonthField="Date.of.Birth.Month",
                                     birthDayField="Date.of.Birth.Day",
                                     houseNumField="House.Number", # If houseNumField is NA, we skip address and nickname matches
                                     streetNameField="Street.Name", # If houseNumField is NA, we skip address and nickname matches
                                     fullNameField="Full.name" ,
                                     facebook=FALSE,
                                     cutoffScore=1e10) {
  
  
  tic()
  
  if ( nrow(namesToMatch) > 0 ) {
    
    
    stopifnot( is.na(fullNameField) |  fullNameField %in% colnames(namesToMatch) )
    stopifnot( voterFileID %in% colnames(masterList) )
    
    if (!is.na(fullNameField) & fullNameField != "Unused") {
      # This is a crude way of checking to make sure that fullNameField is in namesToMatch
      if ( length(setdiff(c(fullNameField),colnames(namesToMatch)))==0) {
        if( length(unique(namesToMatch[,fullNameField])) != nrow(namesToMatch)) { 
          print( " Change namesToMatch = as.data.frame(namesToMatch)")
          # browser()
        } 
        
      }
    }
    
    if ( !is.na( birthYearField)) {
      
      stopifnot( length(unique(masterList[,voterFileID]))==nrow(masterList))
      # possDups = group_by(masterList, SOS_VOTERID) %>% summarise( count = n() )
      # dups = possDups[which(possDups$count>1),]
      # yesDups = merge( masterList, dups)
      stopifnot( length(masterList[,"BirthYear"])==nrow(masterList))
      stopifnot( length(masterList[,"BirthMonth"])==nrow(masterList))
      stopifnot( length(masterList[,"BirthDay"])==nrow(masterList))
      
      if (! birthYearField %in% colnames( namesToMatch )) { 
        
        #
        # WE could fix this by creating fields with names we choose and then setting birthYearField, etc. to match these names
        #
        print( " # # # betterMatchToMasterList needs a birthYearField, set to empty string if it does not exist # # # ")
      }
      
      
      stopifnot( length(namesToMatch[,birthYearField])==nrow(namesToMatch))
      stopifnot( length(namesToMatch[,birthMonthField])==nrow(namesToMatch))
      stopifnot( length(namesToMatch[,birthDayField])==nrow(namesToMatch))
      
    }
    # if( !is.na(houseNumField)) stopifnot( length(namesToMatch[,houseNumField])==nrow(namesToMatch))
    if( !is.na(houseNumField)) stopifnot( (houseNumField %in% colnames(namesToMatch)) )
    
    stopifnot( (IDfield %in% colnames(namesToMatch)) )
    # stopifnot( nrow(unique(namesToMatch[,IDfield])) > 3 ) # Could the IDfield be empty?
    # stopifnot( length(masterList[,voterFileID])==nrow(masterList))
    
    
    if (useFullName) {
      namesToMatch$fullNameFieldToModify = gsub("\\. "," ",namesToMatch[,fullNameField]) 
      namesToMatch$fullNameFieldToModify = gsub("\\."," ",namesToMatch$fullNameFieldToModify) 
      
      
      if ( useHyphens) { 
        browser() # untested 
        #
        # Left as is, It finds matches to names with the hyp
        #
        hypenatedNames = grepl("-", namesToMatch$fullNameFieldToModify)
        
        namesWithHypens = (namesToMatch[which(hypenatedNames),])
        namesWithHypensReplacedBySpace = namesWithHypens
        namesWithHypensReplacedBySpace$fullNameFieldToModify = gsub("-"," ",namesWithHypensReplacedBySpace$fullNameFieldToModify)
        
        namesWithHypensEliminateLastName = namesWithHypens
        namesWithHypensEliminateLastName$fullNameFieldToModify = 
          str_split(namesWithHypensEliminateLastName$fullNameFieldToModify,"-",simplify=TRUE)[,1]
        
        namesToMatch = rbind(namesToMatch,
                             namesWithHypensReplacedBySpace,
                             namesWithHypensEliminateLastName)
      }
      
      # namesToMatch <- splitNameIntoFirstMiddleLastKen( namesToMatch, fullNameField="fullNameFieldToModify", facebook=facebook )
      namesToMatch = namesToMatch[,setdiff(colnames(namesToMatch),c("fullNameFieldToModify"))] # throw away "fullNameFieldToModify" column 
      
    }
    # toc(quiet=FALSE) 
    
    # 
    
    colnames(namesToMatch)
    if ( "FIRST_NAME" %in% colnames( namesToMatch)) {
      namesToMatch = namesToMatch %>% dplyr::rename( Name.First = FIRST_NAME)
      namesToMatch = namesToMatch %>% dplyr::rename( Name.Last = LAST_NAME)
      namesToMatch = namesToMatch %>% dplyr::rename( Name.Middle = MIDDLE_NAME)
      namesToMatch = namesToMatch %>% dplyr::rename( Name.Suffix = SUFFIX)
    } 
    
    
    # if ( length(namesToMatch$Name.First)!=nrow(namesToMatch))  browser()
    # 
    # stopifnot(length(namesToMatch$Name.First)==nrow(namesToMatch))  
    # stopifnot(length(namesToMatch$Name.Last)==nrow(namesToMatch))  
    # 
    stopifnot( ("Name.First" %in% colnames(namesToMatch)) )
    stopifnot( ("Name.Last" %in% colnames(namesToMatch)) )
    
    stopifnot( ("FIRST_NAME" %in% colnames(masterList)) )
    stopifnot( ("LAST_NAME" %in% colnames(masterList)) )
    
    masterList$RESIDENTIAL_ADDRESS1 = as.character(masterList$RESIDENTIAL_ADDRESS1)
    masterList$RESIDENTIAL_ADDRESS1[is.na(masterList$RESIDENTIAL_ADDRESS1)] = ""
    numNAstreetNums = sum( is.na(masterList$streetNum) )
    # masterList$streetNum[is.na(masterList$streetNum)] = "unknown"
    
    fails = ( masterList$streetNum != as.character(lapply(masterList$RESIDENTIAL_ADDRESS1, 
                                                          function(x) strsplit(as.character(x)," ")[[1]][1] ) ) )
    
    
    if (sum(fails,na.rm=TRUE) >=  ( (nrow(masterList)/200))  ) { toc(); browser() } 
    stopifnot( sum(fails,na.rm=TRUE) <   (nrow(masterList)/200) )  # The failures stem from "1/2" addresses, i.e. 301 1/2 
    
    # This just gets rid of the 1/2 
    masterList$streetNum = as.character(lapply(masterList$RESIDENTIAL_ADDRESS1, 
                                               function(x) strsplit(as.character(x)," ")[[1]][1] ) ) 
    
    #
    #  Clean up the input data 
    #
    if (  !("Name.Middle" %in% colnames(namesToMatch)) )  namesToMatch$Name.Middle = "" 
    if (  !("Name.Suffix" %in% colnames(namesToMatch)) )  namesToMatch$Name.Suffix = "" 
    
    # I have no idea why this was here
    if (!("FIRST_NAME" %in% colnames( namesToMatch))) { 
      # browser()
      # stopifnot(FALSE)
      # namesToMatch$Name.Suffix = "" 
    } # Add a Name.Middle if there was none coming in 
    
    namesToMatch$Name.First =str_trim(toupper(gsub("[-' ]","",namesToMatch$Name.First)))
    namesToMatch$Name.Last =str_trim(toupper(gsub("[-' ]","",namesToMatch$Name.Last)))
    namesToMatch$Name.Middle =str_trim(toupper(gsub("[-' ]","",namesToMatch$Name.Middle)) )
    masterList$LAST_NAME = str_trim(toupper(gsub("[-' ]","", masterList$LAST_NAME) ) )
    masterList$FIRST_NAME = str_trim(toupper(gsub("[-' ]","", masterList$FIRST_NAME) )) 
    masterList$MIDDLE_NAME = str_trim(toupper(gsub("[-' ]","", masterList$MIDDLE_NAME) )) 
    
    
    #
    #  Here we look for exact match in both first name and last name 
    #
    matchTomasterList = merge(namesToMatch,masterList,
                              by.x=c( "Name.First", "Name.Last" ),
                              by.y=c("FIRST_NAME","LAST_NAME"))
    
    FirstAndLastNameMatch = distinct(matchTomasterList[,c(IDfield,voterFileID)])
    
    if (nrow(allNicknames)<2) { 
      FirstAndNicknameMatches = FirstAndLastNameMatch
    } else { 
      
      
      allNicknames$AlternateName = str_trim(allNicknames$AlternateName)
      allNicknames$value = str_trim(allNicknames$value)
      
      allNicknames = distinct(allNicknames)
      
      #
      # This merge creates a row for each variation of the first name as given by allNicknames and matches it to the voter list 
      #
      allWithNicknames = merge(namesToMatch,allNicknames,by.x="Name.First",by.y="value")
      nicknameLastnameMatch = merge(allWithNicknames,masterList,
                                    by.x=c( "AlternateName", "Name.Last" ),
                                    by.y=c("FIRST_NAME","LAST_NAME"))[,c(IDfield,voterFileID)]
      
      
      # browser()
      nicknameSuffixMatch = merge(allWithNicknames[which(nchar(as.character(allWithNicknames$Name.Suffix))>0),],masterList,
                                  by.x=c( "AlternateName", "Name.Suffix" ),
                                  by.y=c("FIRST_NAME","SUFFIX"))[,c(IDfield,voterFileID)]
      
      
      nicknameToMiddleAndLastnameMatch = merge(allWithNicknames,masterList,
                                               by.x=c( "AlternateName", "Name.Last" ),
                                               by.y=c("MIDDLE_NAME","LAST_NAME"))[,c(IDfield,voterFileID)]
      
      FirstAndNicknameMatches = rbind( FirstAndLastNameMatch, nicknameSuffixMatch,
                                       nicknameLastnameMatch, nicknameToMiddleAndLastnameMatch )
    }
    # 
    
    
    
    if ( !is.na( birthYearField)  ) { 
      stopifnot( FALSE ) # This code is not in use at the moment, we don't collect birthdates
      stopifnot( length( unique(namesToMatch[,birthMonthField] ))> 5 ) 
      # browser()
      
      if(length(setdiff(c( "Name.First", "Name.Suffix",birthYearField ),colnames(namesToMatch)))>0) browser()
      if(length(setdiff(c("FIRST_NAME","SUFFIX","BirthYear"),colnames(masterList)))>0) browser()
      firstSuffixAndBirthYearMatch = merge(namesToMatch[which(nchar(as.character(namesToMatch$Name.Suffix))>0),],masterList,
                                           by.x=c( "Name.First", "Name.Suffix",birthYearField ),
                                           by.y=c("FIRST_NAME","SUFFIX","BirthYear"))[,c(IDfield,voterFileID)]
      
      
      firstBirthMonthAndYearMatch = merge(namesToMatch[which(nchar(as.character(namesToMatch[,birthMonthField]))>0),],masterList,
                                          by.x=c( "Name.First", birthMonthField,birthYearField ),
                                          by.y=c("FIRST_NAME","BirthMonth","BirthYear"))[,c(IDfield,voterFileID)]
      
      
      lastBirthMonthAndYearMatch = merge(namesToMatch[which(nchar(as.character(namesToMatch[,birthMonthField]))>0),],masterList,
                                         by.x=c( "Name.Last", birthMonthField,birthYearField ),
                                         by.y=c("LAST_NAME","BirthMonth","BirthYear"))[,c(IDfield,voterFileID)]
      
      
      matchesWithoutAddresses = distinct(rbind(FirstAndNicknameMatches,firstSuffixAndBirthYearMatch,
                                               firstBirthMonthAndYearMatch,lastBirthMonthAndYearMatch))   # 5642  = 77 more than 5565 or 591 more than 5051 
      
    } else { 
      matchesWithoutAddresses = distinct(FirstAndNicknameMatches)   # 5642  = 77 more than 5565 or 591 more than 5051 
      
    }
    
    
    firstnameToMiddleAndLastnameMatch = merge(namesToMatch,masterList,
                                              by.x=c( "Name.First", "Name.Last" ),
                                              by.y=c("MIDDLE_NAME","LAST_NAME"))[,c(IDfield,voterFileID)]
    
    
    matchesWithoutAddresses = distinct(rbind(matchesWithoutAddresses,
                                             firstnameToMiddleAndLastnameMatch)) # 5963 = 142 more than 5821 or 321 more than 5642 
    
    #
    #  Here we are going to add matches that we find because of a precinct and 
    #  first name or a precinct and last name match 
    #
    
    if( "Canvass.Precinct" %in% colnames(namesToMatch )) {
      
      oldmatchesWithoutAddresses = matchesWithoutAddresses
      
      firstAndPrecinctMatch = merge(namesToMatch,masterList,
                                    by.x=c( "Name.First", "Canvass.Precinct" ),
                                    by.y=c("FIRST_NAME","PRECINCT_NAME"))[,c(IDfield,voterFileID)]
      
      
      lastAndPrecinctMatch = merge(namesToMatch,masterList,
                                   by.x=c( "Name.Last", "Canvass.Precinct" ),
                                   by.y=c("LAST_NAME","PRECINCT_NAME"))[,c(IDfield,voterFileID)]
      
      
      
      matchesWithoutAddresses = distinct(rbind(matchesWithoutAddresses,
                                               firstAndPrecinctMatch, 
                                               lastAndPrecinctMatch)) 
      
    }
    
    if (is.na(houseNumField)) {
      allmatches = distinct(matchesWithoutAddresses)
      
    } else { 
      
      
      firstAndStreetNumberMatch = merge(namesToMatch,masterList,
                                        by.x=c( "Name.First",houseNumField ),
                                        by.y=c("FIRST_NAME","streetNum"))[,c(IDfield,voterFileID)]
      
      
      nrow(distinct(rbind(matchesWithoutAddresses,firstAndStreetNumberMatch)))   # 6368  = 405 more than 5963
      
      
      lastAndStreetNumberMatch = merge(namesToMatch,masterList,
                                       by.x=c( "Name.Last",houseNumField ),
                                       by.y=c("LAST_NAME","streetNum"))[,c(IDfield,voterFileID)]
      
      justTheFirstNamesMaam = merge( firstAndStreetNumberMatch, masterList[,c(1,4:7,11:13)], by="SOS_VOTERID")
      justTheNamesMaam = merge( lastAndStreetNumberMatch, masterList[,c(1,4:7,11:13)], by="SOS_VOTERID")
      
      firstAndStreetNameMatch = merge(namesToMatch,masterList,
                                      by.x=c( "Name.First",streetNameField ),
                                      by.y=c("FIRST_NAME","streetName"))[,c(IDfield,voterFileID)]
      
      
      nrow(distinct(rbind(matchesWithoutAddresses,firstAndStreetNumberMatch)))   # 6368  = 405 more than 5963
      
      
      lastAndStreetNumberMatch = merge(namesToMatch,masterList,
                                       by.x=c( "Name.Last",streetNameField ),
                                       by.y=c("LAST_NAME","streetName"))[,c(IDfield,voterFileID)]
      
      
      
      allmatches = distinct(rbind(matchesWithoutAddresses,firstAndStreetNumberMatch,
                                  lastAndStreetNumberMatch))   # 9613 = 3245 more than 6368
      
    }
    
    # browser()
    source("scoreTriplerMatches.R")
    allMatchesWithScores = scoreTriplerMatches( allmatches=allmatches,  namesToMatch=namesToMatch, 
                                                voterFile=masterList, voterFileID=voterFileID,
                                                allNicknames=allNicknames,
                                                IDfield=IDfield,
                                                birthYearField=birthYearField,
                                                birthMonthField=birthMonthField,
                                                birthDayField=birthDayField,
                                                houseNumField=houseNumField,
                                                MatchNicknames = TRUE) 
    
    
    interestingColumns = c("LAST_NAME" , "FIRST_NAME" , "MIDDLE_NAME" ,
                           "SUFFIX" , "MLID" , "Full.Name" , 
                           "RESIDENTIAL_ADDRESS1" , "BirthYear", 
                           "Tripler" ,"Individual.Id" , "DWID", "vb.voterbase_id" , "SOS_VOTERID"  , 
                           "PARTY_AFFILIATION"  ,  "votedin2018" , "GENERAL.11.08.2016" , "inDatabases" ,
                           "firstNameScore" , "lastNameScore"  , "suffixScore"   , 
                           "nameAndBirthScore"   )
    
    
    
    
    allMatchesWithScores$totalScore = allMatchesWithScores$nameAndBirthScore * allMatchesWithScores$addressScore
    
    
    
    
    allMatchesWithScores$totalScoreRand = allMatchesWithScores$totalScore * (1 + (1:nrow(allMatchesWithScores))/1e15)
    
    if ( useHyphens) {
      if(IDfield=="FriendFriends") {
        if ( voterFileID=="MLID" ){ 
          allScoresMinRand = group_by( allMatchesWithScores, FriendFriends, MLID) %>% 
            summarize( multipleCount=n(), minScore = min(totalScoreRand)) 
        } else { 
          stopifnot ( voterFileID=="SOS_VOTERID" )
          allScoresMinRand = group_by( allMatchesWithScores, FriendFriends, SOS_VOTERID) %>% 
            summarize( multipleCount=n(), minScore = min(totalScoreRand)) 
          
        }
      } else {
        stopifnot( IDfield == "BetterName" )
        if ( voterFileID=="MLID" ){ 
          allScoresMinRand = group_by( allMatchesWithScores, BetterName, MLID) %>% 
            summarize( multipleCount=n(), minScore = min(totalScoreRand)) 
        } else { 
          stopifnot ( voterFileID=="SOS_VOTERID" )
          allScoresMinRand = group_by( allMatchesWithScores, BetterName, SOS_VOTERID) %>% 
            summarize( multipleCount=n(), minScore = min(totalScoreRand)) 
          
        }
      }
      
    }
    
    
    
    columns = c( IDfield)
    
    # browser()
    dim(allMatchesWithScores)
    if ( nrow( allMatchesWithScores) > 0 ) {
      
      
      bestScores = group_by_at( allMatchesWithScores, vars(one_of(columns)) ) %>% summarize( minBirthAndAddressScore=min(nameAndBirthScore),
                                                                                             minTotalScore=min(totalScoreRand),
                                                                                             count=n())
      
      
      allMatchesWithBestScores = merge(allMatchesWithScores,bestScores[,c(1:4)],
                                       by=IDfield)
      
      
      if ( allowMultipleNameMatches ) { 
        bestMatchesForeachID = allMatchesWithBestScores[which(allMatchesWithBestScores$minTotalScore < cutoffScore ) ,]
      } else { 
        bestMatchesForeachID = allMatchesWithBestScores[which((allMatchesWithBestScores$totalScoreRand == allMatchesWithBestScores$minTotalScore) & 
                                                                (allMatchesWithBestScores$minTotalScore < cutoffScore )) ,]
        if(sum( nrow(bestMatchesForeachID)  != length(unique(bestMatchesForeachID[,IDfield])))>0) browser()
        
        stopifnot( nrow(bestMatchesForeachID)  == length(unique(bestMatchesForeachID[,IDfield])))
        
      }
      
      bestScoresbySOS = group_by_at( bestMatchesForeachID, vars(one_of(c(voterFileID))) )  %>% summarize(     minTotalScoreBySOS=min(totalScoreRand))
      
      bestMatchesBySOS = merge(bestMatchesForeachID,bestScoresbySOS,
                               by=voterFileID)
      
      if ( allowMultipleVoterRecords ) { 
        bestMatchesByBoth = bestMatchesBySOS
      } else {
        bestMatchesByBoth = bestMatchesBySOS[which((bestMatchesBySOS$totalScoreRand == bestMatchesBySOS$minTotalScoreBySOS) ) ,]
        stopifnot( nrow(bestMatchesByBoth)  == length(unique(bestMatchesByBoth[,voterFileID])))
        
      }
      
      
      
      bestMatchesByBoth$registeredToVote = FALSE 
      
      
      
      if ( !allowMultipleVoterRecords )   stopifnot( nrow(bestMatchesByBoth)  == length(unique(bestMatchesByBoth[,voterFileID])))
      
      if ( !allowMultipleNameMatches) stopifnot( nrow(bestMatchesByBoth)  == length(unique(bestMatchesByBoth[,IDfield])))
      
      
      
      return <- bestMatchesByBoth[which(bestMatchesByBoth$totalScore < cutoffScore),] # This cutoff Score test appears to be redundant
    }else {
      return <- allMatchesWithScores 
    }
  } else {
    return <- namesToMatch 
  }
}

