#
#  scoreTriplerMatches.R
#

#
#  Address Score = 10 if neither the street number or the street name match 
#  Address Score = 0.1 if either the number of the street name match
#  Address Score = 0.001 if both match 
#

library(tidyverse)

# I don't think that our nickname match is working at the moment - or maybe it is 

#
# allMatches has just two columns, one for the namesToMatch ID and one for the voterFile ID 
#
scoreTriplerMatches <-function( allmatches,  namesToMatch, voterFile, allNicknames,
                                IDfield="Original.order", voterFileID="SOS_VOTERID",
                                state="OH", useFullName=TRUE,
                                birthYearField="Date.of.Birth.Year",
                                birthMonthField="Date.of.Birth.Month",
                                birthDayField="Date.of.Birth.Day",
                                houseNumField="House.Number",
                                streetNameField="Street.Name",
                                MatchNicknames = FALSE,
                                streetNumOnly = TRUE )  { 
  
  
  if ( nrow( allmatches) > 0 ) {
    
    
    stopifnot( streetNameField=="Street.Name") # Maybe someday I will support other street name fields 
    
    
    if ( !is.na(birthYearField)) {
      stopifnot( FALSE )  # this does not work present
      stopifnot( length(namesToMatch[,birthYearField])==nrow(namesToMatch))
      stopifnot( length(namesToMatch[,birthMonthField])==nrow(namesToMatch))
      stopifnot( length(namesToMatch[,birthDayField])==nrow(namesToMatch))
    }
    if (!is.na(houseNumField)) stopifnot( nrow(namesToMatch[,houseNumField])==nrow(namesToMatch))
    stopifnot( nrow(namesToMatch[,IDfield])==nrow(namesToMatch))
    
    # This stopifnot() tests checks to make sure that if there is ovelap 
    # between colnames(namesToMatch) and colnames(voterFile), voterFileID is that overlap
    # A truly bizarre test 
    
    stopifnot( length(intersect(colnames(namesToMatch),colnames(voterFile)) ) == 0 )  # If oth
    
    firstNameMismatchScore = 10 # We already get penalized for not getting a low name match score 
    middleNameMismatchScore = 9.87 
    lastNameMismatchScore = 13 # A penalty of 1 is generally enough as we count on a 
    # last name and a first name match in most cases. 
    # The only time that a last name mismatch makes sense
    # is when someone has changed their name (typically through marriage)
    # firstNamePenalty = 10 - use firstNameMismatchScore instead 
    failedMiddleNameMatchPenalty = 9.23 # A few women may use a maiden name in some circs and a middle name in others, 
    # but that is a small number and the false positive rate does not justify reducing false negatives 
    middleNameFullMatchScore = .01 
    middleInitialMatchScore = .1 
    BirthYearMismatchPenalty = 101 # 7 or more years difference - don't trust this 100% 
    BirthYear3to6yearMismatchPenalty = 5 
    BirthYear2yearMismatchPenalty = .5 # Not really a penalty 
    BirthYear1yearMismatchPenalty = .1 # Not really a penalty 
    

    # minimumFactorToNextScore = 4 
    # maximumScore = 5 
    
    #
    #  We score first and last names based on how common they are 
    #
    tic()
    firstNameScore = voterFile %>% 
      group_by(voterFile$FIRST_NAME) %>%
      summarise(firstNameScore=n() ) 
    
    firstNameScore$firstNameScore = firstNameScore$firstNameScore / nrow(voterFile)
    colnames(firstNameScore)[1]="FIRST_NAME"
    
    nicknameScore = 1.5* max(firstNameScore$firstNameScore) # A nickname match is no better than the worst first name match  
    
    lastNameScore = voterFile %>% 
      group_by(voterFile$LAST_NAME) %>%
      summarise(lastNameScore=n() ) 
    
    lastNameScore$lastNameScore = lastNameScore$lastNameScore / nrow(voterFile)
    colnames(lastNameScore)[1]="LAST_NAME"
    toc()
    print("that was how long it took to compute the first and last name scores")
    
    
    if (! ("Name.First" %in% colnames(namesToMatch))) browser()
    if (MatchNicknames & ("AlternateName" %in% colnames(allNicknames))) {
      
      nicknameMatch = merge( namesToMatch, allNicknames, by.x="Name.First", by.y="AlternateName")
      nicknameMatchToVoterFile = merge( nicknameMatch, voterFile, by.x="value", by.y="FIRST_NAME") 
      allmatchesPlusNicknameScore = allmatches
      allmatchesPlusNicknameScore$nickNameScore = nicknameScore
      
      nicknameMatches = merge(nicknameMatchToVoterFile,allmatchesPlusNicknameScore) 

      intersect(colnames(nicknameMatchToVoterFile), colnames(allmatches))
      
      nicknameToMiddleMatch = merge( namesToMatch, allNicknames, by.x="Name.First", by.y="AlternateName")
      nicknameToMiddleMatchToVoterFile = merge( nicknameToMiddleMatch, voterFile, by.x="value", by.y="MIDDLE_NAME") 
      nicknameToMiddleMatches = merge(nicknameToMiddleMatchToVoterFile,allmatches) 
      if( nrow(nicknameToMiddleMatches) > 0 ) { 
        
        nicknameToMiddleMatches$nickNameScore = nicknameScore
        allNicknamesMatches = rbind( nicknameMatches[,c(IDfield, voterFileID , "nickNameScore" )],
                                     nicknameToMiddleMatches[,c(IDfield, voterFileID , "nickNameScore" )] )
        allmatchesWithNicknamesScored = merge(allmatches,allNicknamesMatches[,c(IDfield, voterFileID , "nickNameScore" )],
                                              all.x=TRUE)
      } else {
        allmatchesWithNicknamesScored = allmatches
        allmatchesWithNicknamesScored$nickNameScore = firstNameMismatchScore 
      }
      
      if( nrow(allmatchesWithNicknamesScored) != nrow(allmatches)) {
        #
        # I don't know why WILLIE SEWELL JR. results in duplicate matches here, 
        # but this hack gets rid of them - I think 
        #
        # I don't know whow to make group_by_at() work with two columns that
        # are specified by a variable, but so far I only need it with "FriendFriends"
        # and SOS_VOTERID 
        #
        if((IDfield=="FriendFriends") & (voterFileID=="SOS_VOTERID") ) {
          
          
          uniqueMatches = group_by( allmatchesWithNicknamesScored,FriendFriends, SOS_VOTERID ) %>% 
            summarise( nickNameScore=min(nickNameScore)) 
        }  else if ((IDfield=="FriendFriends") & (voterFileID=="MLID") )  {
          uniqueMatches = group_by( allmatchesWithNicknamesScored,FriendFriends, MLID ) %>% 
            summarise( nickNameScore=min(nickNameScore)) 
        } else  if((IDfield=="DWID") & (voterFileID=="SOS_VOTERID") ) {
          uniqueMatches = group_by( allmatchesWithNicknamesScored,DWID, SOS_VOTERID ) %>% 
            summarise( nickNameScore=min(nickNameScore)) 
        } else  if((IDfield=="Individual.Id") & (voterFileID=="MLID") ) {
          uniqueMatches = group_by( allmatchesWithNicknamesScored,Individual.Id, MLID ) %>% 
            summarise( nickNameScore=min(nickNameScore)) 
        } else  if((IDfield=="vb.voterbase_id") & (voterFileID=="MLID") ) {
          uniqueMatches = group_by( allmatchesWithNicknamesScored,vb.voterbase_id, MLID ) %>% 
            summarise( nickNameScore=min(nickNameScore)) 
        } else  if((IDfield=="vb.voterbase_id.orig") & (voterFileID=="MLID") ) {
          uniqueMatches = group_by( allmatchesWithNicknamesScored,vb.voterbase_id.orig, MLID ) %>% 
            summarise( nickNameScore=min(nickNameScore)) 
        } else  if((IDfield=="Individual.Id") & (voterFileID=="SOS_VOTERID") ) {
          uniqueMatches = group_by( allmatchesWithNicknamesScored,Individual.Id, SOS_VOTERID ) %>% 
            summarise( nickNameScore=min(nickNameScore)) 
        } else  if((IDfield=="BetterName") & (voterFileID=="MLID") ) {
          uniqueMatches = group_by( allmatchesWithNicknamesScored,BetterName, MLID ) %>% 
            summarise( nickNameScore=min(nickNameScore)) 
        } else  if((IDfield=="MLID.old") & (voterFileID=="MLID") ) {
          uniqueMatches = group_by( allmatchesWithNicknamesScored,MLID.old, MLID ) %>% 
            summarise( nickNameScore=min(nickNameScore))       
        } else  if((IDfield=="Full.name") & (voterFileID=="SOS_VOTERID") ) {
          uniqueMatches = group_by( allmatchesWithNicknamesScored,Full.name, SOS_VOTERID ) %>% 
            summarise( nickNameScore=min(nickNameScore)) 
        } else{
          browser()
          browser()
          browser()
        }
        allmatchesWithNicknamesScored= uniqueMatches
      }
      #
      # double check 
      #
      if( nrow(allmatchesWithNicknamesScored) != nrow(allmatches)) browser()
      
    } else {
      allmatchesWithNicknamesScored = allmatches
      allmatchesWithNicknamesScored$nickNameScore = firstNameMismatchScore 
    }
    #
    #  Now we attach the nickname score to namesAndVoterMatches
    #
    
    # allmatchesWithNicknamesScored gives a score of 10 to all matches 
    namesAndAllMatches = merge(namesToMatch, allmatchesWithNicknamesScored)
    
    intersect(colnames(namesToMatch), colnames(allmatchesWithNicknamesScored))
    # Why are we losing  a bunch of records with this merge? 
    
    stopifnot( intersect(colnames(namesAndAllMatches),colnames(voterFile)) == c(voterFileID)) # If other column names match, the matching names end up with .x and .y after the merge
    
    namesAndVoterMatches = merge(namesAndAllMatches,voterFile,by=voterFileID)
    
    
    # View( namesAndVoterMatches[,c(20:22,25:26,53:57)])
    if ( nrow(namesAndVoterMatches)!=nrow(namesAndAllMatches)) browser() 
    stopifnot(nrow(namesAndVoterMatches)==nrow(namesAndAllMatches))
    
    
    
    
    #
    # At first we assume that both names match and hence we use firstNameScore and lastNameScore
    #
    nameMatches = merge(merge(namesAndVoterMatches,firstNameScore),lastNameScore)
    
    
    nameMatches$nickNameScore[is.na(nameMatches$nickNameScore)] = firstNameMismatchScore
    
    nameMatches$firstNameScore[which(nameMatches$FIRST_NAME != nameMatches$Name.First)] = 
      nameMatches$nickNameScore[which(nameMatches$FIRST_NAME != nameMatches$Name.First)]
    
    nameMatches$lastNameScore[which(nameMatches$LAST_NAME != nameMatches$Name.Last)] = lastNameMismatchScore 
    
    interestingColumns = c(  "Name.First", "FIRST_NAME" ,"Name.Last", "LAST_NAME",   "Name.Middle",  "MIDDLE_NAME"  , 
                             "firstNameScore"  ,      "lastNameScore"  ,   "SOS_VOTERID",
                            "TriplerID"  )
    View(nameMatches[,interestingColumns])
    #
    #  This is irrelevant because the nickname score is always 10 - not true when 
    #  nickname checking is turned on - But I fixed this above line 210 "= firstNameMismatchScore"
    #
    nameMatches$firstNameScore[which(is.na(nameMatches$firstNameScore))] = firstNameMismatchScore 
    
    # middleNameFullMatchScore = .01 
    middleInitialMatchScore = .1 
    # If both middle names are available in full, 
    middleNameMismatch = failedMiddleNameMatchPenalty
    # If both have middle initials
    # 
    # If we had a first name to middle name match, mismatch should be set to 1 # I don't see any easy way to do this
    # and I suspect that people who list their middle name as their first name, 
    # probably don't list a middle initial 
    
    nameMatches$middleNameScore = 1 # Default if one or the other has no middle name 
    middleInitialsOnBoth = ((nchar(as.character(nameMatches$Name.Middle))>0) & 
                              (nchar(as.character(nameMatches$MIDDLE_NAME))>0)) 
    
    nameMatches$middleNameScore[ middleInitialsOnBoth & 
                                   (substr(nameMatches$Name.Middle,1,1)==substr(nameMatches$MIDDLE_NAME,1,1))] = middleInitialMatchScore
    
    nameMatches$middleNameScore[ middleInitialsOnBoth & 
                                   (substr(nameMatches$Name.Middle,1,1)!=substr(nameMatches$MIDDLE_NAME,1,1))] = middleNameMismatch
    
    fullMiddleNamesOnBoth = ((nchar(as.character(nameMatches$Name.Middle))>1) & 
                               (nchar(as.character(nameMatches$MIDDLE_NAME))>1)) 
    # stopifnot(sum(fullMiddleNamesOnBoth)==0) # The following code is untested for now 
    
    nameMatches$middleNameScore[ which(fullMiddleNamesOnBoth & 
                                         (nameMatches$Name.Middle==nameMatches$MIDDLE_NAME))] = middleNameFullMatchScore
    
    nameMatches$middleNameScore[ which(fullMiddleNamesOnBoth & 
                                         (nameMatches$Name.Middle!=nameMatches$MIDDLE_NAME))] = middleNameMismatch
    
    
    # View(nameMatches[ which(fullMiddleNamesOnBoth),c("Name.First","Name.Last","Name.Middle","MIDDLE_NAME","middleNameScore")])
    
    #
    #  Add Suffix score
    #
    nameMatches$SUFFIX = gsub(" ","",nameMatches$SUFFIX)
    nameMatches$Name.Suffix = gsub(" ","",nameMatches$Name.Suffix)
    
    
    SuffixOnBoth = ((nchar(as.character(nameMatches$SUFFIX))>0) & 
                      (nchar(as.character(nameMatches$Name.Suffix))>0) &
                      !is.na(nameMatches$SUFFIX) & 
                      !is.na(nameMatches$Name.Suffix)) 
    
    SuffixOnNamesToMatch = ((nchar(as.character(nameMatches$Name.Suffix))>0) &
                              !is.na(nameMatches$Name.Suffix)) 
    
    SuffixInVoterFile = ((nchar(as.character(nameMatches$SUFFIX))>0) &
                           !is.na(nameMatches$SUFFIX)) 
    
    
    suffixMatch = nameMatches$SUFFIX == nameMatches$Name.Suffix
    
    suffixMismatchPenalty = 500 # 
    suffixOnJustOnePenalty = 2 # I suspect that when a person uses a suffix they do so consistently 
    suffixMatchScore = .01 # 
    
    nameMatches$suffixScore = 1 
    nameMatches$suffixScore[which(xor(SuffixOnNamesToMatch,SuffixInVoterFile))] = suffixOnJustOnePenalty 
    
    stopifnot( SuffixOnBoth == (SuffixOnNamesToMatch & SuffixInVoterFile)  )
    
    
    nameMatches$suffixScore[which(SuffixOnBoth)] = suffixMismatchPenalty 
    nameMatches$suffixScore[which(SuffixOnBoth & suffixMatch)] = suffixMatchScore
    
    
    # BirthYearMismatchScore = 500
    if ( !is.na(birthYearField)) {
      stopifnot(FALSE) # We don't collect birth dates on Triplers 
      BirthYearOnBoth = ((nchar(as.character(nameMatches$BirthYear))>1) & 
                           (nchar(as.character(nameMatches[,birthYearField]))>1)&
                           as.numeric(nameMatches$BirthYear) > 1800 ) # Some voter records show a birth year of 1800 
      BirthYearDifference = abs(as.numeric(nameMatches$BirthYear) - 
                                  as.numeric(nameMatches[,birthYearField]))  
      
      maximumBirthYearDifference = 123 
      BirthYearScrewups = which(!( (!BirthYearOnBoth) | (abs(BirthYearDifference )< maximumBirthYearDifference) ) )
      # View(nameMatches[BirthYearScrewups,c(birthYearField,"BirthYear","Name.First","FIRST_NAME","Name.Middle","MIDDLE_NAME","middleNameScore","Name.Last", "LAST_NAME","firstNameScore","lastNameScore","nickNameScore")])
      
      if( length(BirthYearScrewups)!=0 ) browser()
      stopifnot( length(BirthYearScrewups)==0 ) 
      
      
      BirthYearMismatchPenalty = 51
      BirthYear3to6yearMismatchPenalty = 5 # We think that in some cases, L2 just guesses
      BirthYear2yearMismatchPenalty = .5 # Not really a penalty
      BirthYear1yearMismatchPenalty = .1 # Not really a penalty
      BirthYearMatchScore = .051 # An address match trumps a birth year match 
      
      nameMatches$BirthYearScore = 1 
      BirthYearOnBoth[is.na(BirthYearOnBoth)] = FALSE 
      bogusBirthYearDifference = 1000
      BirthYearDifference[is.na(BirthYearDifference)] = bogusBirthYearDifference 
      BirthYearMatches =  BirthYearOnBoth & (BirthYearDifference<7)
      
      nameMatches$BirthYearScore[BirthYearOnBoth] = BirthYearDifference[BirthYearOnBoth] * BirthYearDifference[BirthYearOnBoth] # 7 or more years difference - the larger the difference the more we trust it
      # nameMatches$BirthYearScore[which(BirthYearOnBoth)] = BirthYearMismatchPenalty 
      nameMatches$BirthYearScore[which(BirthYearOnBoth & (BirthYearDifference<7))] = BirthYear3to6yearMismatchPenalty
      nameMatches$BirthYearScore[which(BirthYearOnBoth & (BirthYearDifference<3))] = BirthYear2yearMismatchPenalty
      nameMatches$BirthYearScore[which(BirthYearOnBoth & (BirthYearDifference<2))] = BirthYear1yearMismatchPenalty
      nameMatches$BirthYearScore[which(BirthYearOnBoth & (BirthYearDifference<1))] = BirthYearMatchScore
      
      # BirthYearMatches & 
      nameMatches$BirthMonthOnBoth = BirthYearMatches & ((as.numeric(nameMatches$BirthMonth)>-1) & 
                                                           (nchar(as.character(nameMatches[,birthMonthField]))>0) &
                                                           !is.na(nameMatches$BirthMonth) & 
                                                           !is.na(nameMatches[,birthMonthField])) 
      
      nameMatches$BirthMonthOnBoth[is.na(nameMatches$BirthMonthOnBoth)] = FALSE 
      BirthMonthDifference = abs(as.numeric(nameMatches$BirthMonth) - 
                                   as.numeric(nameMatches[,birthMonthField]) )
      
      
      if ( sum(!( (!nameMatches$BirthMonthOnBoth) | (abs(BirthMonthDifference )< 12) )  )>0)  {
        browser()
      }
      
      
      
      
      stopifnot( (!nameMatches$BirthMonthOnBoth) | (abs(BirthMonthDifference )< 12) ) 
      
      BirthMonthMismatchPenalty = 2 # If we have the month and it is wrong, we set the birth year as being wrong
      BirthMonthMatchScore = .1 # 
      
      BirthYearAndMonthMatches = nameMatches$BirthMonthOnBoth & (BirthMonthDifference<1)
      
      nameMatches$BirthMonthScore = 1 
      nameMatches$BirthMonthScore[which(nameMatches$BirthMonthOnBoth)] = BirthMonthMismatchPenalty 
      nameMatches$BirthYearScore[which(nameMatches$BirthMonthOnBoth & (!BirthYearAndMonthMatches))] = BirthYearMismatchPenalty 
      
      nameMatches$BirthMonthScore[which(BirthYearAndMonthMatches)] = BirthMonthMatchScore
      
      #
      # When the birth year is wrong, the birth month does not give us useful information
      # When the birth month is wrong, the birth year begin right loses all meaning 
      # 
      # BirthYearAndMonthMatches & 
      BirthDayOnBoth = BirthYearAndMonthMatches & ((as.numeric(nameMatches$BirthDay)>-1) & 
                                                     (nchar(as.character(nameMatches[,birthDayField]))>0) &
                                                     !is.na(nameMatches$BirthDay) & 
                                                     !is.na(nameMatches[,birthDayField])) 
      BirthDayDifference = abs(as.numeric(nameMatches$BirthDay) - 
                                 as.numeric(nameMatches[,birthDayField]) )
      
      BirthDayScrewups = which(!( (!BirthDayOnBoth) | (abs(BirthDayDifference )< 31) ) )
      
      stopifnot( (!BirthDayOnBoth) | (abs(BirthDayDifference )< 31) ) 
      
      BirthDayMismatchPenalty = 1.5  # BirthDay's are less reliable than birth months 
      BirthDayMatchScore = .25 # 
      
      #
      #  If the brithday is wrong, it calls the birth year and birth month into question and
      #  hence we reduce their correctness 
      #
      nameMatches$BirthDayScore = 1 
      nameMatches$BirthDayScore[which(BirthDayOnBoth)] = BirthDayMismatchPenalty 
      # nameMatches$BirthDayScore[which(BirthDayOnBoth & (BirthDayDifference<3))] = .13
      nameMatches$BirthDayScore[which(BirthDayOnBoth & (BirthDayDifference<1))] = BirthDayMatchScore
      nameMatches$BirthMonthScore[which(BirthDayOnBoth & (BirthDayDifference>=1) & BirthYearAndMonthMatches)] = BirthDayMismatchPenalty
      nameMatches$BirthYearScore[which(BirthDayOnBoth & (BirthDayDifference>=1) & BirthYearAndMonthMatches) ] = BirthDayMismatchPenalty
      
      
      badSuffixes = which((nameMatches$suffixScore>1) & (nameMatches$BirthYearScore==BirthYearMismatchPenalty )) 
      nameMatches$suffixScore[badSuffixes] = nameMatches$suffixScore[badSuffixes] * 1e12 # If the suffix is wrong and the birth year is wrong - this is ont the same person even if the address is right 
    }
    
    addressMismatchPenalty = 10 # We want to find people even if they aren't at the right address 
    addressPartialMatchScore = .1 # if either the street number or street name is right
    addressFullMatchScore = .001 # If we have a perfect match, we certainly want to accept a bad first or last name 
    
    
    if ( is.na(houseNumField)) { 
      nameMatches$addressScore = 1
    } else { 
      
      
      stopifnot( "Street.Name" %in% colnames( nameMatches )) # From the Tripler or Triplee
      stopifnot( "streetName" %in% colnames( nameMatches )) # From the voter database
      stopifnot( houseNumField %in% colnames( nameMatches )) # From the Tripler or Triplee
      stopifnot( "streetNum" %in% colnames( nameMatches )) # From the voter database
      
      # streetNumOnBoth = ((as.numeric(nameMatches$streetNum)>-1) & 
      #                      (nchar(as.character(nameMatches[,houseNumField]))>0) &
      #                      !is.na(nameMatches$streetNum) & 
      #                      !is.na(nameMatches[,houseNumField])) 
      # 
      
      nameMatches$houseNumMatch = nameMatches$streetNum == nameMatches[,houseNumField]
      nameMatches$streetNameMatch = nameMatches$streetName == nameMatches$Street.Name
      
      nameMatches$addressScore = addressMismatchPenalty
      nameMatches$addressScore[which( nameMatches$houseNumMatch | nameMatches$streetNameMatch)] = addressPartialMatchScore
      nameMatches$addressScore[which( nameMatches$houseNumMatch & nameMatches$streetNameMatch)] = addressFullMatchScore
      
    }
    
    if ( !is.na(birthYearField)) {
      
      
      nameMatches$BirthYearMatches = BirthYearMatches
      nameMatches$BirthMatchScore = nameMatches$BirthYearScore * 
        nameMatches$BirthMonthScore * 
        nameMatches$BirthDayScore 
    } else {
      nameMatches$BirthMatchScore = 1
    }
    
    nameMatches$nameAndBirthScore = nameMatches$firstNameScore * nameMatches$lastNameScore *
      nameMatches$middleNameScore * nameMatches$suffixScore * nameMatches$BirthMatchScore * nrow(voterFile)
    
    nameMatches$logNameAndBirthScore = round(log(nameMatches$nameAndBirthScore)/5,0) 
    
    
    
    interestingColumns =  intersect( c("nameAndAddressScore","SUFFIX","Name.Suffix","suffixScore",
                                       "streetNum",houseNumField,
                                       "Street.Name","RESIDENTIAL_ADDRESS1","REGISTRATION_ADDRESS_LINE_1",
                                       "streetNameScore", "addressScore",
                                       "BirthYear", "Birth.Year",
                                       "BirthMonth", "Birth.Month",
                                       "BirthDay", "Birth.Day",
                                       "BirthMatchScore",
                                       "nameAndBirthScore",  "Precinct.Name" , "PRECINCT_NAME" , 
                                       "Name.First","FIRST_NAME","Name.Middle","MIDDLE_NAME","middleNameScore",
                                       "Name.Last", "LAST_NAME","firstNameScore","lastNameScore","nickNameScore"),
                                     colnames(nameMatches) ) 
    
    interestingColumns =  intersect( c("nameAndAddressScore", 
                                       "streetNum",houseNumField,
                                       "Street.Name","streetName","RESIDENTIAL_ADDRESS1","REGISTRATION_ADDRESS_LINE_1",
                                       "streetNameScore", "addressScore",   "houseNumMatch"  , "streetNameMatch"   ,     
                                       "nameAndBirthScore",  
                                       "Name.First","FIRST_NAME",
                                       "Name.Last", "LAST_NAME","firstNameScore","lastNameScore","nickNameScore"),
                                     colnames(nameMatches) ) 
    
    
    # View( nameMatches[,interestingColumns])

    interestingColumns = c(  "Name.First", "FIRST_NAME" ,"Name.Last", "LAST_NAME",   "Name.Middle",  "MIDDLE_NAME"  , 
                             "firstNameScore"  ,      "lastNameScore"  ,   "SOS_VOTERID",
                             "TriplerID"  )
    View(nameMatches[,interestingColumns])
    return <- nameMatches
    
  } else {
    return <- allmatches  # If allMatched is empty, return an empty data.frame 
    
  }
}



