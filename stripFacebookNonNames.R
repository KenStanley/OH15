
stripFacebookNonNames = function( namesToMatch,fullNameField="" ) {
  
  namesToMatch = namesToMatch[which(!grepl("riend",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl(" at ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("View ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("Joined ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("dded by ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl(" of ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("University",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("School",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("College",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("Ohio",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl(" like ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl(" Everything",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("Senior High",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl(" Productions ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl(" Sr. High ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl(" Nonprofit ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl(" like ",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("Admin",namesToMatch[,fullNameField])),]
  namesToMatch = namesToMatch[which(!grepl("Moderator",namesToMatch[,fullNameField])),]
    # 
  # stateResidents = which(grepl(", Indiana",namesToMatch[,fullNameField] ,ignore.case=TRUE)) 
  # stateResidents = c(stateResidents,which(grepl(", Maryland",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Florida",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Nevada",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", New ",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Tennessee",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Kentucky",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Georgia",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", California",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Massachusetts",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Oklahoma",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Minnesota",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Virginia",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Illinois",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Alabama",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Texas",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Pennsylvania",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", WA",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # # if ( length(stateResidents) > 0 ) { namesToMatch = namesToMatch[-c(stateResidents, stateResidents-1),] }
  # stateResidents = c(stateResidents,which(grepl(", North ",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # # stateResidents = which(grepl(", North ",namesToMatch[,fullNameField],ignore.case=TRUE )) 
  # # if ( length(stateResidents) > 0 ) namesToMatch <- namesToMatch[-c(stateResidents, stateResidents-1),]
  # stateResidents = c(stateResidents,which(grepl(", South ",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # # stateResidents = which(grepl(", South ",namesToMatch[,fullNameField],ignore.case=TRUE )) 
  # # if ( length(stateResidents) > 0 ) namesToMatch = namesToMatch[-c(stateResidents, stateResidents-1),]
  # stateResidents = c(stateResidents,which(grepl(", West ",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Penn State",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Colorado",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # stateResidents = c(stateResidents,which(grepl(", Norway",namesToMatch[,fullNameField],ignore.case=TRUE )) ) 
  # # stateResidents = which(grepl(", West ",namesToMatch[,fullNameField],ignore.case=TRUE )) 
  # if( length(stateResidents) > 0 ) {
  #   inStateNamesToMatch = namesToMatch[-c(stateResidents, stateResidents-1),]
  # } else {
  #   inStateNamesToMatch = namesToMatch 
  # }
  inStateNamesToMatch = namesToMatch 
  
  # Eliminate "-" and convert to uppercase
  inStateNamesToMatch[,fullNameField] = gsub("-","",inStateNamesToMatch[,fullNameField])
  inStateNamesToMatch[,fullNameField] = toupper( inStateNamesToMatch[,fullNameField])
  uniqueInStateNamesToMatch = unique(inStateNamesToMatch)
  
  # 
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Hunt High",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Designs",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Admin",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Pages19",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("questionsolid",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Catering",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Salon",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("New ",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("2020",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Moderator",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Members",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("See",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Inc.",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Columbus",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Retailer",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("Sr. High",uniqueInStateNamesToMatch$FriendFriends)),]
  # uniqueInStateNamesToMatch = uniqueInStateNamesToMatch[which(!grepl("[0-9]:[0-9][0-9]",uniqueInStateNamesToMatch$FriendFriends)),]
  # 
  

  
  
  
  return <- uniqueInStateNamesToMatch
}
