library( tidyverse)

source("stripFacebookNonNames.R")

splitNameIntoFirstMiddleLastKen <- function(namesToMatch, fullNameField="Full.name", facebook=FALSE) {
  
  ############################
  # Edit by Patrick to use Full Name instead of First, Middle, Last
  # Space is the delimiter
  # People with one name (i.e. Cher): this name is assigned as both first and last name.
  #    + Middle name is an empty string
  # People with two names (i.e. Taylor Swift, Catherine Zeta-Jones): First -> first, last -> last
  #    + Middle name is an empty string
  # People with three or more names (i.e. Jennifer Love Hewitt, Dwayne The Rock Johnson): 
  # First -> first, last -> last, all other names concatenated with strings -> middle
  # Will not handle names that include spaces properly, i.e. Ludwig Van Beethoven, David St. Hubbins 
  # Will not handle records with more than 5 names separated by spaces.
  
  namesToMatch$Name.Suffix <- ""
  
  browser()
  
  
  namesToMatch$namesWithoutSuffix = namesToMatch[,fullNameField] # May be irrelevant 
  
  
  bogusNamesToAdd = namesToMatch[1:10,]
  bogusNamesToAdd[1,fullNameField] = "bogus ii"
  bogusNamesToAdd[2,fullNameField] = "bogus ii"
  bogusNamesToAdd[3,fullNameField] = "bogus jr"
  bogusNamesToAdd[4,fullNameField] = "bogus jr"
  bogusNamesToAdd[5,fullNameField] = "bogus sr"
  bogusNamesToAdd[6,fullNameField] = "bogus sr"
  bogusNamesToAdd[7,fullNameField] = "bogus iii"
  bogusNamesToAdd[8,fullNameField] = "bogus iii"
  bogusNamesToAdd[9,fullNameField] = "bogus iv"
  bogusNamesToAdd[10,fullNameField] = "bogus iv"
  
  namesToMatch = rbind(namesToMatch, bogusNamesToAdd )
  
  
  browser()
  if( facebook ) {
    # namesToMatch = stripFacebookNonNames( namesToMatch,fullNameField=fullNameField)
    
    
    for (index in 1:nrow(namesToMatch)) {
      namesToMatch$namesWithoutSuffix[index] = gsub("[.,]","",namesToMatch[index,fullNameField])
      
    }
    
    #
    #  This deals with a bug in R that causes things that are found only 
    #  once to be treated differently 
    #
    
    
    browser()
    # " jr", " ii", " sr"
    nnames = nrow(namesToMatch)
    Jrs  = which( grepl(" jr", namesToMatch$namesWithoutSuffix, ignore.case = TRUE)  ) 
    namesToMatch$namesWithoutSuffix[Jrs] = gsub(" jr","",namesToMatch$namesWithoutSuffix[Jrs], ignore.case = TRUE)
    namesToMatch$Name.Suffix[Jrs] <- "JR"
    
    browser()
    
    Srs  = which( grepl(" sr", namesToMatch$namesWithoutSuffix, ignore.case = TRUE)  ) 
    namesToMatch$namesWithoutSuffix[Srs] = gsub(" sr","",namesToMatch$namesWithoutSuffix[Srs], ignore.case = TRUE)
    namesToMatch$Name.Suffix[Srs] <- "SR"
    
    iiis  = which( grepl(" iii", namesToMatch$namesWithoutSuffix, ignore.case = TRUE)  ) 
    namesToMatch$namesWithoutSuffix[iiis] = gsub(" iii","",namesToMatch$namesWithoutSuffix[iiis], ignore.case = TRUE)
    namesToMatch$Name.Suffix[iiis] <- "III"
    
    iis  = which( grepl(" ii", namesToMatch$namesWithoutSuffix, ignore.case = TRUE)  ) 
    namesToMatch$namesWithoutSuffix[iis] = gsub(" ii","",namesToMatch$namesWithoutSuffix[iis], ignore.case = TRUE)
    namesToMatch$Name.Suffix[iis] <- "II"
    
    ivs  = which( grepl(" iv", namesToMatch$namesWithoutSuffix, ignore.case = TRUE)  ) 
    namesToMatch$namesWithoutSuffix[ivs] = gsub(" iv","",namesToMatch$namesWithoutSuffix[ivs], ignore.case = TRUE)
    namesToMatch$Name.Suffix[ivs] <- "IV"
    
    browser()
    
    # Split full name by spaces into a list of lists
    namesplit = strsplit(as.character(namesToMatch$namesWithoutSuffix)," ")
    num_names <- rep(0,nnames)
    
    
    for(kk in 1:nnames){
      num_names[kk]<-length(namesplit[[kk]])
      length(namesplit[[kk]])<-5
    }
    
    browser()
    
    namesToMatch = namesToMatch[which((num_names>1) & (num_names<5)),] # num_names = 2,3 or 4 
  } 
  # if (((length(colnames(namesToMatch))<6)  | (length(colnames(namesToMatch)) > 11 ))  ) browser() 
  # stopifnot((length(colnames(namesToMatch))>5) & (length(colnames(namesToMatch))<12))
  colsToKeepZ = c(fullNameField, "Date.of.Birth.Year", "Date.of.Birth.Month",
                  "Date.of.Birth.Day" ,"House.Number", "Name.Suffix" )
  if( length(intersect(colsToKeepZ,colnames(namesToMatch)))!=6) browser() 
  stopifnot(length(intersect(colsToKeepZ,colnames(namesToMatch)))==6)
  nnames = nrow(namesToMatch[,colsToKeepZ])
  
  
  # Split full name by spaces into a list of lists
  namesplit = strsplit(as.character(namesToMatch$namesWithoutSuffix)," ")
  
  # Initialize some vectors
  num_names <- rep(0,nnames)
  last_names <- rep("",nnames)
  middle_names <- rep("",nnames)
  
  
  # Loop through records and count number of names per entry.
  # Pad each list of names to 5 names (truncate if there are >5?)
  for(kk in 1:nnames){
    num_names[kk]<-length(namesplit[[kk]])
    length(namesplit[[kk]])<-5
  }
  
  
  #Convert list to an array to make it easier to work with
  name_array <- t(array(unlist(namesplit),dim=c(5,nnames)))
  
  #Grab first element of each array as first name
  first_names <- str_trim(toupper(name_array[,1]))
  
  # Loop through and grab last names and middle names
  for(nn in 1:nnames){
    last_names[nn] <- str_trim(toupper(name_array[nn,num_names[nn]]))
    if(num_names[nn]>2){
      middle_names[nn] <- str_trim(toupper(paste(name_array[nn,2:(num_names[nn]-1)],collapse=' ')))
    }
  }
  
  stopifnot(sum(grepl("bogus",namesToMatch[,fullNameField]))==0)
  stopifnot(sum(grepl("bogus",namesToMatch$namesWithoutSuffix))==0)
  
  # Enter separate names into the the data frame
  namesToMatch$Name.First <- first_names
  namesToMatch$Name.Last <- last_names
  namesToMatch$Name.Middle <- middle_names
  
  return <- namesToMatch
}

