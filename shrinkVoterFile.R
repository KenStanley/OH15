#
#  shrink Voter File 
#

columbus3XOnly = grepl("COLS",FranklinVoterFile$PRECINCT_NAME)

SmallerVoterFile = FranklinVoterFile[which(columbus3XOnly), c(1:40 ,113:118)]

save(SmallerVoterFile,file="SmallerVoterFile.rds")
