#
# countBallots
#

# 
# electionColumns = which(grepl(".20",colnames(thisVF))) 
# votesOnly = thisVF[, electionColumns]
# 
# ballotType = "D"

countBallots <- function(votesOnly, ballotType) {
  votesThisBallot = ( votesOnly == ballotType ) +0 
  votesAllElections = rowSums( votesThisBallot,na.rm=TRUE)
}
