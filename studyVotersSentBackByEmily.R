#
#  studyVotersSentBackByEmily 
#


latestSS = "https://docs.google.com/spreadsheets/d/1ltqw-qxLBjMBAnAQQJCvQ4iwjlour4JY6tlnhNFQDt4/edit#gid=548195870"
EmilyExportSept24SS = "https://docs.google.com/spreadsheets/d/150t3476tPpfHUCf_wNxsFoTHZWfCF7uJoVxB531Rm5s/edit#gid=1853794895"
Universe8KWithScores = read_sheet_nice(ss=latestSS, sheet="8k universe with scores")
allRegisteredVotersInCols08D = read_sheet_nice(ss=latestSS, sheet="All registered voters in COLS 8-D")
# EmilyExportSept24 = read_sheet_nice(ss=EmilyExportSept24SS, sheet="StandardText20210924-4157859556")


h15WithEmily = merge( oh15ForPowerTest, allRegisteredVotersInCols08D, 
       by.x="SOS_VOTERID", by.y="StateFileID")

allRegisteredVotersInCols08D_SOS_VOTERID = allRegisteredVotersInCols08D %>% dplyr::rename( SOS_VOTERID = StateFileID)
missingFromPowerTest = anti_join( allRegisteredVotersInCols08D_SOS_VOTERID, 
                                  oh15ForPowerTest, 
                                  by="SOS_VOTERID")
# missingFromPowerTest has 27 entries - COLS 08 D only - these are people who have 
# been removed from the voter file since March 2021



