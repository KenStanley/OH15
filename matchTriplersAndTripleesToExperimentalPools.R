#
#  matchTriplersAndTripleesToExperimentalPools 
#
#  what do we want to know about triplers and triplees?
#    1) precinct pool for triplers and triplees
#    2) household pool for triplers and triplees - this should be done by household , but we didn't really provde 
#       the household - so for our first cut we will do this by SOS_VOTERID 
#    3) partisanship score for trilers and triplees
#    4) turnout score for triplers and triplees - both mine and TPMs 
#

Cols02Fss = "https://docs.google.com/spreadsheets/d/1BXzvLoV9rdZ-8Aaq8wOTiHz1lSZB_lRMKu7din0bkZw/edit#gid=0"
Cols02Fsheet = "VoteTriplers"

Cols02FTriplers = read_sheet_nice( ss=Cols02Fss, sheet=Cols02Fsheet )

load( file="inPrecinctRandomization.rds", verbose=TRUE )
load(file="experimentalPrecints.rds", verbose=TRUE )

experimentalPrecints$triplerPool = experimentalPrecints$Order <= 13 


Cols02FTriplersWithTriplerPrecinctPool = merge(Cols02FTriplers,
                                               experimentalPrecints[,c("PRECINCT_NAME", "triplerPool")], 
                                               by.x="Precinct",
                                               by.y="PRECINCT_NAME", all.x=TRUE)

relevantColumns = c( "Tripler.in.voter.database" , "Precinct"  ,   "triplerPool" )



Cols02FTriplersWithTriplerHouseholdPool = merge(Cols02FTriplersWithTriplerPrecinctPool,
                                                inPrecinctRandomization[,c("StateFileID", "category")], 
                                                by.x="TriplerSosId",
                                                by.y="StateFileID", all.x=TRUE)

Cols02FTriplersWithTriplerHouseholdPool %<>% dplyr::rename( triplerCategory = category )

relevantColumns = c( relevantColumns, "triplerCategory" )

# View( Cols02FTriplersWithTriplerHouseholdPool[,relevantColumns] )

setdiff(relevantColumns, colnames(Cols02FTriplersWithTriplerHouseholdPool ) )

load(file="OH15withBothAssigns.rds", verbose=TRUE)


# load( file="OH15FranklinDems.rds", verbose = TRUE )
# 
#  Bummer the Sept 24th version is of little use 
# EmilyExportSept24SS = "https://docs.google.com/spreadsheets/d/150t3476tPpfHUCf_wNxsFoTHZWfCF7uJoVxB531Rm5s/edit#gid=1853794895"
# EmilyExportSept24 = read_sheet_nice(ss=EmilyExportSept24SS, sheet="StandardText20210924-4157859556")

EmilyExportSept29ss = "https://docs.google.com/spreadsheets/d/1p8XwpuDOELVMGIBD_qsUZxKEKyDg_RhU1VfS4ouhej0/edit#gid=2111583216"
EmilyExportSept29Sheet = "JustTurnoutAndPartinsanship"
EmilyExportSept29 = read_sheet_nice(ss=EmilyExportSept29ss, sheet=EmilyExportSept29Sheet) 


OH15withBothAssigns$partisanshipScore = OH15withBothAssigns$percentUnaffiliatedVoteD
OH15withBothAssigns$partisanshipScore[OH15withBothAssigns$demVote] = 1 
OH15withBothAssigns$partisanshipScore[OH15withBothAssigns$repVote] = 0 

bothAssignsCols = c( "SOS_VOTERID","predicted2021turnout", "partisanshipScore")

Cols02FTriplersWithTriplerTurnAndPartisanKensA = merge(Cols02FTriplersWithTriplerHouseholdPool,
                                                       OH15withBothAssigns[,bothAssignsCols], 
                                                       by.x="TriplerSosId",
                                                       by.y="SOS_VOTERID", all.x=TRUE)

# "predicted2021turnout", "percentUnaffiliatedVoteD" 

Cols02FTriplersWithTriplerTurnAndPartisanKens = 
  Cols02FTriplersWithTriplerTurnAndPartisanKensA %>% 
  dplyr::rename( tripler21turnout = predicted2021turnout ) %<>% 
  dplyr::rename( tripler21partisanship = partisanshipScore )

relevantColumns = c( relevantColumns, "tripler21turnout", "tripler21partisanship" )


#
#  triplee1 
#


Cols02FTriplee1PrecinctA = merge(Cols02FTriplersWithTriplerTurnAndPartisanKens,
                                 OH15withBothAssigns[,bothAssignsCols], 
                                 by.x="Triplee1ID",
                                 by.y="SOS_VOTERID", all.x=TRUE)

Cols02FTriplee1Precinct = Cols02FTriplee1PrecinctA %>% 
  dplyr::rename( triplee1_21turnout = predicted2021turnout ) %<>% 
  dplyr::rename( triplee1_21partisanship = partisanshipScore )


Cols02FTriplee1PrecinctAndPool = merge(Cols02FTriplee1Precinct,
                                       inPrecinctRandomization[,c("StateFileID", "category")], 
                                       by.x="Triplee1ID",
                                       by.y="StateFileID", all.x=TRUE)

Cols02FTriplee1PrecinctAndPool %<>% dplyr::rename( triplee1Category = category )

relevantColumns = c( relevantColumns, "triplee1_21turnout", "triplee1_21partisanship", "triplee1Category" )


#
# triplee2 
#


Cols02FTriplee2PrecinctA = merge(Cols02FTriplee1PrecinctAndPool,
                                 OH15withBothAssigns[,bothAssignsCols], 
                                 by.x="Triplee2ID",
                                 by.y="SOS_VOTERID", all.x=TRUE)

Cols02FTriplee2Precinct = Cols02FTriplee2PrecinctA %>% 
  dplyr::rename( triplee2_21turnout = predicted2021turnout ) %<>% 
  dplyr::rename( triplee2_21partisanship = partisanshipScore )


Cols02FTriplee2PrecinctAndPool = merge(Cols02FTriplee2Precinct,
                                       inPrecinctRandomization[,c("StateFileID", "category")], 
                                       by.x="Triplee2ID",
                                       by.y="StateFileID", all.x=TRUE)

Cols02FTriplee2PrecinctAndPool %<>% dplyr::rename( triplee2Category = category )

relevantColumns = c( relevantColumns, "triplee2_21turnout", "triplee2_21partisanship", "triplee2Category" )


#
# triplee3 
#


Cols02FTriplee3PrecinctA = merge(Cols02FTriplee2PrecinctAndPool,
                                 OH15withBothAssigns[,bothAssignsCols], 
                                 by.x="Triplee3ID",
                                 by.y="SOS_VOTERID", all.x=TRUE)

Cols02FTriplee3Precinct = Cols02FTriplee3PrecinctA %>% 
  dplyr::rename( triplee3_21turnout = predicted2021turnout ) %<>% 
  dplyr::rename( triplee3_21partisanship = partisanshipScore )


Cols02FTriplee3PrecinctAndPool = merge(Cols02FTriplee3Precinct,
                                       inPrecinctRandomization[,c("StateFileID", "category")], 
                                       by.x="Triplee3ID",
                                       by.y="StateFileID", all.x=TRUE)

Cols02FTriplee3PrecinctAndPool %<>% dplyr::rename( triplee3Category = category )

relevantColumns = c( relevantColumns, "triplee3_21turnout", "triplee3_21partisanship", "triplee3Category" )


#
# triplee1 precinct
#

experimentalPrecintsTriplee = experimentalPrecints %>% dplyr::rename( tripleePool = triplerPool)

Cols02FTriplersWithPrec1Pool = merge(Cols02FTriplee3PrecinctAndPool,
                                     experimentalPrecintsTriplee[,c("PRECINCT_NAME", "tripleePool")], 
                                     by.x="Precinct1",
                                     by.y="PRECINCT_NAME", all.x=TRUE) %>% 
  dplyr::rename( triplee1Pool = tripleePool)

relevantColumns = c( relevantColumns[c(1:15)], "Precinct1",  "triplee1Pool" )


#
# triplee2 precinct
#

Cols02FTriplersWithPrec2Pool = merge(Cols02FTriplersWithPrec1Pool,
                                     experimentalPrecintsTriplee[,c("PRECINCT_NAME", "tripleePool")], 
                                     by.x="Precinct2",
                                     by.y="PRECINCT_NAME", all.x=TRUE) %>% 
  dplyr::rename( triplee2Pool = tripleePool)

relevantColumns = c( relevantColumns[c(1:17)], "Precinct2",  "triplee2Pool" )

View( Cols02FTriplersWithPrec2Pool[,relevantColumns] )
