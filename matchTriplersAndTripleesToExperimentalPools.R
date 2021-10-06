#
#  matchTriplersAndTripleesToExperimentalPools 
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

View( Cols02FTriplersWithTriplerPrecinctPool[,relevantColumns] )


