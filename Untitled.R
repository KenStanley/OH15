#
# randomizeWithinPrecincts
#
# 
# library(readxl)
# 
# downloadsDir = "/Users/kenstanley/Downloads"
# 
# feelLucky = read_excel(path=file.path(downloadsDir,"KenSampleExport20210923-20026609372.xls"),sheet=1,skip=0)

library( googlesheets4 )

# KenSampleSS = "https://docs.google.com/spreadsheets/d/150t3476tPpfHUCf_wNxsFoTHZWfCF7uJoVxB531Rm5s/edit#gid=1853794895"
# 
# KenSample = read_sheet_nice(ss=KenSampleSS, sheet=1)
DemographicsNew = read_sheet_nice(ss=KenSampleSS, sheet="DemographicsNew")
DemographicsNew2 = read_sheet_nice(ss=KenSampleSS, sheet="DemographicsNew2")


FranklinCountyDemPrecinctsSS  = "https://docs.google.com/spreadsheets/d/1-m5VsqfXOS0Ox1rdC6UsL9pLMuCFDcdLipP22sdelTI/edit#gid=1985667243"
treatmentPrecinctsSS  = "allInfoForRandomization"
TargetPrecincts = read_sheet_nice(ss=FranklinCountyDemPrecinctsSS, sheet=treatmentPrecinctsSS)

intersect( colnames(TargetPrecincts ), colnames( DemographicsNew))

targetPrecinctAnalysis = merge( TargetPrecincts, DemographicsNew, 
                                by="PRECINCT_NAME")

targetPrecinctAnalysis2 = merge( targetPrecinctAnalysis, DemographicsNew2[,c("PRECINCT_NAME", "Total Doors")], 
                                by="PRECINCT_NAME")


targetPrecinctAnalysis2 %<>% 
  dplyr::rename( `Total People Max` = occVoters ) %<>% 
  dplyr::rename( `Total Doors Max` = occHouse )

colsToView = c( "PRECINCT_NAME"   ,    "Precinct"    ,   "Order"   ,
                   "2020 Voters"  ,"alwaysHousehold"  ,
                "voted 19 %" ,   "Total People Max" , "Total People" ,"Total Doors Max"  , "Total Doors", 
                "fraction Inaccessible"  , "Percent D 2020"   )

View(targetPrecinctAnalysis2[,colsToView] )

# write_sheet_nice( targetPrecinctAnalysis2[,colsToView] , ss= KenSampleSS, sheet="Both" )

setdiff( colsToView, colnames(targetPrecinctAnalysis2 ))
# 
# save(KenSample, file="KenSample.rds" )
# load( file="OH15FranklinDems.rds", verbose = TRUE )


KenSample$FIRST_NAME = toupper( KenSample$FirstName)
KenSample$LAST_NAME = toupper( KenSample$LastName)
KenSample$MIDDLE_NAME = toupper( KenSample$MiddleName)
KenSample$SUFFIX = toupper( KenSample$Suffix)

KenSample$StreetNum = as.numeric(lapply(KenSample$Address,
                                        function(x) strsplit(as.character(x)," ")[[1]][1] ) )

# 
# KenSample$PRECINT_NAME = toupper(lapply(KenSample$PrecinctName,
#                                           function(x) strsplit(as.character(x)," \(")[[1]][1] ) )
# 

KenSample$MIDDLE_NAME[which(is.na( KenSample$MIDDLE_NAME))] = "" 
KenSample$SUFFIX[which(is.na( KenSample$SUFFIX))] = "" 

targetSmartMatchedToVoterFile = merge( KenSample, OH15FranklinDems , 
                                       by= c( "PRECINCT_NAME", "FIRST_NAME", "LAST_NAME", "MIDDLE_NAME" , "SUFFIX" ))

MissingFromKenSample = anti_join( OH15FranklinDems, KenSample, 
                                  by= c( "PRECINCT_NAME", "FIRST_NAME", "LAST_NAME", "MIDDLE_NAME" , "SUFFIX" ) )



MissingFromVoterFile = anti_join( KenSample, OH15FranklinDems, 
                                  by= c( "PRECINCT_NAME", "FIRST_NAME", "LAST_NAME", "MIDDLE_NAME" , "SUFFIX" ) )


MatchesFromMissingCols33D = merge( MissingFromKenSample[(which(MissingFromKenSample$PRECINCT_NAME=="COLS 33-D")),],
                                   MissingFromVoterFile, 
                                   by= c( "FIRST_NAME", "LAST_NAME")) 

View( MissingFromKenSample[which(MissingFromKenSample$PRECINCT_NAME=="COLS 33-D"),])

View( KenSample)


intersect( colnames( KenSample), colnames( OH15FranklinDems))

