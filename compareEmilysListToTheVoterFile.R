#
# compareEmilysListToTheVoterFile
#
library( googlesheets4 )
library( magrittr )
library( dplyr )
source("sheet_ops_nice.R")
# setwd("/Users/kenstanley/Google Drive/Rcode2019Mac/OH15Rcode/OH15")


# load( file="OH15FranklinDems.rds", verbose = TRUE )


EmilyExportSept24SS = "https://docs.google.com/spreadsheets/d/150t3476tPpfHUCf_wNxsFoTHZWfCF7uJoVxB531Rm5s/edit#gid=1853794895"
# EmilyExportSept24 = read_sheet_nice(ss=EmilyExportSept24SS, sheet="StandardText20210924-4157859556")

EmilyExportSept24$FIRST_NAME = toupper( EmilyExportSept24$FirstName)
EmilyExportSept24$LAST_NAME = toupper( EmilyExportSept24$LastName)
EmilyExportSept24$MIDDLE_NAME = toupper( EmilyExportSept24$MiddleName)
EmilyExportSept24$SUFFIX = toupper( EmilyExportSept24$Suffix)

EmilyExportSept24$StreetNum = as.numeric(lapply(EmilyExportSept24$Address,
                                                function(x) strsplit(as.character(x)," ")[[1]][1] ) )

# 
# EmilyExportSept24$PRECINT_NAME = toupper(lapply(EmilyExportSept24$PrecinctName,
#                                           function(x) strsplit(as.character(x)," \(")[[1]][1] ) )
# 

EmilyExportSept24$MIDDLE_NAME[which(is.na( EmilyExportSept24$MIDDLE_NAME))] = "" 
EmilyExportSept24$SUFFIX[which(is.na( EmilyExportSept24$SUFFIX))] = "" 


EmilyExportSept24 %<>% filter( !is.na(FIRST_NAME ))

length( which( grepl(" ",EmilyExportSept24$FIRST_NAME) | 
                 grepl(" ",EmilyExportSept24$LAST_NAME)))


May5_2021 = as.Date("2021-05-05")
OH15FranklinDems$regsSinceMay  = as.Date(OH15FranklinDems$REGISTRATION_DATE) > May5_2021


Jan1_2021 = as.Date("2021-01-01")
OH15FranklinDems$regsSinceJan  = as.Date(OH15FranklinDems$REGISTRATION_DATE) > Jan1_2021


Mar3_2021 = as.Date("2021-03-03")
OH15FranklinDems$regsSinceMar  = as.Date(OH15FranklinDems$REGISTRATION_DATE) > Mar3_2021


Apr4_2021 = as.Date("2021-04-04")
OH15FranklinDems$regsSinceApr  = as.Date(OH15FranklinDems$REGISTRATION_DATE) > Apr4_2021


Nov11_2020 = as.Date("2020-11-11")
OH15FranklinDems$regsSinceNov20  = as.Date(OH15FranklinDems$REGISTRATION_DATE) > Nov11_2020


OCT1_2021 = as.Date("2020-10-01")
OCT2_2021 = as.Date("2020-10-02")
OCT30_2021 = as.Date("2020-10-30")

OH15FranklinDems$age = as.numeric(OCT2_2021 - 
                                    as.Date( OH15FranklinDems$DATE_OF_BIRTH) ) / 365.25


targetSmartMatchedToVoterFile = merge( EmilyExportSept24, OH15FranklinDems , 
                                       by= c( "PRECINCT_NAME", "FIRST_NAME", "LAST_NAME", "MIDDLE_NAME" , "SUFFIX" ))



MissingFromEmilyExportSept24 = anti_join( OH15FranklinDems, EmilyExportSept24, 
                                          by= c( "PRECINCT_NAME", "FIRST_NAME", "LAST_NAME", "MIDDLE_NAME" , "SUFFIX" ) )

targetPrecincts = group_by( EmilyExportSept24, PRECINCT_NAME ) %>% summarize( EmilyExportVotersInPrecinct = n() )

MissingFromEmilyExportSept24InTargetPrecincts = merge( MissingFromEmilyExportSept24, 
                                                       targetPrecincts, by="PRECINCT_NAME")

OCT1_2021 = as.Date("2020-10-01")
OCT2_2021 = as.Date("2020-10-02")
OCT30_2021 = as.Date("2020-10-30")

MissingFromEmilyExportSept24InTargetPrecincts$age = as.numeric(OCT2_2021 - 
                                                                 as.Date( MissingFromEmilyExportSept24InTargetPrecincts$DATE_OF_BIRTH) ) / 365.25
# MissingFromEmilyExportSept24InTargetPrecincts has 18737 rows 
MissingFromEmilyA = MissingFromEmilyExportSept24InTargetPrecincts %>% 
  filter( PARTY_AFFILIATION!="R")

# MissingFromEmilyA has 15732 rows (we threw out 3005 Rs )
MissingFromEmilyB = MissingFromEmilyA %>% filter( GENERAL.11.03.2020 == "X" )

# MissingFromEmilyB has 10,534,  rows (we threw out 5198 who did not vote in 2020 ) 
MissingFromEmilyC = MissingFromEmilyB %>% filter( GENERAL.11.05.2019 != "X" )
# MissingFromEmilyC has 7108 rows (we threw out 3,4XX who voted in 2019 ) 

MissingFromEmilyD = MissingFromEmilyC %>% filter( PRIMARY.03.17.2020 != "D" )
# MissingFromEmilyD has 6515 rows (we threw out 93 who voted in Mar 2020 ) 

# MissingFromEmilyE has 6457 rows (we threw out 58 who registered since Mar 2021 )
MissingFromEmilyE = MissingFromEmilyD %>% filter( !regsSinceMar )



MissingFromVoterFile = anti_join( EmilyExportSept24, OH15FranklinDems, 
                                  by= c( "PRECINCT_NAME", "FIRST_NAME", "LAST_NAME", "MIDDLE_NAME" , "SUFFIX" ) )



browser()

MissingFromEmilyCOLS08D = MissingFromEmilyE %>% filter( PRECINCT_NAME=="COLS 08-D")

load( file="oh15ForPowerTest.rds", verbose=TRUE)

justThe2021Prob = oh15ForPowerTest[,c("SOS_VOTERID","predicted2021turnout")]
MissingFromEmilyF = merge( MissingFromEmilyE,justThe2021Prob,
                           by="SOS_VOTERID")

targetSmartMatchedToVoterFileF =  merge( targetSmartMatchedToVoterFile,justThe2021Prob,
                                         by="SOS_VOTERID",all.x=TRUE)

mean(targetSmartMatchedToVoterFileF$predicted2021turnout)

mean(MissingFromEmilyF$predicted2021turnout)

write_sheet_nice( MissingFromEmilyCOLS08D, ss=EmilyExportSept24SS, sheet="MissingFromSept24Export COLS 08-D")


MatchesFromMissingCols08D = merge( MissingFromEmilyExportSept24[(which(MissingFromEmilyExportSept24$PRECINCT_NAME=="COLS 08-D")),],
                                   MissingFromVoterFile, 
                                   by= c( "FIRST_NAME", "LAST_NAME")) 



colsFromBoth =  c(  "SOS_VOTERID", "FIRST_NAME"   ,  "LAST_NAME"  , "PRECINCT_NAME.x" ,
                   "PRECINCT_NAME.y"  ,  "SOS_VOTERID" ,              
                   "MIDDLE_NAME.x"  , "MIDDLE_NAME.y" ,  "SUFFIX.x"   ,  "SUFFIX.y"  ,
                   "RESIDENTIAL_ADDRESS1" , "RESIDENTIAL_SECONDARY_ADDR", "Address"   )            


ugh = group_by(MatchesFromMissingCols08D, PRECINCT_NAME.x) %>% summarise( numVotersMissingMatched=n() )
MissingFromVoterFileByPrecinct = group_by(MissingFromVoterFile, PRECINCT_NAME) %>% summarise( numVotersMissingMatched=n() )


browser()
View( MatchesFromMissingCols08D[which(MatchesFromMissingCols08D$PRECINCT_NAME=="COLS 33-A"),
                                colsFromBoth])

View( EmilyExportSept24)



sum(EmilyExportSept24$PRECINCT_NAME == "COLS 08-D")


