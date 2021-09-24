#
#  
#
rm( list=ls())
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
source("countBallots.R")
source("sheet_ops_nice.R")




# load( file="OH15VF.rds", verbose=TRUE )
# 
# OH15FranklinDems = OH15VF %>% filter( `County Name`=="Franklin" & Biden > 1.5 * Trump )
# save(OH15FranklinDems,file="OH15FranklinDems.rds")
load( file="OH15FranklinDems.rds", verbose = TRUE )
# load( file="COLS33Voters.rds")
source("sheet_ops_nice.R")

# OH15FranklinDems = COLS33Voters

OH15FranklinDems$votedin202020 = OH15FranklinDems$GENERAL.11.03.2020=="X"
OH15FranklinDems$Rep = OH15FranklinDems$PARTY_AFFILIATION=="R" & OH15FranklinDems$votedin2020
OH15FranklinDems$Dem = OH15FranklinDems$PARTY_AFFILIATION=="D" & OH15FranklinDems$votedin2020
OH15FranklinDems$household = paste( OH15FranklinDems$RESIDENTIAL_ADDRESS1,  OH15FranklinDems$RESIDENTIAL_SECONDARY_ADDR)

OH15FranklinDems$score = OH15FranklinDems$Dem - OH15FranklinDems$Rep # 1 = Dem,  0 = neither, -1 = Rep 

OH15FranklinDems$voteScore = 
  
  
  Aug30_2021 = as.Date("2021-08-30")
OH15FranklinDems$age =   floor( as.integer(Aug30_2021  - as.Date(OH15FranklinDems[,"DATE_OF_BIRTH"]))  / 365.25 )



OH15FranklinDems$nameAgeAddress = paste(proper(OH15FranklinDems$FIRST_NAME),
                                        " ",substr(OH15FranklinDems$MIDDLE_NAME,1,1),
                                        " ",proper(OH15FranklinDems$LAST_NAME),
                                        " ",proper(OH15FranklinDems$SUFFIX),
                                        " [",OH15FranklinDems$age,"] ",proper(OH15FranklinDems$RESIDENTIAL_ADDRESS1),
                                        " ",proper(OH15FranklinDems$RESIDENTIAL_SECONDARY_ADDR), sep="" )



allColNames = colnames( OH15FranklinDems)
primaryColNames = allColNames[ which( grepl("PRIMARY" , allColNames))]
generalColNames = allColNames[ which( grepl("GENERAL" , allColNames))]

OH15FranklinDems$numPrimaryDVotes = countBallots(OH15FranklinDems[,primaryColNames],"D")
OH15FranklinDems$numPrimaryRVotes = countBallots(OH15FranklinDems[,primaryColNames],"R")
OH15FranklinDems$numPrimaryXVotes = countBallots(OH15FranklinDems[,primaryColNames],"X")
OH15FranklinDems$numGeneralVotes = countBallots(OH15FranklinDems[,generalColNames],"X")

demVotePoints = 4
repVotePoints = -6 
votePoints = 1 

OH15FranklinDems$voteScore = demVotePoints * OH15FranklinDems$numPrimaryDVotes + 
  repVotePoints * OH15FranklinDems$numPrimaryRVotes + 
  votePoints * ( OH15FranklinDems$numPrimaryXVotes + OH15FranklinDems$numGeneralVotes )  


OH15FranklinDems$addressSansOnehalf = gsub( "1/2 ","",OH15FranklinDems$RESIDENTIAL_ADDRESS1)


OH15FranklinDems$streetNum = as.numeric(lapply(OH15FranklinDems$addressSansOnehalf,
                                          function(x) strsplit(as.character(x)," ")[[1]][1] ) )


OH15FranklinDems$streetName = as.character(lapply(OH15FranklinDems$addressSansOnehalf,
                                                  function(x) strsplit(as.character(x)," ")[[1]][2] ) )

streetTooShort = nchar(OH15FranklinDems$streetName) < 3

OH15FranklinDems$streetName[streetTooShort] = paste(OH15FranklinDems$streetName[streetTooShort],
                                                    as.character(lapply(OH15FranklinDems$addressSansOnehalf[streetTooShort],
                                                                        function(x) strsplit(as.character(x)," ")[[1]][3] ) ) )

houseScores = group_by( OH15FranklinDems, RESIDENTIAL_ADDRESS1, household ) %>% 
  summarise( numVoters = sum ( votedin202020 ), partyScore = sum( score ))


lockedHouseholds33dSS = "https://docs.google.com/spreadsheets/d/11QUN4JynMg2Vo1_gQkQLSdKHnYoUkYWzMorOl3ZIULg/edit"


lockedHouseholds33d = read_sheet_nice( ss=lockedHouseholds33dSS,  sheet="All" ) 
colnames(lockedHouseholds33d)[2] = "inaccessible"

houseScores$target = houseScores$numVoters > 0 & houseScores$partyScore >= 0 
houseScores$address = paste( houseScores$RESIDENTIAL_ADDRESS1, "Columbus OH")

browser()

houseScoresNew = merge( houseScores,lockedHouseholds33d[,c("address","inaccessible")], 
                        by="address", all.x = TRUE )

houseScoresNew$locked = houseScoresNew$inaccessible=="l"
houseScoresNew$locked[is.na(houseScoresNew$locked)] = FALSE 

houseScores = houseScoresNew %>% filter( !locked )

sum( houseScores$numVoters > 0  )
sum(houseScores$partyScore >= 0   )
sum(houseScores$target)

OH15FranklinDemsWithHouseScore = merge( OH15FranklinDems, houseScores )

ThisPrecinct = "UA 1-D"

thisPrecinctToCanvass = OH15FranklinDemsWithHouseScore %>% 
  filter( PRECINCT_NAME==ThisPrecinct ) %>% 
  filter( target )

thisPrecinctToCanvass$support = ""
thisPrecinctToCanvass$triplees = ""
thisPrecinctToCanvass$notes = ""
maxStreetNum = 100000



browser()

streetsToPrint = group_by( thisPrecinctToCanvass, streetName) %>% 
  summarise( numDoors = n_distinct(household))


col33Bss = "https://docs.google.com/spreadsheets/d/1cnuxKXXJwb6Rexk5CaL8_4LF4Npg6mJe2-Z8lUCjvJg/edit#gid=1696535020"

Sept20ss = "https://docs.google.com/spreadsheets/d/1PR_z9iClUsHTKOQS3G7cOu-YwdONWHS-D5o-OC_6u5E/edit#gid=0"

UA1Ass= "https://docs.google.com/spreadsheets/d/1vLOs5g0lbyyPahse5kj5385ZBVMRBp5jA7XO6gK-3x0/edit#gid=0"

write_sheet_nice(streetsToPrint, ss=Sept20ss, sheet="Streets1D")

# streetsToUse = read_sheet_nice( ss=UA1Ass, sheet="Streets")

# streetsToUseA =  streetsToUse[,c("streetName","Area" )]

# thisPrecinctToCanvassPlus = merge( thisPrecinctToCanvass, streetsToUseA)
# 
# thisPrecinctToCanvassPlus$North = thisPrecinctToCanvassPlus$Area=="North" |
#   ( ( thisPrecinctToCanvassPlus$streetNum > 1600 ) & thisPrecinctToCanvassPlus$Area=="NS" ) |
#   ( ( mod(thisPrecinctToCanvassPlus$streetNum,2)==0 ) & thisPrecinctToCanvassPlus$Area=="Divide" ) 
#   
# 
# thisPrecinctToCanvassPlus$streetName[thisPrecinctToCanvassPlus$North] =
#   paste( "N", thisPrecinctToCanvassPlus$streetName)[thisPrecinctToCanvassPlus$North]
# 
# 
# streetsToPrintII = group_by( thisPrecinctToCanvassPlus, streetName) %>% 
#   summarise( numDoors = n_distinct(household))

# write_sheet_nice(streetsToPrint, ss=Sept20ss, sheet="streetsToPrintII")


colsToPrint = c( "SOS_VOTERID",  "nameAgeAddress" ,  "notes", "support"  , "triplees" ,   "mappableAddress" ,   
                 "voteScore",  "streetName" ,  "streetNum" , "household")

# write_sheet_nice(thisPrecinctToCanvass[,colsToPrint], ss=Sept20ss, sheet="UA1DtoStudy")

browser()


extraRows = group_by( thisPrecinctToCanvass, household ) %>% 
  summarise( streetName = first( streetName), 
             streetNum = mean( streetNum),
             )

setdiff( colnames( extraRows), colsToPrint)
setdiff( colsToPrint, colnames( extraRows))
extraRows$SOS_VOTERID = ""
extraRows$support = ""
extraRows$triplees = ""
extraRows$notes = ""
extraRows$nameAgeAddress = ""
extraRows$mappableAddress = ""
extraRows$voteScore = -1 

canvassWithExtraRows = rbind( thisPrecinctToCanvass[,colsToPrint], extraRows)


setdiff(colsToPrint,colnames(thisPrecinctToCanvass) )

write_sheet_nice(canvassWithExtraRows[,colsToPrint], ss=Sept20ss, sheet="UA1DtoCanvass")


splitTest = FALSE 
if ( splitTest ) {
  
  
  numVoters = 4000
  numTreatHouses = numVoters * .9  
  numControlHouses = numVoters * .1
  baseTurnout = .3 
  turnoutUp = .08
  
  treatment = rbinom( n=10000, size=numTreatHouses, prob = baseTurnout + turnoutUp )  
  
  control = rbinom( n=10000, size=numControlHouses, prob = baseTurnout )  
  
  sum( ( treatment/ numTreatHouses - control / numControlHouses )  > turnoutUp/(2) )  
}

