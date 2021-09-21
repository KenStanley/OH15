#
# studyPriorityPrecincts 
#
rm(list=ls())
source("directory_path.R")
library(dplyr)
library(tictoc)
library(magrittr)

source("sheet_ops_nice.R")
source("countBallots.R")
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

if ( 0 ) {
  
  load(file="FranklinVoterFile.rds",verbose = TRUE)
  
  OH15only = FranklinVoterFile[which(FranklinVoterFile$CONGRESSIONAL_DISTRICT==15),] 
  
  June30_2021 = as.Date("2021-06-30")
  OH15only$age =   floor( as.integer(June30_2021  - as.Date(OH15only[,"DATE_OF_BIRTH"]))  / 365.25 )
  
  save(OH15only,file="OH15only.rds")
  
}
load( file="OH15only.rds", verbose=TRUE)

OH15only$apts = nchar(OH15only$RESIDENTIAL_SECONDARY_ADDR) > 0
OH15only$apts[which(is.na(OH15only$apts ))] = FALSE 

OH15only$voted2020 = OH15only$GENERAL.11.03.2020 == "X"

OH15only$household = paste( OH15only$RESIDENTIAL_ADDRESS1, OH15only$RESIDENTIAL_SECONDARY_ADDR, 
                            OH15only$RESIDENTIAL_ZIP)


franklinCountyPrecinctsSS = "https://docs.google.com/spreadsheets/d/1-m5VsqfXOS0Ox1rdC6UsL9pLMuCFDcdLipP22sdelTI/edit#gid=1301414353"

allMappablePrecinctsSheet = "allMappablePrecincts"

priorityPrecincts = read_sheet_nice( ss=franklinCountyPrecinctsSS, sheet=allMappablePrecinctsSheet )

priorityPrecinctsTrue = priorityPrecincts %>% filter( Experiment > 0 )

OH15priorityOnly = merge( OH15only,priorityPrecinctsTrue[,c("PRECINCT_NAME", "percentDem2020" )], by="PRECINCT_NAME" )

# View( OH15priorityOnly[which(OH15priorityOnly$RESIDENTIAL_ADDRESS1=="955 DORCHESTER WAY"),]) # youngest at that address is 22 years old, the 10 percentile is 35 yrs old, oldest is 59 
# View( OH15priorityOnly[which(OH15priorityOnly$RESIDENTIAL_ADDRESS1=="825 JUNCTION WAY"),]) # youngest at that address is 22 years old, the 10 percentile is 40 yrs old, the median is 30, oldest is 59 
View( OH15priorityOnly[which(OH15priorityOnly$RESIDENTIAL_ADDRESS1=="626 LEHMAN ST"),]) # typical young adult apt bldg 

OH15onlyDoors = group_by( OH15priorityOnly, PRECINCT_NAME, RESIDENTIAL_ZIP ,RESIDENTIAL_ADDRESS1, household ) %>% summarise(apts=mean(apts), voters=sum(voted2020))

aptBldgs = group_by( OH15onlyDoors,  PRECINCT_NAME,RESIDENTIAL_ZIP ,RESIDENTIAL_ADDRESS1 ) %>% summarise(apts=sum(apts), doors=n(), voters=sum(voters) ) 

sum(aptBldgs$apts>3)

aptBldgs$bigApts = (aptBldgs$apts>=10 ) + 0 
aptBldgs$smallApts = (aptBldgs$apts>2 & aptBldgs$apts<10 ) +0 
aptBldgs$tinyApts = (aptBldgs$apts>0 & aptBldgs$apts<=2 ) +0 
#
# %inaccesible = apts / (apts + 3 )  % 1/4 for one apt, 1/2 for three apts 
#
aptBldgs$fractionInaccessible = aptBldgs$apts / ( 3 + aptBldgs$apts )

aptBldgs$inaccessibleVoters = aptBldgs$fractionInaccessible * aptBldgs$voters

aptBldgs$aptSize = aptBldgs$smallApts + aptBldgs$bigApts *2+ aptBldgs$tinyApts*0.5
aptBldgs$mappableAddress = paste(aptBldgs$RESIDENTIAL_ADDRESS1, "Columbus OH", aptBldgs$RESIDENTIAL_ZIP)


inaccesibleByPrec = group_by( aptBldgs , PRECINCT_NAME ) %>% summarize( inaccessibleVoters = sum(inaccessibleVoters), 
                                                                        numVoters = sum( voters ), 
                                                                        mappableAddress = first(mappableAddress ))

# inaccesibleByPrec$mappableAddress = paste(inaccesibleByPrec$addr, "Columbus OH", inaccesibleByPrec$zip)


inaccesibleByPrec$fractionInaccessible = inaccesibleByPrec$inaccessibleVoters / inaccesibleByPrec$numVoters

precAptInfo = group_by( aptBldgs, PRECINCT_NAME, aptSize ) %>% summarize( numDoors = n(), numVoters = sum( voters ))

votersPerPrec = group_by( aptBldgs, PRECINCT_NAME ) %>% summarize( numAllDoors = n(), numAllVoters = sum( voters ))


precAptAndTotalInfo = merge( precAptInfo, votersPerPrec)

precAptAndTotalInfo$fractionVoters = precAptAndTotalInfo$numVoters /precAptAndTotalInfo$numAllVoters

# write_sheet_nice( inaccesibleByPrec, ss=franklinCountyPrecinctsSS, sheet="InaccesiblePrecincts")

# 33G 
# COLS 33-F 
# COLS 33-A
# MARBLECLIFF-A -skip 
# GRANDVIEW-B
# GRANDVIEW-E


GrandCApts = aptBldgs[ which(aptBldgs$PRECINCT_NAME=="GRANDVIEW-C" & aptBldgs$apts > 1),
                        c("PRECINCT_NAME" ,"voters"    , "mappableAddress")]
write_sheet_nice( GrandCApts, ss=franklinCountyPrecinctsSS, sheet="GrandCApts")

# Col14C = OH15priorityOnly[ which(OH15priorityOnly$PRECINCT_NAME=="COLS 40-C" ),               ]

# June30_2021 = as.Date("2020-10-02")
# 
# Col14C$age =   floor( as.integer(June30_2021  - as.Date(Col14C[,"REGISTRATION_DATE"]))  / 365.25 )



