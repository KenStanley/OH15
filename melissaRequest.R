#
# melissaRequest 
#
#  1) Find all voter records that we are rejecting 
#     )) The idea being that I will skip those who have submitted NCOA forms but learn from the others 
#  2) Find all zip codes in all of our precincts 
#       

randomizationSS = "https://docs.google.com/spreadsheets/d/1pEInYA4W4yyaqqcMQcnrvxRKlZ3WHdH4HK539nrn4T0/edit#gid=99147741"
randomizationSheet = "OH15 within precinct randomization household universe"

inPrecinctRandomization = read_sheet_nice( ss=randomizationSS,
                                           sheet=randomizationSheet)


treatmentPrecincts = 
