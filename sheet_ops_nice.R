
library(googlesheets4)
read_sheet_nice <- function(ss, sheet, initWaitTime=1,maxWaitTime=256, notify=TRUE, skip=0, col_types=NULL) {

  # Wrapper function for read_sheet()
  # If no errors, returns the same structure as read_sheet
  # read_sheet() often fails with an error code, RESOURCE_EXHAUSTED, resulting from too many requests within 100 seconds. 
  # This might be because these calls are seen as coming from a generic account, though we are not sure of that.
  # If a resource exhausted error occurs, it waits and retries. First wait time is initWaitTime
  # Second wait time is doubled, and so on. If the next retry would exceed maxWaitTime, it exits 
  # returning the string 'max wait time exceeded...'
  # Also handles an error where resoure is not found by returning a descriptive string
  # If any other error occurs, it returns the error data structure
  
  # Patrick Riehl
  # 2020-08-23
  
  resEx=TRUE
  waitTime = initWaitTime
  while(resEx){
    resEx=FALSE
    result <- tryCatch({read_sheet(ss, sheet, skip=skip, col_types=col_types )}
                       , error = function(err) {
                         if ( grepl("RESOURCE_EXHAUSTED", err)) { 
                             return("resourceExhausted") 
                         }
                           else if (grepl("resource is not found",err) || grepl("No sheet found",err)) {
                             return("not found")
                           } else {
                            return(err)
                           }
                       }
                   )
    
    if(typeof(result)=="character") {
      if(result=="resourceExhausted"){
        if(waitTime>maxWaitTime) {
          return("ERROR: Max wait time exceeded, exiting without sheet read")
        } else {
          if(notify) {print(paste("Resource exhausted, waiting ",waitTime," sec"))}
          Sys.sleep(waitTime)
          waitTime = waitTime * 2
          resEx = TRUE
          }
      } else if (result=="not found") {
        return(paste("ERROR: Did not find sheet", sheet, " in spreadsheet ",ss))
      }
    } else {return(result)}
  }
  }



write_sheet_nice <- function(data, ss=NULL, sheet=NULL, initWaitTime=1,maxWaitTime=256, notify=TRUE) {
  
  # Wrapper function for write_sheet() aka sheet_write()
  # If no errors, returns the same structure as read_sheet
  # write_sheet() often fails with an error code, RESOURCE_EXHAUSTED, resulting from too many requests within 100 seconds. 
  # This might be because these calls are seen as coming from a generic account, though we are not sure of that.
  # If a resource exhausted error occurs, it waits and retries. First wait time is initWaitTime
  # Second wait time is doubled, and so on. If the next retry would exceed maxWaitTime, it exits 
  # returning the string 'max wait time exceeded...'
  # Also handles an error where resoure is not found by returning a descriptive string
  # If any other error occurs, it returns the error data structure
  
  # Patrick Riehl
  # 2020-08-23
  
  resEx=TRUE
  waitTime = initWaitTime
  while(resEx){
    resEx=FALSE
    result <- tryCatch({write_sheet(data, ss, sheet)}
                       , error = function(err) {
                         if ( grepl("RESOURCE_EXHAUSTED", err)) { 
                           return("resourceExhausted") 
                         }
                         else if (grepl("NOT_FOUND",err)) {
                           return("not found")
                         } else {
                           return(err)
                         }
                       }
    )
    
    if(typeof(result)=="character") {
      if(result=="resourceExhausted"){
        if(waitTime>maxWaitTime) {
          return("ERROR: Max wait time exceeded, exiting without sheet write")
        } else {
          if(notify) {print(paste("Resource exhausted, waiting ",waitTime," sec"))}
          Sys.sleep(waitTime)
          waitTime = waitTime * 2
          resEx = TRUE
        }
      } else if (result=="not found") {
        return(paste("ERROR: Did not find sheet", sheet, " in spreadsheet ",ss))
      }
    } else {return(result)}
  }
}




range_write_nice <- function(data, ss=NULL, sheet=NULL, range=NULL, initWaitTime=1,
                             maxWaitTime=512, notify=TRUE, col_names = TRUE) {
  
  # Wrapper function for range_write() aka sheet_write()
  # If no errors, returns the same structure as read_sheet
  # range_write() often fails with an error code, RESOURCE_EXHAUSTED, resulting from too many requests within 100 seconds. 
  # This might be because these calls are seen as coming from a generic account, though we are not sure of that.
  # If a resource exhausted error occurs, it waits and retries. First wait time is initWaitTime
  # Second wait time is doubled, and so on. If the next retry would exceed maxWaitTime, it exits 
  # returning the string 'max wait time exceeded...'
  # Also handles an error where resoure is not found by returning a descriptive string
  # If any other error occurs, it returns the error data structure
  
  # Patrick Riehl
  # 2020-08-23
  
  resEx=TRUE
  waitTime = initWaitTime
  while(resEx){
    resEx=FALSE
    result <- tryCatch({range_write(data=data, ss=ss, sheet=sheet, range=range, col_names=col_names)}
                       , error = function(err) {
                         if ( grepl("RESOURCE_EXHAUSTED", err)) { 
                           return("resourceExhausted") 
                         }
                         else if (grepl("NOT_FOUND",err)) {
                           return("not found")
                         } else {
                           return(err)
                         }
                       }
    )
    
    if(typeof(result)=="character") {
      if(result=="resourceExhausted"){
        browser() # I want to see if the error gives me useful information
        if(waitTime>maxWaitTime) {
          return("ERROR: Max wait time exceeded, exiting without range write")
        } else {
          if(notify) {print(paste("Resource exhausted, waiting ",waitTime," sec"))}
          Sys.sleep(waitTime)
          waitTime = waitTime * 2
          resEx = TRUE
        }
      } else if (result=="not found") {
        return(paste("ERROR: Did not find sheet", sheet, " in spreadsheet ",ss))
      }
    } else {return(result)}
  }
}






