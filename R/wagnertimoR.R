


#' @title formatGermanNumber
#'
#' @description This function formats a 'german' style number into a classical (scientific) format with only a point as decimal delimiter.
#'
#' @param x - a 'german' number in the format like e.g. 123.456.789,09
#'
#' @return the converted input number with only a point as decimal delimiter. E.g. 123546789.09
#'
#' @examples
#' singlePointNumber <- formatGermanNumber("123.456.789,09")
#'
#' @export
#'
formatGermanNumber <- function(x){
  z <- gsub("[^0-9,.]", "", x)
  z <- gsub("\\.", "", z)
  gsub(",", ".", z)
}


#' @title dropColsInDF
#'
#' @description This function removes columns from a given data.frame. The columns have to be in a vector.
#'
#' @param df - a data.frame
#' @param namesVector - the vector (c()) with the names of data.frame which should be dropped.
#'
#' @return the given data.frame without the variables (columns) in the namesVector.
#'
#' @examples
#'
#' df <- data.frame(age = c(16,20,19), name = c("Tim", "Anne", "Mike"), weight = c(75,51,80), height = c(182,164,188))
#' df.new <- dropColsInDF(df, c("weight", "height"))
#'
#' @export
#'
dropColsInDF <- function(df, namesVector){
  return(df[ , !(names(df) %in% namesVector)])
}



#' @title keepColsInDF
#'
#' @description This function only keeps the columns of a given data.frame from a specified namesVector.
#'
#' @param df - a data.frame
#' @param namesVector - the vector (c()) with the names of data.frame which will be kept.
#'
#' @return the given data.frame only with the variables (columns) in the namesVector.
#'
#' @examples
#'
#' df <- data.frame(age = c(16,20,19), name = c("Tim", "Anne", "Mike"), weight = c(75,51,80), height = c(182,164,188))
#' df.new <- keepColsInDF(df, c("name"))
#'
#' @export
#'
keepColsInDF <- function(df, namesVector){
  return(df[ , names(df) %in% namesVector])
}



#' @title as.POSIXct.no.dst
#'
#' @description This function takes care of the daylight saving changes on last sunday of march (switch from CET to CEST) and october (switch from CEST to CET).
#' The switches either omit the 2am hour (march) or add an extra 2am hour (october). This function will allow you to add an extra 2am hour for CET in october (as seen in the example).
#' And if you have an 2am hour observation in march it automatically changes it to 3 am hour CEST and doesn't omit or drop it.
#'
#' @param x - the DateTime as character
#' @param tz - the time zone
#' @param format - specify the format of the input DateTime string. Default format: YYYY-MM-DD HH:MM:SS
#' @param offset - Default offset: +0100 to take care of the DST
#'
#' @return the given data.frame only with the variables (columns) in the namesVector.
#'
#' @examples
#'
#' # DST end
#' # Normal POSIXct sees 2 am hour only as CEST but omitts the secound 2am hour of CET
#' as.POSIXct("2016-10-30 02:00:00", tz = "Europe/Berlin")
#' #[1] "2016-10-30 02:00:00 CEST"
#' # To get the extra 2am hour (CET) use this function
#' as.POSIXct.no.dst("2016-10-30 02:00:00", tz = "Europe/Berlin")
#' #[1] "2016-10-30 02:00:00 CET"
#'
#' # DST start
#' # If you have a 2 am hour in march than this will normally omiited(left out) by POSIXct.
#' as.POSIXct.no.dst("2012-03-25 02:00:00", tz="Europe/Berlin")
#' #[1] "2012-03-25 03:00:00 CEST"
#'
#'
#' @export
#'
as.POSIXct.no.dst <- function (x, tz = "", format="%Y-%m-%d %H:%M:%S", offset="+0100", ...) {
  x <- paste(x, offset)
  format <- paste(format, "%z")
  as.POSIXct(x, tz, format=format, ...)
}


#' @title lastDayOfMonth
#'
#' @description This function returns the date ("YYYY-MM-DD") of the last specified day (Sunday == 1, Monday == 2, ... Saturday == 7) in a given month (January == 1 ...)
#'
#' @param day - Specify the last week day. Sunday == 1, Monday == 2, ... Saturday == 7
#' @param month - Determine the month of the date
#' @param year - Determine the year of the date
#'
#' @return date (Date object) of the last given week day in the month and year
#'
#' @examples
#'
#' # Give me the date of the last sunday in october 2016 --> DST date
#' lastDayOfMonth(1,10,2016)
#'
#' @export
#'
lastDayOfMonth <- function(day, month, year){
  library(lubridate)
  library(zoo)

  lastDate = as.Date(zoo::as.yearmon(paste(year,"-",month,"-01",sep = "")), frac = 1)
  # 1 = sunday , 2 = monday ... 7 saturday
  lastWeekDay = wday(lastDate)
  diff = lastWeekDay - day
  if(diff == 0) {
    return(lastDate)
  }
  else {
    # e.g target sunday = 1 and lastWeekDay monday = 2 --> diff 2 - 1 = 1 --> shift lastDate back 1 (diff) day(s)
    # e.g target sunday = 1 and lastWeekDay tuesday = 3 --> diff 3 - 1 = 2 --> shift lastDate back 2 (diff) day(s)
    # e.g target wednesday = 4 and lastWeekDay tuesday = 3 --> diff 3 - 4 = -1 --> if negative --> 7 - diff = 6 --->shift lastDate back 6 (diff) day(s)
    # e.g target tuesday = 3 and lastWeekDay monday = 2 --> diff 2 - 3 = -1 --> if negative --> 7 - diff = 6 --->shift lastDate back 6 (diff) day(s)
    if(diff < 0) {
      # shift lastDate back by 7 - diff
      shiftback = 7  + diff
    }
    else {
      # diff positive --> shift lastDate back by diff
      shiftback = diff
    }

    return(lastDate - shiftback)
  }
}




#' @title cseq
#'
#' @description This function enhances the base seq() function by allowing patterns for the by parameter. The by parameter can handle an array with a incrementation sequence.
#'
#' @param from - a number from where to start the sequence
#' @param to - a number where to end the sequence
#' @param by - Set an array as a by sequence for a pattern of incrementations. Status Quo only positive incrementations are allowed - no negative numbers in the array.
#'
#' @return a vector with a number sequence of by patterns
#'
#' @examples
#'
#' # Get a sequence from 36 to 106 by incrementing with 3 again with 3 and then with 2 and repeat this till 106
#' cseq(36, 106, c(3,3,2))
#' [1]  36  39  42  44  47  50  52  55  58  60  63  66  68  71  74  76  79  82  84  87  90  92  95  98
#' [25] 100 103 106
#'
#' @export
#'
cseq <- function(from, to, by){
  times <- (to-from) %/% sum(by)
  x <- cumsum(c(from, rep(by, times+1)))
  x[x<=to]
}



