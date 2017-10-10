


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







#' @title join_left_fill
#'
#' @description This function proceeds a left join (dplyr packages) and fills missing values of the 'by variable' with a given value (instead of the default NA).
#' Importantly, the other valid NA values are preserved. BUT only for the data.frame on the left side!
#' NOTE: dplyr and tidyr package are needed!
#'
#' @param x - the left data.frame where the NAs are perserved
#' @param y - the right data.frame wherre NA values can occur due to the join. CAUTION valid NAs get also replaced by the fill value!
#' @param by - the by variable to join the data.frames
#' @param fill - custom fill value to replace the default NA in dplyrs join function
#'
#' @return the joined data.frame with filled 0 values
#'
#' @examples
#' dt1 <- data.table(x=c('a', 'b', 'c', 'd', 'e'), y=c(NA, 'w', NA, 'y', 'z'));
#' dt2 <- data.table(x=c('a', 'b', 'c'), new_col=c(1,2,3));
#'
#' left_join_fill(dt1, dt2, by = x, fill = 0)
#'
#' #  x    y new_col
#' #1 a <NA>       1
#' #2 b    w       2
#' #3 c <NA>       3
#' #4 d    y       0
#' #5 e    z       0
#'
#' @export
#'
left_join_fill <- function(x, y, by, fill = 0){
  library(dplyr)
  library(tidyr)

  z <- left_join(x, y, by = by)
  # Get the 'new' variables/row which were created by the joined to only set their NA values to the fill value
  tmp <- setdiff(names(z), names(x))
  z <- replace_na(z, setNames(as.list(rep(fill, length(tmp))), tmp))
  z
}




#' @title filterDataDynamically
#'
#' @description Filter a data frame dynamically based on a list of predicates, specified in the form column = list(binary_operator, rhs) (e.g. x = list(`<=`, 3) for x <= 3).
#'
#' @param df data frame to filter
#' @param controls list of filters (with optional operators)
#'
#' @return the filtered data.frame
#'
#' @examples
#'
#' # create example data
#' df = data.frame(
#'  x=sample(1:3, 100L, TRUE),
#'  y=runif(100L, 1, 5),
#'  z=sample(c('A', 'B', 'C'), 100L, TRUE)
#' )
#'
#' controls = list(x=2L, y=list(`<=`, 2.5), z=list(`!=`, 'C'))
#' filter_data(df, controls)
#'
#' @export
#'
filterDataDynamically = function(df, controls) {

  evaluate = function(predicate, value) {
    if (is.list(predicate)) {
      operator = predicate[[1L]]
      rhs = predicate[[2L]]
    } else {
      operator = `==`
      rhs = predicate
    }
    return(operator(value, rhs))
  }

  index = apply(
    mapply(evaluate, predicate=controls, value=df[names(controls)]), 1L, all
  )

  return(df[index, ])

}





#' @title subsetDynamic
#'
#' @description THis function subsets a a given data frame (df1) by specific columns on the values of the comparing data frame (df2).
#' So both data frames have to have those columns. Comparing operater is "==".
#'
#' @param df1 is the main data frame which has to be subsetted
#' @param df2 is the comparing data.frame
#' @param cols are the column names to compare, which have to occur in both data frames
#'
#' @return the data frame df1 subsetted by the values in df2
#'
#' @examples
#'
#' # create example data
#' df1 = data.frame(
#'  x=sample(1:3, 100L, TRUE),
#'  y=runif(100L, 1, 5),
#'  z=sample(c('A', 'B', 'C'), 100L, TRUE)
#' )
#'
#' df2 = data.frame(
#'  x=sample(1:3, 100L, TRUE),
#'  z=sample(c('A', 'B', 'C'), 100L, TRUE)
#' )
#'
#' # df1
#' # df2
#' subsetDynamic(df1, df2, c("x","z"))
#'
#'
#'
#' @export
#'
subsetDynamic <- function(df1, df2, cols) {

  for(i in 1:length(cols)) {

    df1 = df1[df1[, cols[i]] == df2[, cols[i]],]
  }

  df1
}





#' @title renameColsIndexedByName
#'
#' @description This function renames a vector of given column names. It can be used within a pipeline of dplyr operations.
#' It can not rename columns by given indices. Therefore @seealso renameColsIndexedByIndex
#'
#' @param dat is the data.frame with the old column names
#' @param oldnames specifies a string vector of the old column names which has to be renamed
#' @param newnames the names of the replacing strings in a vector. Note that the order matters!
#'
#' @return the input data.frame with the new column names
#'
#' @examples
#'
#' # create example data
#' df1 = data.frame(
#'  x=sample(1:3, 100L, TRUE),
#'  y=runif(100L, 1, 5),
#'  z=sample(c('A', 'B', 'C'), 100L, TRUE)
#' )
#'
#' head(renameColsIndexedByName(df1, c("x", "y"), c("xnew", "ynew")))
#'
#'
#'
#' @export
#'
renameColsIndexedByName <- function(dat, oldnames, newnames) {
  datnames <- colnames(dat)
  datnames[which(datnames %in% oldnames)] <- newnames
  colnames(dat) <- datnames
  dat
}






#' @title renameColsIndexedByIndex
#'
#' @description This function renames a vector of given column indices It can be used within a pipeline of dplyr operations.
#' It can not rename columns by given indices. Therefore @seealso renameColsIndexedByName
#'
#' @param dat is the data.frame with the old column names
#' @param indexOfoldnames specifies a integer vector which contain the old column names
#' @param newnames the names of the replacing strings in a vector. Note that the order matters!
#'
#' @return the input data.frame with the new column names
#'
#' @examples
#'
#' # create example data
#' df1 = data.frame(
#'  x=sample(1:3, 100L, TRUE),
#'  y=runif(100L, 1, 5),
#'  z=sample(c('A', 'B', 'C'), 100L, TRUE)
#' )
#'
#' head(renameColsIndexedByIndex(df1, c(1, 2), c("xnew", "ynew")))
#'
#'
#'
#' @export
#'
renameColsIndexedByIndex <- function(dat, indexOfoldnames, newnames) {
  datnames <- colnames(dat)
  datnames[indexOfoldnames] <- newnames
  colnames(dat) <- datnames
  dat
}






#' @title plotHistSkewnessAndTransOfVar
#'
#' @description This function plots a histogram of a given variable in a dataset with its skewness value and it plots the transformed (Yeo transformation) data.
#' The function needs ggplot2, grid.arrange and the caret packages.
#'
#' @param Data is the data.frame
#' @param variable character name of a column/variable in the Data data.frame which histogram and transformed histogram has to be plotted
#'
#' @return a grid.arrange plot with the histogram of the original variable and the (Yeo) transformed data
#'
#' @examples
#'
#'
#'
#'
#' @export
#'

plotHistSkewnessAndTransOfVar <- function(Data, variable) {


  skew = round(skewness(Data[,variable]),2)

  # Histogram
  g1 = ggplot(Data) +
    geom_histogram(aes_string(variable), color = "black", fill = "darkblue") +
    labs(y = "Anzahl an Beobachtungen") +
    annotate("label", x = max(Data[,variable])/2, y = 500, label = paste("Schiefe:", skew), color = "black", size = 6) +
    theme_bw() +
    theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm")) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.text.x = element_text(color = "black", size = 16)) +
    theme(axis.text.y = element_text(color = "black", size = 16)) +
    theme(axis.title.x = element_text(face = "bold", color = "#007749", size = 16)) +
    theme(axis.title.y = element_text(face = "bold", color = "#007749", size = 16)) +
    theme(legend.title = element_text(face = "bold", color = "#007749", size = 12))


  # Box-Cox Transformation --> lambda estimation ///// TransformedValue = [OriginalValue^lambda - 1] / lambda
  #
  #
  # NOTE: Box-Cox Transformation cant accomplish negative values --> use Yeo-Johnson transformation "YeoJohnson"
  #summary(Data$RenewableVsFossilRatio_lag_6)

  # you have to add a random second variable --> only numeric vars are allowed and not the selected one
  nums = sapply(Data, is.numeric)
  nums = names(Data[,nums])[names(Data[,nums]) != variable]
  # new data frame with the requested variable and a random numeric variable to calc the transformation
  dat = Data[, c(nums[length(nums)], variable)]

  t = preProcess(dat, method = c("center", "scale", "YeoJohnson"))
  lambda = round(as.numeric(t$yj[[variable]]$lambda), 2)
  # Get the transformed values with predict method


  t = predict(t, dat)
  #head(t)
  skew = round(skewness(t[,variable]),2)


  #   # Histogram
  g2 = ggplot(t) +
    #geom_histogram(aes(ENTSOE_Generation_Forecast_WindOffshore_Netzregelverbund), color = "black", fill = "darkblue") +
    #geom_histogram(aes(ENTSOE_Load_Error_Sum_Netzregelverbund_lag_6), color = "black", fill = "darkblue") +
    #geom_histogram(aes(ENTSOE_Load_Forecast_Sum_Netzregelverbund), color = "black", fill = "darkblue") +
    #geom_histogram(aes(EPEX_DayAhead_BasePrice), color = "black", fill = "darkblue") +
    #geom_histogram(aes(EPEX_DayAhead_BaseVolume), color = "black", fill = "darkblue") +
    geom_histogram(aes_string(variable), color = "black", fill = "darkblue") +
    #geom_vline(xintercept = 1, color = "black") +
    labs(y = "Anzahl an Beobachtungen") +
    annotate("label", x = 2, y = 200, label = paste("Schiefe: ", skew, "\nLambda: ", lambda, sep = ""), color = "black", size = 6) +
    theme_bw() +
    theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm")) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.text.x = element_text(color = "black", size = 16)) +
    theme(axis.text.y = element_text(color = "black", size = 16)) +
    theme(axis.title.x = element_text(face = "bold", color = "#007749", size = 16)) +
    theme(axis.title.y = element_text(face = "bold", color = "#007749", size = 16)) +
    theme(legend.title = element_text(face = "bold", color = "#007749", size = 12))


  grid.arrange(g1, g2, ncol = 2)
  #g2

}



