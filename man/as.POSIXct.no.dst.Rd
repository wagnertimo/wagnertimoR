% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wagnertimoR.R
\name{as.POSIXct.no.dst}
\alias{as.POSIXct.no.dst}
\title{as.POSIXct.no.dst}
\usage{
\method{as.POSIXct}{no.dst}(x, tz = "", format = "\%Y-\%m-\%d \%H:\%M:\%S",
  offset = "+0100", ...)
}
\arguments{
\item{x}{- the DateTime as character}

\item{tz}{- the time zone}

\item{format}{- specify the format of the input DateTime string. Default format: YYYY-MM-DD HH:MM:SS}

\item{offset}{- Default offset: +0100 to take care of the DST}
}
\value{
the given data.frame only with the variables (columns) in the namesVector.
}
\description{
This function takes care of the daylight saving changes on last sunday of march (switch from CET to CEST) and october (switch from CEST to CET).
The switches either omit the 2am hour (march) or add an extra 2am hour (october). This function will allow you to add an extra 2am hour for CET in october (as seen in the example).
And if you have an 2am hour observation in march it automatically changes it to 3 am hour CEST and doesn't omit or drop it.
}
\examples{

# DST end
# Normal POSIXct sees 2 am hour only as CEST but omitts the secound 2am hour of CET
as.POSIXct("2016-10-30 02:00:00", tz = "Europe/Berlin")
#[1] "2016-10-30 02:00:00 CEST"
# To get the extra 2am hour (CET) use this function
as.POSIXct.no.dst("2016-10-30 02:00:00", tz = "Europe/Berlin")
#[1] "2016-10-30 02:00:00 CET"

# DST start
# If you have a 2 am hour in march than this will normally omiited(left out) by POSIXct.
as.POSIXct.no.dst("2012-03-25 02:00:00", tz="Europe/Berlin")
#[1] "2012-03-25 03:00:00 CEST"


}
