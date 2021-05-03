
##  R Documentation

###############################################################################
##                                                                           ##
## JCRImpactFactor - Journal Citation Reports (JCR) Impact Factor             ##
##  find.IF.JCR                                                                         ##
## Author: Shahla Faisal shahla_ramzan@yahoo.com                             ##
##                                                                           ##
###############################################################################

#'
#'  find.IF.JCR Journal Citation Reports (JCR) Impact Factor
#'
#'
#' This function gives the Impact Factor of the journal(s) provided by Journal Citation Reports (JCR) Thomson Reuters
#' for the year 1997 to 2019. If the output shows a NA for a journal, it means that the journal has never been in JCR list.

#'
#'
#' @name find.IF.JCR
#' @aliases find.IF.JCR
#' @param journals  a \code{character vector} containing journals whose impact factor is required
#' @param year \code{scaler}, the year for which impact factor is required. If not mensioned, then impact factor for all year is displayed.
#' @param exact.match \code{logical}.
#'
#'
#' @keywords package JCR ImpactFactor clarivate analytics
#' @return  impact factor of the journal(s)
#' @export
#' @encoding UTF-8
#' @examples
#'    find.IF.JCR("ieee access")
#'    find.IF.JCR( "ieee access", year=2016)
#'
#'
#########    start of find.IF.JCR function       ########


find.IF.JCR <- function( journals, year, exact.match=TRUE)
{

  jcrlsit <- DataDupN

  journals <- unlist(lapply( journals, str_squish))   #remove white spaces

  if(!missing(year)) {year= year
  		    if (!is.numeric(year) ) stop("year should be a numeric, like 2019")
  		    if (! (year > 2009) || (year > 2019) ) stop("year should be 2010 to 2019")

  res <- data.frame( Reduce( rbind,  lapply(journals, find.impactfactor.year.and, year=year,jcr.list=jcrlsit, exact.match=exact.match)  ) )
  result <-  res
  } else {
    res <- data.frame( Reduce( rbind,  lapply(journals, find.impactfactor.year.and, jcr.list=jcrlsit,exact.match=exact.match)  ) )
    result <- subset(res, select=c("Full.Journal.Title",  "IF2010", "IF2011",  "IF2012",
                                   "IF2013", "IF2014",  "IF2015",  "IF2016",  "IF2017",  "IF2018","IF2019"))
  }
  if(nrow(take_rows_all_na(result, pct=1))>=1){
    num.journals.not.jcr <- nrow(take_rows_all_na(result, pct=1))  #number of journals not found in JCR
    name.journals.not.jcr <- journals[ index_rows_all_na(result, pct=1) ]   #print the names of "journals" not found in JCR
    print(paste("The following", num.journals.not.jcr, "journal(s) are not in JCR", sep=" "   )  )
    print(name.journals.not.jcr)
    result.final <- result[ - index_rows_all_na(result, pct=1), ]
  }else{
    result.final <- result
  }
  return(result.final )
}

