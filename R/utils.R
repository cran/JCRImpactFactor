

library(stringr)

find.impactfactor.year.and <- function(journal, year,
                                       jcr.list , exact.match=TRUE, output=FALSE, ...  )
{
  jcrlsit <- DataDupN
  mdf <- jcr.list
  if( !missing(journal) )     journal = journal
  if(exact.match==TRUE){
    res <- mdf[ match(x=tolower(journal) , tolower(mdf$"Full Journal Title"))    , ]
    if(output) print(res)
    if( is.na(res$`Full Journal Title` ) )
    {
      if(output) print(res$`Full Journal Title`)
      if(output) print(journal)
      if ( grepl("and", journal))
      {
        journal=gsub("and", "&", journal)  ;if(output) print(journal)
      }else{
        if( grepl("&", journal))  journal=gsub("&", "and", journal)   ;if(output) print(journal)
      }
      res <- mdf[ match(x=tolower(journal) , tolower(mdf$"Full Journal Title"))    , ]
    }
  } else{
    res <-  mdf[ grepl(journal , mdf$"Full Journal Title"    , ignore.case = TRUE), ]
  }

  if( !missing(year)  ) {
    year=year
    res.all <- res
    res <- subset(res.all,  select =c("Full Journal Title", paste("IF",year,sep = "")  ) )
  }
  return(res)
}




drop_rows_all_na <- function(x, pct=1) x[!rowSums(is.na(x)) >= ncol(x)*pct,]
take_rows_all_na <- function(x, pct=1) x[rowSums(is.na(x)) >= ncol(x)*pct,]
index_rows_all_na <- function(x, pct=1) which(rowSums(is.na(x)) >= ncol(x)*pct)


