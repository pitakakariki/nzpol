url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_New_Zealand_general_election"

#'
#'
#'
#'
#'
#'
#'
getWikiData <- function(url) {
    
    h <- htmlParse(GET(url))
    x <- readHTMLTable(h, encoding=getEncoding(h), stringsAsFactors=FALSE)[[1]]
    
    
    return(x)
}

