url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_New_Zealand_general_election"

#'
#'
#'
#'
#'
#'
#'
getWikiData <- function(url) {
    
    # read the table
    h <- htmlParse(GET(url))
    x <- readHTMLTable(h, encoding=getEncoding(h), stringsAsFactors=FALSE)[[1]]
    
    # extract events
    is_event <- sum(!is.na(x))
    
    # remove footnotes
    
    
    
    # dates
    
    return(x)
}

trim <- function(x) {
    
    x <- str_replace_all(x, "\\[(.*?)\\]", "") # remove any Wikipedia footnotes, e.g. [1]
    x <- str_trim(x) # trim leading/trailing spaces
    
    return(x)
}