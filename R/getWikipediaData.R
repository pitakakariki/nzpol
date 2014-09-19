url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_New_Zealand_general_election"

urls <- str_c("https://en.wikipedia.org/wiki/Opinion_polling_for_the_New_Zealand_general_election,_", c(2005, 2008, 2011))
urls <- c(urls, "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_New_Zealand_general_election")

library(httr) # for GET
library(stringr) # for fixing up [1]'s etc
library(plyr) # of course
library(XML) # for web scraping
library(magrittr) # for fun

opt_dashes <- "[\u002D\u2013\u2014\u2212]"
opt_spaces <- "[\u0020\u00A0]"

getAllData <- function(urls, ...) {

    # extract events later ...
    
    data_list <- llply(urls, getWikiData, ...)
    Reduce(function(x, y) merge(x, y, all=TRUE), data_list)
}

#'
#'
#'
#'
#'
#'
#'
getWikiData <- function(url, events=FALSE, dates=TRUE) {
    
    message(url)
    
    # read the table
    h <- htmlParse(GET(url))
    X <- readHTMLTable(h, encoding=getEncoding(h), stringsAsFactors=FALSE)[[1]]

    # trim footnotes[1] - they should be either in the first two columns or the header row
    names(X) <- fn_trim(names(X))
    X[["Poll"]] <- fn_trim(X[["Poll"]])
    X[["Date"]] <- fn_trim(X[["Date"]])
    
    # remove the extra header rows (first entry will be "Poll")
    X <- X[-which(X[["Poll"]] == "Poll"), ]   
    
    # extract events
    is_event <- which(is.na(X[["Date"]]))
    event_list <- X[is_event, 1]
    X <- X[-is_event, ]
    
    # make Poll a categorical variable, with "Election" the first level
    X[["Poll"]][str_detect(X[["Poll"]], "election")] <- "Election"
    X[["Poll"]] <- as.factor(X[["Poll"]])
    X[["Poll"]] <- reorder(X[["Poll"]], X[["Poll"]], function(x) if(x[1] == "Election") 1 else 2)
    
    # and make all the polling data numeric
    for(party in names(X)[-c(1,2)])
        X[party] <- suppressWarnings(as.numeric(X[[party]])) # suppress conversion to NA warnings

    # special handling for Internet/Mana
    null0 <- function(x) if(is.null(x)) 0 else ifelse(is.na(x), 0, x)
    
    IM <- rep(0, nrow(X)) +
        null0(X$Internet) +
        null0(X$Mana) +
        null0(X$`Internet Mana`)
    
    IM <- ifelse(IM==0, NA, IM)
    
    X[['Internet Mana']] <- IM
    
    
    if(events) {
        
        attr(X, "events") <- build_events(event_list)
    }
    
    if(dates) {
        
        X <- cbind(X, date(X$Date))
    }
    
    return(X)
}

fn_trim <- function(x) {
    
    x <- str_replace_all(x, "\\[(.*?)\\]", "") # remove any Wikipedia footnotes, e.g. [1]
    x <- str_trim(x) # trim leading/trailing spaces
    
    return(x)
}

build_events <- function(event_list) {
    
    events <- unlist(str_split(event_list, "\n")) # multiple events per table cell
    events <- str_split(events, opt_dashes, n=2) # format: date - text

    Date <- sapply(events, `[`, 1)
    Date <- as.Date(Date, "%d %b %Y")

    Event <- sapply(events, `[`, 2)
    Event <- str_trim(Event)

    data.frame(Date=Date, Event=Event, stringsAsFactors=FALSE)
}
