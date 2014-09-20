elect <- read.csv("data/electorates.csv", stringsAsFactor=F)
elect$Electorate <- factor(elect$Electorate)
elect$Winner <- !is.na(elect$Winner)

lists <- read.csv("data/lists.csv", stringsAsFactors=F, check.names=F)

# lookup the electorate winners for each party, figure out who's left
get_mps <- function(p, electorate_winners, n) {
    names <- electorate_winners
    if (length(names) < n) {
        names <- c(names, paste(setdiff(lists[[p]], names), "(L)"))[1:n]
    }
    names
}

