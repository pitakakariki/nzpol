
edates <- as.Date(c("2002-07-27", "2005-09-17", "2008-11-08", "2011-11-26", "2014-09-20", "2511-05-01"))

#
# Allow for faster movement closer to elections
#
time_dilation <- function(x, A=9, k=5) {
    
    k <- exp(-k)
    
    # calculate x* for each election
    
    e <- e.star <- as.numeric(edates)
    
    for(i in seq_along(edates)[-1]) {
        
        e.star[i] <- e.star[i-1] + (e[i] - e[i-1]) + (A/k)
    }
    
    # x -> x*
    
    x.star <- x
    
    for(i in seq_along(x)) {
        
        # prev election?
        j <- sum(e <= x[i])

    #cat(i);cat(j)    
        
        x.star[i] <- e.star[j] + (x[i] - e[j]) + (A/k)*exp((x[i]-e[j+1])*k)
        
    }
    
    
    return(x.star)
}


pwt <- function(poll) ifelse(poll=="Election", 1000, ifelse(poll=="Herald-DigiPoll", 0.75, 1))