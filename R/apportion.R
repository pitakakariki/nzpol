Hamilton <- function(votes, total) {
    
    alloc <- votes / sum(votes) * total
    
    integer <- floor(alloc)
    fraction <- alloc - integer
    
    n <- total - sum(integer)
    extra <- head(order(fraction, decreasing=TRUE), n)
    
    alloc <- integer
    alloc[extra] <- alloc[extra] + 1
    
    if(sum(alloc) != total) stop("Allocation error.")
    
    return(alloc)
}

quotient <- function(votes, total, start) {
    
    f <- function(s) 2*s + 1

    if(missing(start)) start <- Hamilton(votes, total)
    alloc <- start

    stopifnot(sum(alloc)==total)
    stopifnot(length(alloc)==length(votes))
    
    for(it in c(0, seq_len(total))) {

        # Q0 are the quotients justifying
        # the current seat allocations
        Q0 <- votes / f(alloc - 1)
        Q0[votes == 0] <- 0
        Q0[alloc == 0] <- Inf
        
        # Q1 are the quotients for additional seats
        Q1 <- votes / f(alloc)
        
        # All of the Q0 should be larger than all of the Q1,
        # otherwise parties with larger Q1 quotients need more seats.
        if(min(Q0) > max(Q1)) break

        # The correct allocation should be guaranteed after
        # this many adjustments. Final pass through the loop
        # is only to test that the allocation is correct.
        stopifnot(it < total)
        
        m <- median(c(Q0, Q1))
        
        inc <- (Q1 >= m)
        dec <- (Q0 < m)

        stopifnot(sum(inc)==sum(dec)) # shouldn't be possible?
        
        alloc[inc] <- alloc[inc] + 1
        alloc[dec] <- alloc[dec] - 1
    }

    stopifnot(sum(alloc)==total) # don't allocate wrong number
        
    attr(alloc, "iterations") <- it

    return(alloc)    
}

quotient.old <- function(votes, total, H=TRUE) {
    
    f <- function(s) ifelse(s < 0, 0, 2*s + 1)
    
    alloc <- rep(0, length(votes))
    alloc[1] <- total
    
    if(H) alloc <- Hamilton(votes, total)
    
    it <- 0
    
    Q1 <- Q0 <- NULL
    
    test <- function(alloc) {
        
        Q0 <<- votes / f(alloc - 1)
        Q1 <<- votes / f(alloc)

    #print(alloc)    
    #print(Q0)
    #print(Q1)
        
        min(Q0) >= max(Q1)
    }
    
    while(!test(alloc)) {

        it <- it + 1
        
        m <- median(c(Q0, Q1))
        
        inc <- (Q1 >= m)
        dec <- (Q0 < m)

        stopifnot(sum(inc)==sum(dec))
        
        alloc[inc] <- alloc[inc] + 1
        alloc[dec] <- alloc[dec] - 1
    }

    stopifnot(sum(alloc)==total)
        
    attr(alloc, "iterations") <- it

    return(alloc)    
}