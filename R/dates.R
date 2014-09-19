#
# Dates are in the not-so-useful format "something - something".
#
#

date <- function(x) {

    temp <- lapply(str_split(x, opt_dashes), rev) # Four different kinds of dashes used!

    # Date that poll coverage ends

    a <- sapply(temp, `[`, 1)
    a <- str_replace(a, "c\\.", "")
    a <- str_replace(a, "Released", "")
    a <- str_trim(a)

    date_end <- as.Date(a, "%d %b %Y")

    # Date that poll coverage starts

    b <- sapply(temp, `[`, 2)

    b <- str_trim(b)
    b <- str_split(b, opt_spaces) # Second space is a non-breaking space.

    year_start <- as.numeric(sapply(b, `[`, 3))
    month_start <- match(sapply(b, `[`, 2), month.name)
    day_start <- as.numeric(sapply(b, `[`, 1))

    date_start <- date_end
    s <- !is.na(year_start);   if(any(s)) date_start[s] <- update(date_start[s], years=year_start[s])
    s <- !is.na(month_start);  if(any(s)) date_start[s] <- update(date_start[s], months=month_start[s])
    s <- !is.na(day_start);    if(any(s)) date_start[s] <- update(date_start[s], days=day_start[s])

    # Polls with only month and year

    s <- is.na(date_start)

    if(any(s)) {

        date_start[s] <- as.Date(as.yearmon(a[s]))
        date_end[s] <- date_start[s] + new_period(month=1) - new_period(day=1)
    }

    # Combine and return

    date_mean <- as.Date(apply(cbind(date_start, date_end), 1, mean))
    
    # adjust for unreported polling period
    date_end <- ifelse(date_end %in% edates, date_end+1, date_end)
    date_mean <- ifelse(date_start == date_end, date_end - 4, date_mean)

    data.frame(orig=x, date_start, date_end, date_mean)
}