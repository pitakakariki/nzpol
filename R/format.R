format_num <- function(x) formatC(x, digits=1, format="f")

format_date <- function(x) format(as.Date(x), "%d %B %Y")

format_date_range <- function(x, y) {

    x_day <- day(x)
    x_month <- month.name[month(x)]
    x_year <- year(x)

    y_day <- day(y)
    y_month <- month.name[month(y)]
    y_year <- year(y)

    en <- "\u2013"

    ifelse(

        x_year != y_year,
        str_c(x_day, x_month, x_year, en, y_day, y_month, y_year, sep=" "),
        ifelse(

            x_month != y_month,
            str_c(x_day, x_month, en, y_day, y_month, y_year, sep=" "),
            ifelse(

                x_day != y_day,
                str_c(x_day, en, y_day, y_month, y_year, sep=" "),
                str_c(y_day, y_month, y_year, sep=" ")
            )
        )
    )
}