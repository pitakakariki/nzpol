###
##
## --- New Zealand General Election Polling ---
##
## Plots polling data scraped from Wikipedia.
##
## - GAM -
##
## The smoothed curves are calculated using a generalised additive model (GAM).  The
## smoothing parameter is estimated using cross-validation. This means that the curves
## are estimated based on subsets of the data, and the parameter that best predicts
## the hold-out data is chosen. Note that the error bounds are based on the assumption
## that the smoothing parameter is correct, i.e. uncertainty in the smoothing parameter
## estimate are not accounted for.
##
## A separate smoothed curve is estimated for each combination of polling firm and party
## (the election is treated as a poll and given more weight than the opinion polls). The
## shape of the curves for each party is constrained to be the same for every polling firm,
## although they can be vertically offset from each other. The displayed curve is aligned
## so that it passes through the election result, but in the interactive version you can
## see the offset curves for each polling outfit.
##
## - SVG -
##
## The svg graphics format allows interactivity via Javascript.  Note that the
## SVGAnnotation library comes from http://www.bioconductor.org/ rather that
## the CRAN repositories.  This requires Cairo to be available (use 
## capabilities("cairo") to check this).
##
###

library(stringr)
library(plyr)
library(XML)
library(xts)
library(ggplot2)
library(lubridate)
library(mgcv)
library(reshape)
library(Hmisc)
library(rje)

###
##
## Settings
##
###

# Options

opt_A <- 0
opt_k <- 1

opt_2011 <- 0

opt_begin <- as.Date("2011-01-01")
opt_end <- today()

if(opt_2011) {
opt_begin <- as.Date("2008-01-01")
opt_end <- as.Date("2011-11-24")
}

opt_filename <- "nzpolls"
opt_svg <- FALSE         # set to FALSE if you can't get SVGAnnotation to work
opt_png <- !opt_svg

opt_filename <- str_c(opt_filename, if(opt_svg) ".svg" else ".png")

opt_bc <- TRUE         # bias corrections
opt_bcplot <- FALSE     # plot bias-corrected points
opt_bcreport <- TRUE    # print bias estimates
opt_bccurves <- opt_svg # curves for each polling firm

opt_events <- FALSE      # event markers
opt_thresh <- TRUE      # 5% threshold marker

opt_ewt <- 1000         # weighting for election data points
opt_adapt <- FALSE      # Adaptive smoother

opt_svgwidth <- 12      # width and height if the output file is svg
opt_svgheight <- 7      # these will be multiplied by 72 for png

opt_xwidth <- 3/2 * opt_svgwidth    # 1 / opt_xwidth of the plot is reserved for final results
opt_xheight <- 3/2 * opt_svgheight  # 1 / opt_xheight for event markers

opt_dashes <- "[\u002D\u2013\u2014\u2212]"
opt_spaces <- "[\u0020\u00A0]"

opt_plotcurves <- TRUE
opt_curvepts <- 500     # number of line segements to make "curves"
opt_curvett <- TRUE     # tooltips on curves

opt_moe <- 0

opt_legend <- TRUE

# Wikipedia urls

urls <- str_c("https://en.wikipedia.org/wiki/Opinion_polling_for_the_New_Zealand_general_election,_", c(2005, 2008, 2011))
urls <- c(urls, "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_New_Zealand_general_election")

# Party details

party_ctl <- read.table(

    header = TRUE,
    row.names = "Party",
    stringsAsFactors = FALSE,
    comment = "",
    text = "

    Party           Colour   Contrast Include
    ACT             #FFE401  #FFFF80    Y
    Conservative    #00AEEF  #33CCFF    Y
    Destiny         #FF0000  #000000    -
    Green           #098137  #B3FFB3    Y
    Labour          #FF0000  #FFBAA8    Y
    Mana            #770808  #FF6E6E    -
    Maori           #EF4A42  #FFCC80    Y
    National        #00529F  #CCDDFF    Y
    'NZ First'      #000000  #CCCCCC    Y
    Progressive     #9E9E9E  #DDCCDD    -
    Internet        #662C92  #EE77FF    -
    'Internet Mana' #662C92  #FF6E6E    Y
    'United Future' #501557  #DD99DD    Y
    'Labour/Green'  #FF0000  #B3FFB3    -
")

party_ctl[, "Include"] <- (party_ctl[, "Include"] == "Y")
rownames(party_ctl)[match("Maori", rownames(party_ctl))] <- "M\u0101ori"

if(opt_svg) library(SVGAnnotation, quietly=TRUE)

###
##
## Helper function definitions
##
###

#source("R/getWikipediaData.R")
source("R/helper.R")
source("R/dates.R")
source("R/format.R")
source("R/analysis.R")

this.e <- as.numeric(as.Date("2014-09-20"))
if(opt_2011) this.e <- as.numeric(as.Date("2011-11-26"))

get_party_data <- function(p) {

    s <- !is.na(poll_matrix[, p])
    n <<- sum(s)

    x <<- as.numeric(x_date[s])
    xadj <<- as.numeric(x_adj[s])

    y <<- poll_matrix[s, p]
    z <<- poll_data $ Poll[s]

    this_colour <<- party_ctl[p, "Colour"]
    this_contrast <<- party_ctl[p, "Contrast"]

    xs <<- seq(min(x), this.e, length=opt_curvepts)
    xadjs <<- time_dilation(xs, A=opt_A, k=opt_k)
    new_X <<- data.frame(xadj=xadjs, z="Election")

    s_elect <<- (z == "Election")

    invisible()
}

###
##
## Collect the data.
##
###

poll_data <- getAllData(urls)
poll_data <- sort_df(poll_data, 'date_mean')                # Sort from earliest to latest poll

# Restrict to selected dates
s_date <- (poll_data $ date_start >= opt_begin & poll_data $ date_end <= opt_end)
poll_data <- poll_data[s_date, ]

poll_data <- droplevels(poll_data)                          # Remove unused polling firms

incl_parties <- rownames(party_ctl)[party_ctl $ Include]
poll_matrix <- as.matrix(poll_data[, incl_parties])         # Restrict to parties of interest
x_date <- poll_data $ date_mean
x_adj <- time_dilation(x_date, A=opt_A, k=opt_k)

###
##
## Analysis and plotting.
##
###

#png(opt_filename, width=opt_svgwidth*72, height=opt_svgheight*72)

xw <- as.numeric(opt_end - opt_begin) / (opt_xwidth - 1)
xh <- if(opt_events) max(poll_matrix, na.rm=TRUE) / (opt_xheight - 1) else 0

# This sets up the plot region
matplot(

    x_date, poll_matrix,
    pch = NA,
    main = "New Zealand General Election Polling",
    xlim = c(min(x_date), max(x_date) + xw), xlab = "", xaxt = "n",
    ylim = c(-xh, max(6, 1.1 * max(poll_matrix, na.rm=TRUE))), ylab = "Party Percentage (%)"
)
axis.Date(1, as.Date(x_date))
abline(h=0)

is_election <- (poll_data $ Poll == "Election")
abline(v=x_date[is_election])

if(opt_thresh) {

    abline(h=5, lty=2)
}

# Fit the Generalised Additive Model

a_list <- list()
b_list <- list()

cf_list <- list()
final <- character()

for(p in incl_parties) {

    get_party_data(p)

    a <- gam(y ~ s(xadj) + z, weights=pwt(z))

    a_list[[p]] <- a

    cf <- coef(a)
    names(cf)[1] <- "zElection"; cf[1] <- 0

    cf_list[[p]] <- cf[str_c("z", levels(poll_data $ Poll)[-1])] # Keep track of bias estimates
    names(cf_list[[p]]) <- levels(poll_data $ Poll)[-1]

}

bias <- do.call(cbind, cf_list)
colnames(bias)[grep("Maori", colnames(bias))] <- "M\u0101ori"

# Plot spline curves

for(p in incl_parties) {

    get_party_data(p)

    a <- a_list[[p]]

    b <- predict(a, newdata=new_X, se=TRUE)
    b_list[[p]] <- b

    polygon(

        loop(xs),
        loop(b$fit) + 1.96 * loop(b$se.fit, -b$se.fit),
        col = alpha(this_contrast, 1/2),
        border = NA
    )

    lines(xs, b$fit, lwd=3, col=this_colour)

    if(opt_curvett) {
        
        points(xs, b$fit, col=alpha(this_colour, 0), bg=alpha(this_contrast, 0.1), pch=21)
    }
}

# Plot data points

for(p in incl_parties) {

    get_party_data(p)

    if(opt_bc && opt_bcplot) {

        cf <- cf_list[[p]]
        y[!s_elect] <- y[!s_elect] - cf[as.character(z[!s_elect])] # Bias correction
    }

    n <- sum(!is.na(y))

    if(opt_moe > 0) {

        x_me <- ggplot2:::interleave(x, x, NA)
        y_me <- ggplot2:::interleave(y-opt_moe, y+opt_moe, NA)
        lines(x_me, y_me, col=this_colour)
    }

    points(x, y, pch=21, cex=1, col=this_colour, bg=this_contrast)
}


# Election results

for(p in incl_parties) {

    get_party_data(p)

    points(

        x[s_elect], y[s_elect],
        pch = 21, cex = 2,
        col = party_ctl[p, "Colour"],
        bg = party_ctl[p, "Contrast"]
    )
    n_elect <- sum(s_elect)
}

# Plot final estimates

final_vote <- numeric()
final_se <- numeric()

for(p in incl_parties) {

    get_party_data(p)

    b <- b_list[[p]]

    final_x <- tail(x_date, 1)
    final_fit <- tail(b $ fit, 1)
    final_err <- 1.96 * tail(b $ se.fit, 1)
    final[p] <- sprintf("%s \u00B1 %s%%", format_num(final_fit), format_num(final_err))

    final_vote[p] <- final_fit
    final_se[p] <- final_err/1.96
    
    final_date <- tail(x_date, 1)
    right_x <- par("usr")[2]
    avail_w <- right_x - as.numeric(final_date)

    test_str <- sprintf("%s \u00B1 %s%%_", format_num(800/9), format_num(80/9)) # "88.8 ± 8.8%_"
    test_w <- strwidth(test_str, font=4)

    text(
        final_x, final_fit, final[p],
        pos = 4, font = 4, xpd = TRUE,
        col = this_colour,
        cex = avail_w / test_w,
    )
}

# Legend (SVG is broken for this, do it last)

if(opt_legend) {

  party_col <- sapply(incl_parties, function(p) {get_party_data(p); this_colour})
  party_bg <- sapply(incl_parties, function(p) {get_party_data(p); this_contrast})
    
  legend("topleft", legend=incl_parties, pch=21, col=party_col, pt.bg=party_bg)
}

#dev.off()

# Bias estimates

print(round(bias, 1))

final_sim <- function(random=TRUE) {
    
    x <- rnorm(9, final_vote, final_se)
    names(x) <- names(final_vote)
    
    if(!random) x <- final_vote
    
    if(x["Conservative"] < 5) x["Conservative"] <- 0
    
    app <- SainteLague(x, 120)
    
    app["ACT"] <- max(app["ACT"], 1)
    app["M\u0101ori"] <- max(app["M\u0101ori"], 1)
    app["Internet Mana"] <- max(app["Internet Mana"], 1)
    app["United Future"] <- max(app["United Future"], 1)
    
    app["TOTAL"] <- sum(app)
    
    app["Left"] <- sum(app[c(3,4,7,8)])
    app["Right"] <- sum(app[c(1,2,6,9)])
    
    app["Required"] <- floor(app["TOTAL"]/2) + 1
    app["Win"] <- app["Left"] >= app["Required"]
    app["Lose"] <- app["Right"] >= app["Required"]
    
    cbind(app)
}