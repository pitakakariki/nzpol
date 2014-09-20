library(shiny)
library(stringr)

source("apportion.R")
source("shinyPlot.R")
source("electorates.R")

party <- c("ACT", "Conservative", "Green", "Labour", "M\u0101ori", "National", "NZ First", "Internet Mana", "United Future")
vote <- c(0.4, 5.7, 12.5, 24.3, 0.9, 42.8, 9.9, 2.5, 0.5)
seat <- c(1,0,0,0,1,0,0,1,1)


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
    BLANK           #000000  #FFFFFF    -
    ")

party_ctl[, "Include"] <- (party_ctl[, "Include"] == "Y")
rownames(party_ctl)[match("Maori", rownames(party_ctl))] <- "M\u0101ori"