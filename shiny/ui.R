source("globals.R")

input_list <- list()

input_list[["Header"]] <- list(column(4), column(3, h6("Votes")), column(2, h6("Electorate", align="right")), column(2, h6("Total", align="right")))

for (i in seq_along(party)) {
    
    input_list[[party[i]]] <- list(
        column(4, h6(party[i], align="right")),
        column(3, numericInput(str_c("vote", party[i]), '', vote[i], 0, 100, 1)),
        column(2, uiOutput(str_c("seat", party[i]))),
        column(2, uiOutput(party[i]))
    )
}

electorate_list <- list()
for (i in unique(elect$Num))
{
    cand  <- elect[elect$Num == i,]
    names <- list()
    electorate_name <- cand$Electorate[1]
    for (j in seq_along(cand$Party))
        names[[cand$Party[j]]] <- cand$Party[j]
    electorate_list[[i]] <- list(
        column(4, h6(electorate_name, align="right")),
        column(4, selectInput(str_c("electorate", electorate_name), '', names, selected=cand$Party[cand$Winner])),
        column(4, uiOutput(str_c("electorate",electorate_name)))
    )
}

input_list[["Total"]] <- list(column(7), column(2, uiOutput("Seats")), column(2, uiOutput("Total")))

party_list <- list()

for (i in seq_along(party)) {
    party_list[[party[i]]] <- list(
        column(12, uiOutput(str_c("mps", party[i])))
    )
}


shinyUI(fluidPage(

  titlePanel("New Zealand Parliament"),

  fluidRow(
      
    column(6, wellPanel(h3("Party vote"), lapply(input_list, do.call, what=fluidRow))),
          
    column(6, wellPanel(h3("House layout"), plotOutput("Plot")))
  ),
  fluidRow(
    column(6, wellPanel(h3("Electorates"), lapply(electorate_list, do.call, what=fluidRow))),
    column(6, wellPanel(h3("Members of Parliament"), fluidRow(column(6, uiOutput("LeftMPs")), column(6, uiOutput("RightMPs")))))
  )
))