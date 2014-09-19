source("globals.R")

input_list <- list()

input_list[["Header"]] <- list(column(4), column(4, h6("Votes")), column(2, h6("Seats")), column(2))

for (i in seq_along(party)) {
    
    input_list[[party[i]]] <- list(
        column(4, h6(party[i], align="right")),
        column(4, numericInput(str_c("vote", party[i]), '', vote[i], 0, 100, 1)),
        column(2, numericInput(str_c("seat", party[i]), '', seat[i], 0, 71, 1)),
        column(2, uiOutput(party[i]))
    )
}

input_list[["Total"]] <- list(column(10), column(2, uiOutput("Total")))

shinyUI(fluidPage(

  titlePanel("New Zealand Parliament"),

  fluidRow(
      
    column(6, wellPanel(lapply(input_list, do.call, what=fluidRow))),
          
    column(6, wellPanel(plotOutput("Plot")))
  )
))