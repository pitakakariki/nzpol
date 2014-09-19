library(shiny)

party <- c("ACT", "Conservative", "Green", "Labour", "M\u0101ori", "National", "NZ First", "Internet Mana", "United Future")
vote <- c(0.4, 5.7, 12.5, 24.3, 0.9, 42.8, 9.9, 2.5, 0.5)
seat <- c(1,0,0,0,1,0,0,1,1)

vote_list <- list()
seat_list <- list()
for (i in seq_along(party)) {
    
    vote_list[[i]] <- numericInput(str_c('x[', party[i], ']'), '', vote[i], 0, 100, 1)
    seat_list[[i]] <- numericInput(str_c('y[', party[i], ']'), '', seat[i], 0, 71, 1)
}

shinyUI(fluidPage(

  titlePanel("New Zealand Parliament"),

  fluidRow(
      
    column(2, do.call(wellPanel, as.list(party))),  
    column(2, do.call(wellPanel, vote_list)),
    column(2, do.call(wellPanel, seat_list)),
          
    column(6)
  )
))