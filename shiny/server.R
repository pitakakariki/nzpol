source("globals.R")

shinyServer(function(input, output) {

    share <- reactive({
        
        votes <- sapply(party, function(p) input[[str_c("vote", p)]])    
        seats <- sapply(party, function(p) input[[str_c("seat", p)]])
        
        votes[is.na(votes)] <- 0
        seats[is.na(seats)] <- 0

        # threshold and coat-tails
        votes[seats < 1 & votes < 5] <- 0
        
        x <- SainteLague(votes, 120)

        pmax(x, seats)
    })
    
    lapply(seq_along(party), function(i) {
        
        rtext <- renderUI({
            
            h6(share()[i])
        })
        
        output[[party[i]]] <- rtext
    })
    
    output$Total <- renderUI(h5(sum(share())))
    
    output$Plot <- renderPlot(housePlot(share()))

})