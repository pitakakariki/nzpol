source("globals.R")

shinyServer(function(input, output) {

    cand_names <- reactive({
        sapply(levels(elect$Electorate), function(i) {
            row <- elect$Electorate == i & elect$Party == input[[str_c("electorate", i)]]
            c(paste(elect$Candidate.Surname[row], elect$Candidate.Given.names[row], sep=", "), elect$Party[row])
        })
    })

    party_seats <- reactive({
        sapply(party, function(p) {
            sum(cand_names()[2,] == p)
        })
    })

    share <- reactive({
        
        votes <- sapply(party, function(p) input[[str_c("vote", p)]])    
        seats <- party_seats()

        votes[is.na(votes)] <- 0
        seats[is.na(seats)] <- 0

        # threshold and coat-tails
        votes[seats < 1 & votes < 5] <- 0
        
        x <- SainteLague(votes, 120)

        pmax(x, seats)
    })

    party_mps <- reactive({
        lapply(seq_along(party), function(p) {
             el_won  <- cand_names()[2,] == party[p]
             get_mps(party[p], cand_names()[1, el_won], share()[p])
        })
    })

    # output electorate seats
    lapply(seq_along(party), function(i) {

        rtext <- renderUI({
            h6(party_seats()[i], align="right")
        })

        output[[str_c("seat",party[i])]] <- rtext
    })

    # output total seats
    lapply(seq_along(party), function(i) {

        rtext <- renderUI({
            h6(share()[i], align="right")
        })

        output[[party[i]]] <- rtext
    })

    # output electorate winners
    lapply(levels(elect$Electorate), function(i) {
        rtext <- renderUI({
            h6(cand_names()[1,i])
        })

        output[[str_c("electorate", i)]] <- rtext
    })

    # output MP list
    output$LeftMPs <- renderUI({
        lapply(c(4,3,8,7), function(i) {
            if (length(party_mps()[[i]]) > 0) {
                list(h4(party[i]),
                    lapply(party_mps()[[i]], p))
            }
        })
    })
    output$RightMPs <- renderUI({
        lapply(c(6,1,5,2), function(i) {
            if (length(party_mps()[[i]]) > 0) {
                list(h4(party[i]),
                    lapply(party_mps()[[i]], p))
            }
        })
    })

    output$Debug <- renderPrint(paste(party_mps()[[4]], collapse="\n"))

    output$Seats <- renderUI(h5(sum(party_seats()), align="right"))
    output$Total <- renderUI(h5(sum(share()), align="right"))

    output$Plot <- renderPlot(housePlot(share()))

})