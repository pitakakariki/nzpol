housePlot <- function(app, square=FALSE) {

    win=app["Win"]
    
    app <- app[1:9]
    
    extra <- sum(app) - 120
    ex1 <- ceil(extra/2)
    ex2 <- floor(extra/2)
    
    xlimit <- if(square) c(0,100) else c(-50,150)
    
    plot(NA, xlim=xlimit, ylim=c(0,100), xaxt="n", yaxt="n", xlab="", ylab="")    
    
    r <- 15
    w <- 5
    
    x1 <- 48.5
    x2 <- 51.5
    
    y1 <- 47
    y2 <- 47
    
    # Side-benches (Win)
    
    x <- rep(c(x1+r+(1:3)*w), each=9)
    y <- rep(seq(w, 9*w, w), times=3)
    a <- rep(0, 27)
    
    Z <- rbind(x, y, a)
    
    # Cross-benches (Win)

    c1 <- arc(5, r)
    c2 <- arc(7, r + w)
    c3 <- arc(10, r + 2*w)
    c4 <- arc(11, r+3*w)[,5:10]
    c5l <- arc(12, r+4*w)[,(7-ex1):11]
    c5w <- arc(12, r+4*w)[,(7-ex2):11]

    Z <- cbind(Z, c(x1+w, y1+w, 0) + c1)
    Z <- cbind(Z, c(x1+w, y1+w, 0) + c2)
    Z <- cbind(Z, c(x1+w, y1+w, 0) + c3)
    Z <- cbind(Z, c(x1+w, y1+w, 0) + c4)
    Z <- cbind(Z, c(x1+w, y1+w, 0) + c5l)

    # Side-benches (Lose)

    x <- rep(c(x2-r-(1:3)*w), each=9)
    y <- rep(seq(w, 9*w, w), times=3)
    a <- rep(0, 27)
    Z <- cbind(Z, rbind(x,y,a))

    # Cross-bences (Lose)
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c1)
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c2)
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c3)
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c4)
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c5w)

    # Reverse order of losing benches so it flows around the house
    o <- c(1:(60+ex1),(120+ex1+ex2):(61+ex1))

    if(!win) o <- rev(o)

    plotSeats(x=Z[1,o], y=Z[2,o], w=w-1, a=Z[3,o], party=party_rep(app))   
}

arc <- function(n, r) {
    
    a <- seq(0, 1, length=n)
    rbind(r * cospi(a/2), r * sinpi(a/2), a)
}

party_rep <- function(app) {
    
    app <- app[c(4,3,8,5,7,2,9,1,6)]
    rep(names(app), times=app)    
}

party_cat <- function(x) {
    
    y <-str_c(names(x), " ", x, ",")
    
    
    
    cat(y)
}

plotSeats <- function(x, y, w, a, party) {
    
    # w is the width, we want half-width
    w <- w/2
    
    # a=1 is a quarter rotation, which is half of a half-rotation  
    a <- a/2
    
    c <- cospi(a)
    s <- sinpi(a)
    
    X <- rbind(
        x - w*(c-s),
        x + w*(c+s),
        x + w*(c-s),
        x - w*(c+s),
    NA)
    
    Y <- rbind(
        y - w*(c+s),
        y - w*(c-s),
        y + w*(c+s),
        y + w*(c-s),
    NA)
    
    polygon(X, Y, col=party_ctl[party,"Contrast"], border=party_ctl[party,"Colour"], lwd=2)
}

test_result <- final_sim(0)[,1]

housePlot(test_result)

png("house.png", width=500, height=500)
housePlot(test_result, square=TRUE)
dev.off()

