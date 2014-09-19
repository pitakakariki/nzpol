

housePlot <- function(app) {

    left <- sum(app[c(3,4,7,8)])
    right <- sum(app[c(1,2,6,9)])
    required <- floor(sum(app)/2) + 1
    win <- (left > right)
    
    total <- sum(app)
    
    if(total > 132) {app <- "BLANK"; total <- 120}
    
    extra <- total - 120
    ex1 <- ceiling(extra/2)
    ex2 <- floor(extra/2)
    
    par(mar=rep(0, 4), mai=rep(0, 4), xpd = NA)
    
    plot(NA, xlim=c(0,100), ylim=c(0,100),
         xaxt="n", yaxt="n", xlab="", ylab="",
         asp=1, axes=FALSE
    )
    
    r <- 15
    w <- 5
    
    x1 <- 48.5
    x2 <- 51.5
    
    y1 <- 47
    y2 <- 47
    
    # Side-benches
    
    x <- rep(c(x2-r-(3:1)*w, x1+r+(1:3)*w), each=9)
    y <- rep(seq(w, 9*w, w), times=6)
    a <- rep(0, 54)
    
    Z <- rbind(x, y, a)
    
    # Cross-benches
    
    c1 <- arc(5, r)
    Z <- cbind(Z, c(x1+w, y1+w, 0) + c1)
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c1)
    
    c2 <- arc(7, r + w)
    Z <- cbind(Z, c(x1+w, y1+w, 0) + c2)
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c2)
    
    c3 <- arc(10, r + 2*w)
    Z <- cbind(Z, c(x1+w, y1+w, 0) + c3)
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c3)
    
    c4 <- arc(11, r+3*w)[,5:10]
    Z <- cbind(Z, c(x1+w, y1+w, 0) + c4)
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c4)
    
    c5 <- arc(12, r+4*w)[,(7-ex1):11]
    Z <- cbind(Z, c(x1+w, y1+w, 0) + c5)
    
    c6 <- arc(12, r+4*w)[,(7-ex2):11]
    Z <- cbind(Z, c(x2-w, y2+w, 0) + c(-1,1,-1) * c6)
    
    o <- c(28:54, 55:59, 65:71, 79:88, 99:104,
        111:(115+ex2), (120+ex1+ex2):(116+ex2),
        110:105, 98:89, 78:72, 64:60, 27:1)

    if(!win) o <- c(1:27, 60:64, 72:78, 89:98, 105:110, (116+ex1):(120+ex1+ex2), (115+ex1):111,
        104:99, 88:79, 71:65, 59:55, 54:28)

    plotSeats(x=Z[1,o], y=Z[2,o], w=w-1, a=Z[3,o], party=party_rep(app))   
}

arc <- function(n, r) {
    
    a <- seq(0, 1, length=n)
    rbind(r * cospi(a/2), r * sinpi(a/2), a)
}

party_rep <- function(app) {
    
    if(identical(app, "BLANK")) return(rep("BLANK", 120))
    
    app <- app[c(4,3,8,5,7,2,9,1,6)]
    rep(names(app), times=app)    
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
