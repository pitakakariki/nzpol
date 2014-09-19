### this seems to be graphics stuff now. rename?

alpha <- function(x, alpha=1) rgb(t(col2rgb(x)), alpha=255*alpha, max=255)

loop <- function(x, y=x) c(x, rev(y))