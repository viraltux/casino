deck <- function(n = 1){
    ## Generates a poker deck
    
    ranks <- c('A',2:10,'J','Q','K')
    suits <- c('S','H','D','C')
    
    ascii <- do.call(paste0, expand.grid(ranks, suits))
    sample(rep(ascii,n))
}

