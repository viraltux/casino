deck <- function(n = 1, type = c('standard')){
    ## Generation of card decks

    type <- match.arg(type,  c('standard'))
    
    ## Standard 52 card deck (French playing cards)    
    if (type == 'standard'){    

        ranks <- c('A',2:10,'J','Q','K')
        suits <- c('S','H','D','C')

        ascii <- do.call(paste0, expand.grid(ranks, suits))
        return(sample(rep(ascii,n)))
    }
}


