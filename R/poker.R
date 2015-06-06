## Initializes Package

.onLoad<- function(libname, pkgname){

  constants <- parse( text = "
    deck <- NULL
    for (j in c('♣','♦','♥','♠')){
      for (i in c('A',as.character(2:10),'J','Q','K')) {
        deck <- c(deck,paste(i,j,sep=''))
      }
    }

    order <- rep(1:13,4)
    rank <- rep(c(14,2:13),4)
    suit <- c(rep('♣',13),rep('♦',13),rep('♥',13),rep('♠',13))
    assign('deck.df', data.frame(deck,rank,order,suit), envir = baseenv())",
                      encoding="UTF-8")

  eval(constants)
}
