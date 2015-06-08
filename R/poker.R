## Initializes Package

.onLoad<- function(libname, pkgname){
  constants <- parse( text = "
    deck <- NULL
    for (j in c('♣','♦','♥','♠')){
      for (i in c('A',as.character(2:10),'J','Q','K')) {
        deck <- c(deck,paste(i,j,sep=''))
      }
    }
    assign('deck', deck, envir = baseenv())",
                      encoding="UTF-8")

  eval(constants)
}
