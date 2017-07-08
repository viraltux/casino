## Initializes Package

.onLoad<- function(libname, pkgname){

  constants <- parse( text = "
    deck <- NULL

    us <- c('\\u2660','\\u2665','\\u2666','\\u2663')
    us <- stringi::stri_unescape_unicode(us)

    for (j in us){
      for (i in c('A',as.character(2:10),'J','Q','K')) {
        deck <- c(deck,paste(i,j,sep=''))
      }
    }
      assign('deck', deck,  envir=parent.env(environment()))",
                      encoding="UTF-8")

  eval(constants)
}
