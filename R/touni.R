touni <- function(cards){
    ## Converts cards to unicode format
    cards <- gsub('S','\\\\u2660',cards)
    cards <- gsub('C','\\\\u2663',cards)
    cards <- gsub('H','\\\\u2665',cards)
    cards <- gsub('D','\\\\u2666',cards)
    stringi::stri_unescape_unicode(cards)
}

