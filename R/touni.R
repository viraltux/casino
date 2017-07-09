touni <- function(cards){
    ## Converts cards to unicode format

    ## TODO consider implement version for Unicode v.7 and higher with playing
    ## cards on range: 1F0A0-1F0FF
    
    cards <- gsub('S','\\\\u2660',cards)
    cards <- gsub('C','\\\\u2663',cards)
    cards <- gsub('H','\\\\u2665',cards)
    cards <- gsub('D','\\\\u2666',cards)
    stringi::stri_unescape_unicode(cards)
}
