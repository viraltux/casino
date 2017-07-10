toutf <- function(token){
    ## Converts casino game tokens to unicode format

    ## TODO consider implement version for Unicode v.7 and higher with playing
    ## cards on range: 1F0A0-1F0FF
    
    token <- gsub('S','\\\\u2660',token)
    token <- gsub('C','\\\\u2663',token)
    token <- gsub('H','\\\\u2665',token)
    token <- gsub('D','\\\\u2666',token)
    stringi::stri_unescape_unicode(token)
}
