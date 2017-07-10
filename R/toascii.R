toascii <- function(token){
    ## Converts casino game tokens to ascii format

    token <- gsub(stringi::stri_unescape_unicode('\\u2660'),'S',token)
    token <- gsub(stringi::stri_unescape_unicode('\\u2663'),'C',token)
    token <- gsub(stringi::stri_unescape_unicode('\\u2665'),'H',token)
    token <- gsub(stringi::stri_unescape_unicode('\\u2666'),'D',token)
    token
}



