hand.info <- function(cards, type){
  ## group summary per type and highest card per type

  if (type == 'rank') {
    c <- as.numeric(table(card.info(cards,'rank')))
    r <- as.numeric(names(table(card.info(cards,'rank'))))
    return(list(group = c, high = max(r[which(max(c)==c)])))
  }
  if (type == 'order') {
    c <- as.numeric(table(card.info(cards,'order')))
    r <- as.numeric(names(table(card.info(cards,'order'))))
    return(list(group = c, high = max(r[which(max(c)==c)])))
  }
  if (type == 'suit') {
    c <- as.numeric(table(card.info(cards,'suit')))
    s <-  names(table(card.info(cards,'suit')))
    pattern <- s[which(max(c)==c)][1]
    return(list(group = c,
                high = hand.info(cards[grep(pattern,cards)],'rank')$high))
  }
}
