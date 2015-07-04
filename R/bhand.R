get.cards <- function(cards, info){
  # Returns the rank, order or suit information regards a set or cards

  if (!(info %in% c('rank','order','suit'))) stop('No info set or available')

  pattern <- '(([2-9]|10)|[AJQK])'

  if (info == 'suit'){ return( gsub(pattern, '', cards) )}

  r <- regexpr(pattern,cards)
  m <- regmatches(cards,r)
  if (info == 'rank') {m <- gsub('A',14, m)}
  else {m <- gsub('A',1, m)}
  m <- gsub('J',11, m)
  m <- gsub('Q',12, m)
  m <- gsub('K',13, m)
  return(as.numeric(m))
}

group.high <- function(cards, type){
  #group summary per type and highest card per type

  if (type == 'rank') {
    c <- as.numeric(table(get.cards(cards,'rank')))
    r <- as.numeric(names(table(get.cards(cards,'rank'))))
    return(list(group = c, high = max(r[which(max(c)==c)])))
  }
  if (type == 'order') {
    c <- as.numeric(table(get.cards(cards,'order')))
    r <- as.numeric(names(table(get.cards(cards,'order'))))
    return(list(group = c, high = max(r[which(max(c)==c)])))
  }
  if (type == 'suit') {
    c <- as.numeric(table(get.cards(cards,'suit')))
    s <-  names(table(get.cards(cards,'suit')))
    pattern <- s[which(max(c)==c)][1]
    return(list(group = c,
                high = group.high(cards[grep(pattern,cards)],'rank')$high))
  }
}


straight <- function(cards){
  # Checks for a straight in a set of cards returning the highest card
  # within the straight and 0 if no straight is found

  sr <- paste(-diff(sort(get.cards(cards,'rank'), decreasing = TRUE)),collapse = '')
  i <- regexpr('1{4,}', sr)[1]

  if (i>0) {
    r <- sort(get.cards(cards,'rank'), decreasing = TRUE)[i]
    return(r)
  }

  sri <- paste(diff(sort(get.cards(cards,'order'), decreasing = FALSE)),collapse = '')
  srd <- paste(-diff(sort(get.cards(cards,'order'), decreasing = TRUE)),collapse = '')

  ii <- regexpr('1{4,}', sri)[1]
  id <- regexpr('1{4,}', srd)[1]

  if (ii>0) {
    ri <- sort(get.cards(cards,'order'), decreasing = FALSE)[ii]
    rd <- sort(get.cards(cards,'order'), decreasing = TRUE)[id]

    r <- max(c(ri,rd))

    return(r)
  }
  return(0)
}

bhand <- function(cards){
  # Returns the best poker hand within a set of cards

  # Using [] breaks testthat & not encoding in latin1 breaks bhand
  cards <- `Encoding<-`(cards,'latin1')
  nright <- length(grep('^(([2-9]|10)|[AJQK])(♣|♦|♥|♠)$',cards))
  if (length(cards) != nright) {stop('Cards not properly formatted.')}

  cr <- group.high(cards,'rank')
  co <- group.high(cards,'order')
  cs <- group.high(cards,'suit')

  bcards <- function(h,l,r){
    hand <- list(name = h, level = l, high = r, score = l*100 + r)
    class(hand) <- 'hand'
    hand
  }

  s <- straight(cards)
  if (sum(cs$group == 5)>0 & (s == 14 )) return(bcards('Royal flush',10,14))
  if ( s & sum(cs$group == 5)) return(bcards('Straight flush',9, s))
  if (sum(cr$group == 4)) return(bcards('Four of a kind',8,cr$high))
  if (sum(cr$group == 2) & sum(cr$group == 3)) return(bcards('Full house',7,cr$high))
  if (sum(cs$group >= 5)) return(bcards('Flush',6,cs$high))
  if ( s ) return(bcards('Straight',5,s ))
  if (sum(cr$group == 3) >= 1) return(bcards('Three of a kind',4,cr$high))
  if (sum(cr$group == 2) >= 2) return(bcards('Two pairs',3,cr$high))
  if (sum(cr$group == 2) >= 1) return(bcards('One pair',2,cr$high))
  return(bcards('High',1,cr$high))
}


print.hand <- function(hand, ...){
  # Print information for a poker hand from the class 'hand'

  high.name <- hand$high
  if (high.name == 14) {high.name <- 'Ace'}
  if (high.name == 13) {high.name <- 'King'}
  if (high.name == 12) {high.name <- 'Queen'}
  if (high.name == 11) {high.name <- 'Jack'}
  desc <- NULL
  if (hand$level == 1)  {desc <- paste(high.name,'high')}
  else {desc <- paste(hand$name, high.name, 'high')}
  if (hand$level == 10) {desc <- paste(hand$name)}
  cat(desc)
}

gop<- function(left,op,right){
  # Checks for equalities and inequalities between poker hands
  capture.output(if (class(left) == 'hand' & class(left) == 'hand'){
    bl <- left
    br <- right
  } else {bl <- bhand(left); br <- bhand(right)}, file='NUL')
  return(eval(parse(text = paste('(bl$level*100 + bl$high)', op,
                                 '(br$level*100 + br$high)'))))
}

'%>%'<- function(left,right){gop(left,'>',right)}
'%<%'<- function(left,right){gop(left,'<',right)}
'%<=%'<- function(left,right){gop(left,'<=',right)}
'%>=%'<- function(left,right){gop(left,'>=',right)}
'%==%'<- function(left,right){gop(left,'==',right)}
'%!=%'<- function(left,right){gop(left,'!=',right)}
