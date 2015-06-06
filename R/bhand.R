

ranks <- function(cards){
  deck.df[deck.df$deck %in% cards,]$rank
}

order <- function(cards){
  deck.df[deck.df$deck %in% cards,]$order
}

suits <- function(cards){
  deck.df[deck.df$deck %in% cards,]$suit
}

group.high <- function(cards, type){
  #group summary per type and highest card per type
  if (type == 'rank') {
    c <- as.numeric(table(ranks(cards)))
    r <- as.numeric(names(table(ranks(cards))))
    return(list(group = c, high = max(r[which(max(c)==c)])))
  }
  if (type == 'order') {
    c <- as.numeric(table(order(cards)))
    r <- as.numeric(names(table(order(cards))))
    return(list(group = c, high = max(r[which(max(c)==c)])))
  }
  if (type == 'suit') {
    c <- as.numeric(table(suits(cards)))
    s <-  names(table(suits(cards)))
    f <- s[which(max(c)==c)]
    pattern <- paste("[",paste(f, collapse = ''),"]",sep='')
    return(list(group = c, high = group.high(cards[grep(pattern,cards)],'rank')$high))
  }
}

straight <- function(cards){

  sr <- paste(-diff(sort(ranks(cards), decreasing = TRUE)),collapse = '')
  i <- regexpr('1{4,}', sr)[1]

  if (i>0) {
    r <- sort(ranks(cards), decreasing = TRUE)[i]
    return(r)
  }

  sri <- paste(diff(sort(order(cards), decreasing = FALSE)),collapse = '')
  srd <- paste(-diff(sort(order(cards), decreasing = TRUE)),collapse = '')

  ii <- regexpr('1{4,}', sri)[1]
  id <- regexpr('1{4,}', srd)[1]

  if (ii>0) {
    ri <- sort(order(cards), decreasing = FALSE)[ii]
    rd <- sort(order(cards), decreasing = TRUE)[id]

    r <- max(c(ri,rd))

    return(r)
  }

  return(0)
}


bhand <- function(cards){

  deck=NULL
  for (j in c('♣','♦','♥','♠')){
    for (i in c('A','2','3','4','5','6','7','8','9','10','J','Q','K')) {
      deck=c(deck,paste(i,j,sep=''))
    }
  }

  order=rep(1:13,4)
  rank=rep(c(14,2:13),4)
  suit=c(rep('♣',13),rep('♦',13),rep('♥',13),rep('♠',13))
  deck.df = data.frame(deck,rank,order,suit)

  cr <- group.high(cards,'rank')
  co <- group.high(cards,'order')
  cs <- group.high(cards,'suit')

  bcards <- function(h,l,r){list(cards = h, level = l, rank = r)}

  s <- straight(cards)
  if (sum(cs$group == 5)>0 & (s == 14 )) return(bcards('royal flush',10,14))
  if ( s & sum(cs$group == 5)) return(bcards('straight flush',9, s))
  if (sum(cr$group == 4)) return(bcards('four of a kind',8,cr$high))
  if (sum(cr$group == 2) & sum(cr$group == 3)) return(bcards('full house',7,cr$high))
  if (sum(cs$group == 5)) return(bcards('flush',6,cs$high))
  if ( s ) return(bcards('straight',5,s ))
  if (sum(cr$group == 3) >= 1) return(bcards('three of a kind',4,cr$high))
  if (sum(cr$group == 2) >= 2) return(bcards('two pair',3,cr$high))
  if (sum(cr$group == 2) >= 1) return(bcards('oner pair',2,cr$high))
  return(bcards('high',1,cr$high))
}

parse(text = paste())

gop<- function(left,op,right){
  bl <- bhand(left)
  br <- bhand(right)
  return(eval(parse(text = paste('(bl$level*100 + bl$rank)',op,'(br$level*100 + br$rank)'))))
}

'%>%'<- function(left,right){gop(left,'>',right)}
'%<%'<- function(left,right){gop(left,'<',right)}
'%<=%'<- function(left,right){gop(left,'<=',right)}
'%>=%'<- function(left,right){gop(left,'>=',right)}
'%==%'<- function(left,right){gop(left,'==',right)}
