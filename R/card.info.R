card.info <- function(cards, info){
  ## Returns the rank, order or suit information regards a set or cards

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
