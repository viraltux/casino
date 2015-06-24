context('Basic')

test_that('basic', {
  left <- c("J♥","2♦","10♥","A♥","3♠","Q♥","K♥")
  right <- c("J♥","2♦","10♥","A♥","3♠","Q♦","K♥")

  expect_is(bhand(left),'hand')
  expect_true(left%==%left)
  expect_true(left%>=%right)

  cat('\n')

  test.hand <- function(cards, name, level, high){
    bh <- bhand(cards)
    expect_equal(bh$name, name, info = print(bh))
    expect_equal(bh$level, level)
    expect_equal(bh$high, high)
    expect_equal(bh$score, 100*level+high)
    cat('\n')
  }


  cards = c("2♦","3♦","4♠","5♣","7♠","8♥","9♠") # 9 High
  test.hand(cards, 'High', 1, 9)

  cards = c("J♦","2♦","10♠","A♣","3♠","9♥","4♠") # Ace High
  test.hand(cards,'High', 1, 14)

  cards = c("2♦","3♦","4♠","5♣","7♠","8♥","2♠") # One Pair 2 High
  test.hand(cards, 'One pair', 2, 2)

  cards = c("J♦","2♦","10♠","A♣","3♠","9♥","A♠") # One Pair Ace High
  test.hand(cards, 'One pair', 2, 14)

  cards = c("2♦","K♦","4♠","5♣","3♠","3♥","2♠") # Two Pairs 3 High
  test.hand(cards, 'Two pairs', 3, 3)

  cards = c("2♦","K♦","4♠","5♣","A♠","A♥","2♠") # Two Pairs Ace High
  test.hand(cards, 'Two pairs', 3, 14)

  cards = c("2♦","K♦","4♠","2♣","5♠","3♥","2♠") # Three
  test.hand(cards, 'Three of a kind', 4, 2)

  cards = c("2♦","J♦","4♠","A♣","A♠","A♥","K♠") # Three
  test.hand(cards, 'Three of a kind', 4, 14)

  cards = c("2♦","3♦","4♠","5♣","A♠","8♥","9♠") # Straight 5 high
  test.hand(cards, 'Straight', 5, 5)

  cards = c("J♦","2♦","10♠","A♣","3♠","Q♥","K♠") # Straight Ace high
  test.hand(cards, 'Straight', 5, 14)

  cards = c("A♦","3♦","4♦","5♦","7♦","8♥","Q♥") # Flush Ace high
  test.hand(cards, 'Flush', 6, 14)

  cards = c("2♦","3♦","4♦","5♦","7♦","8♥","Q♥") # Flush 7 high
  test.hand(cards, 'Flush', 6, 7)

  cards = c("2♦","3♦","4♦","5♦","8♦","8♥","10♠") # Flush 8 high
  test.hand(cards, 'Flush', 6, 8)

  cards = c("J♦","Q♦","10♠","A♦","3♦","9♥","4♦") # Flush Ace high
  test.hand(cards, 'Flush', 6, 14)

  cards = c("2♦","3♦","4♠","2♣","5♠","3♥","2♠") # Full House 2 high
  test.hand(cards, 'Full house', 7, 2)

  cards = c("A♦","3♦","4♠","A♣","5♠","3♥","A♠") # Full House Ace high
  test.hand(cards, 'Full house', 7, 14)

  cards = c("2♦","K♦","4♠","2♣","5♠","2♥","2♠") # Four of a kind 2 high
  test.hand(cards, 'Four of a kind', 8, 2)

  cards = c("A♦","K♦","4♠","A♣","5♠","A♥","A♠") # Four of a kind Ace high
  test.hand(cards, 'Four of a kind', 8, 14)

  cards = c("2♦","3♦","4♦","5♦","A♦","8♥","9♠") # Straight flush 5 high
  test.hand(cards, 'Straight flush', 9, 5)

  cards = c("2♦","3♦","4♦","5♦","6♦","8♥","9♠") # Straight flush 6 high
  test.hand(cards, 'Straight flush', 9, 6)

  cards = c("J♥","2♦","10♥","A♥","3♠","Q♥","K♥") # Royal flush
  test.hand(cards, 'Royal flush', 10, 14)


})

