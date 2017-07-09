context('Basic')

test_that('basic', {
  left <- c("JH","2D","10H","AH","3S","QH","KH")
  right <- c("JH","2D","10H","AH","3S","QD","KH")
  center <- c("JH","2D","10H","2H","3S","QD","KH")

  expect_is(bhand(left),'hand')
  expect_true(left%==%left)
  expect_true(left%>=%right)
  expect_true(right%<=%left)
  expect_true(right%!=%center)
  expect_true(right%>=%center)
  expect_true(center%<=%right)

  cat('\n')

  test.hand <- function(cards, name, level, high){
    bh <- bhand(cards)
    expect_equal(bh$name, name, info = print(bh))
    expect_equal(bh$level, level)
    expect_equal(bh$high, high)
    expect_equal(bh$score, 100*level+high)
    cat('\n')
  }


  cards = c("2D","3D","4S","5C","7S","8H","9S") # 9 High
  test.hand(cards, 'High', 1, 9)

  cards = c("JD","2D","10S","AC","3S","9H","4S") # Ace High
  test.hand(cards,'High', 1, 14)

  cards = c("2D","3D","4S","5C","7S","8H","2S") # One Pair 2 High
  test.hand(cards, 'One pair', 2, 2)

  cards = c("JD","2D","10S","AC","3S","9H","AS") # One Pair Ace High
  test.hand(cards, 'One pair', 2, 14)

  cards = c("2D","KD","4S","5C","3S","3H","2S") # Two Pairs 3 High
  test.hand(cards, 'Two pairs', 3, 3)

  cards = c("2D","KD","4S","5C","AS","AH","2S") # Two Pairs Ace High
  test.hand(cards, 'Two pairs', 3, 14)

  cards = c("2D","KD","4S","2C","5S","3H","2S") # Three
  test.hand(cards, 'Three of a kind', 4, 2)

  cards = c("2D","JD","4S","AC","AS","AH","KS") # Three
  test.hand(cards, 'Three of a kind', 4, 14)

  cards = c("2D","3D","4S","5C","AS","8H","9S") # Straight 5 high
  test.hand(cards, 'Straight', 5, 5)

  cards = c("JD","2D","10S","AC","3S","QH","KS") # Straight Ace high
  test.hand(cards, 'Straight', 5, 14)

  cards = c("AD","3D","4D","5D","7D","8H","QH") # Flush Ace high
  test.hand(cards, 'Flush', 6, 14)

  cards = c("2D","3D","4D","5D","7D","8H","QH") # Flush 7 high
  test.hand(cards, 'Flush', 6, 7)

  cards = c("2D","3D","4D","5D","8D","8H","10S") # Flush 8 high
  test.hand(cards, 'Flush', 6, 8)

  cards = c("JD","QD","10S","AD","3D","9H","4D") # Flush Ace high
  test.hand(cards, 'Flush', 6, 14)

  cards = c("2D","3D","4S","2C","5S","3H","2S") # Full House 2 high
  test.hand(cards, 'Full house', 7, 2)

  cards = c("AD","3D","4S","AC","5S","3H","AS") # Full House Ace high
  test.hand(cards, 'Full house', 7, 14)

  cards = c("2D","KD","4S","2C","5S","2H","2S") # Four of a kind 2 high
  test.hand(cards, 'Four of a kind', 8, 2)

  cards = c("AD","KD","4S","AC","5S","AH","AS") # Four of a kind Ace high
  test.hand(cards, 'Four of a kind', 8, 14)

  cards = c("2D","3D","4D","5D","AD","8H","9S") # Straight flush 5 high
  test.hand(cards, 'Straight flush', 9, 5)

  cards = c("2D","3D","4D","5D","6D","8H","9S") # Straight flush 6 high
  test.hand(cards, 'Straight flush', 9, 6)

  cards = c("JH","2D","10H","AH","3S","QH","KH") # Royal flush
  test.hand(cards, 'Royal flush', 10, 14)


})

