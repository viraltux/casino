context('Basic')

test_that("basic", {
  left <- c("J♥","2♦","10♥","A♥","3♠","Q♥","K♥")
  right <- c("J♥","2♦","10♥","A♥","3♠","Q♦","K♥")

  expect_is(bhand(left),'hand')
  expect_true(left%==%left)
  expect_true(left%>=%right)

  cards = c("2♦","3♦","4♠","5♣","7♠","8♥","9♠"); bh <- bhand(cards)  # 9 High
  expect_message(bhand(cards),'9 high\n')
  expect_equal(bh$name, 'High')
  expect_equal(bh$level, 1)
  expect_equal(bh$high, 9)
  expect_equal(bh$score, 1*100 + 9)

#    cards = c("J♦","2♦","10♠","A♣","3♠","9♥","4♠"); bhand(cards) # High      6
#   cards = c("2♦","3♦","4♠","5♣","7♠","8♥","2♠"); bhand(cards)  # Pair      7 = 6+1
#   cards = c("J♦","2♦","10♠","A♣","3♠","9♥","A♠"); bhand(cards) # Pair      19 = 6+13
#   cards = c("2♦","K♦","4♠","5♣","3♠","3♥","2♠"); bhand(cards)  # TPair     20 = 6+13+1
#   cards = c("2♦","K♦","4♠","5♣","A♠","A♥","2♠"); bhand(cards)  # TPair     31 = 6+13+12
#   cards = c("2♦","K♦","4♠","2♣","5♠","3♥","2♠"); bhand(cards)  # Three     32 = 6+13+12+1
#   cards = c("2♦","J♦","4♠","A♣","A♠","A♥","K♠"); bhand(cards)  # Three     44 = 6+13+12+13
#   cards = c("2♦","3♦","4♠","5♣","A♠","8♥","9♠"); bhand(cards)  # Straight  45 = 6+13+12+13+1
#   cards = c("J♦","2♦","10♠","A♣","3♠","Q♥","K♠"); bhand(cards) # Straight  54 = 6+13+12+13+10
#   cards = c("2♦","3♦","4♦","5♦","7♦","8♥","9♠"); bhand(cards)  # Flush     55 = 6+13+12+13+10+1
#   cards = c("J♦","Q♦","10♠","A♦","3♦","9♥","4♦"); bhand(cards) # Flush     62 = 6+13+12+13+10+8
#   cards = c("2♦","3♦","4♠","2♣","5♠","3♥","2♠"); bhand(cards)  # FullHouse 63 = 6+13+12+13+10+8+1
#   cards = c("A♦","3♦","4♠","A♣","5♠","3♥","A♠"); bhand(cards)  # FullHouse 75 = 6+13+12+13+10+8+13
#   cards = c("2♦","K♦","4♠","2♣","5♠","2♥","2♠"); bhand(cards)  # Four      76 = 6+13+12+13+10+8+13+1
#   cards = c("A♦","K♦","4♠","A♣","5♠","A♥","A♠"); bhand(cards)  # Four      88 = 6+13+12+13+10+8+13+13
#   cards = c("2♦","3♦","4♦","5♦","A♦","8♥","9♠"); bhand(cards)  # StrFlush  89 = 6+13+12+13+10+8+13+13+1
#   cards = c("2♦","3♦","4♦","5♦","6♦","8♥","9♠"); bhand(cards)  # StrFlush  90 = 6+13+12+13+10+8+13+13+2
#   cards = c("J♥","2♦","10♥","A♥","3♠","Q♥","K♥"); bhand(cards) # StrFlush  98 = 6+13+12+13+10+8+13+13+10



})
