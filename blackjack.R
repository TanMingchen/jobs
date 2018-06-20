library(ggplot2)

# game example

#build a card function: get a card, use n,m to control the probability of getting card with large number
card.select <- function(n=1,m=6) {
  x <- sample(1:13,1,prob=rep(c(n,m),c(9,4))/sum(rep(c(n,m),c(9,4))))
  # treat j,q,k as
  x <-ifelse(x>10,10,x)
  return(x)
}

#build a game function for getting the points of a player or a banker, set a point as a threshold o determine whether to stop getting cards.
game <- function(point) {
  # get 2 cards during start
  select <- c(card.select(),card.select())
  # convert A to 11 and sum up
  Ato11 <- select
  Ato11[Ato11==1] <- 11
  card.sum <- sum(Ato11)
  # detemine whether to get a card or stop if the sum exceeds the threshold
  while (card.sum <= point) { 
    select <- c(select, card.select())
    Ato11 <- select
    Ato11[Ato11==1] <- 11
    card.sum <- sum(Ato11)
  } 
  # if the sum > 21 and contains A, convert A to 1, and sum 
  if (card.sum > 21 && 1 %in% select) {
    card.sum <- sum(select)
  }
  # detemine whether to get a card or stop
  while (card.sum <= point) {
    select <- c(select, card.select())
    card.sum <- sum(select)
  }
  # if > 21, boom, set the point as 0. 
  y <-ifelse((card.sum<=21),card.sum,0)
  return(y)
  # cat('select=',select,'\n','return=',y,'\n')
}

# play with banker (threshold, banker-16, player-12)
player <- replicate(100000,game(12))
dealer <- replicate(100000,game(16))
#results: win, lose or tie
result <-ifelse(player > dealer,1,ifelse(player < dealer,-1,0))
# if boom and tie, player loses
result[player==0 & result==0] <- -1
#Analysis of the result
# remove the results of ties
result.no.tie <- result[result!=0]
# binominal test ( in this case, player has higher p to win)
binom.test(length(result.no.tie[result.no.tie==1]),length(result.no.tie))
# see to consecutive wins/loses
table(rle(result))
# cumulative profits
profit <- cumsum(result)
# visualisation of cumulative profits
q <- ggplot(data.frame(profit,index=1:length(profit)),aes(index,profit))
q + geom_line(colour='lightskyblue4')


# test the profit and loss of the player under diffeerent (m/n)s
odd.10 <-  win <- rep(0,10)
for (i in 1:10) {
  card.select <- function(n=1,m=i) {
    x <- sample(1:13,1,prob=rep(c(n,m),c(9,4))/sum(rep(c(n,m),c(9,4))))
    x <- ifelse(x>10,10,x)
    return(x)
  }
  odd.10[i] <- 4*i/9 # p for large number: 4i/(9+4i), p for small number: 9/(9+4i); odd of large/small = odd.10
  player <- replicate(100000,game(12))
  dealer <- replicate(100000,game(16))
  result <-ifelse(player > dealer,1,ifelse(player < dealer,-1,0))
  result[player==0 & result==0] <- -1
  result.no.tie <- result[result!=0]
  win[i] <- length(result.no.tie[result.no.tie==1])/length(result.no.tie)
}

d <- ggplot(data.frame(odd.10,win),aes(odd.10,win))
d + geom_line(colour='lightskyblue4',size=1) + 
  geom_point(colour='red4',size=3.5) 
#  geom_hline(win=0.5,linetype=2)
