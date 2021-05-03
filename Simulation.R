source("InternetDist.R")


genNums <- function(n,dist){
  mat <- matrix(data = replicate(n,rand_vect(10,100,9)),nrow = n,ncol = 10, byrow = T)
  return(mat)
}

playGame<- function(Mat,Vec){
  n = ncol(Mat)
  for(i in 1:n){
    for(j in i:n){
      if(i!= j){
        temp <- Mat[i,] - Mat[j,]
        playerB <- sum(temp < 0) + sum(temp == 0)
        playerA <- sum(temp > 0) + sum(temp == 0)
        if(playerB >  playerA ){
          Vec[j] = Vec[j] + 1
        }
        else if(playerA > playerB){
          Vec[i] = Vec[i] + 1
        }
        else{
          Vec[i] = Vec[i] + 0.5
          Vec[j] = Vec[j] + 0.5
        }
      }
    }
  }
  return(Vec)
}
R = 10
rounds = 0
repeat{
  best_vec <- list(numeric(10), 0)
  
  n = 1000
  mat <- genNums(n)
  vec <- numeric(n)
  vec <- playGame(mat,vec)
  which.max(vec)
  max(vec)
  win_vec <- list(mat[which.max(vec),], max(vec))
  if(best_vec[[2]] < win_vec[[2]]){
    best_vec <- win_vec
  }
  rounds = rounds +1
  if(rounds == R){
    break
  }
}


winning.Scores <- read.csv("blottoscores_bonus_piazza.csv")
winning.Scores <- (as.matrix(winning.Scores))
winning.Scores <- subset(winning.Scores,select= c(8:17))
winVec <- numeric(10)
playGame(winning.Scores, winVec)


