source("InternetDist.R")


genNums <- function(n,dist){
  mat <- matrix(data = replicate(n,rand_vect(10,100,9)),nrow = n,ncol = 10, byrow = T)
  return(mat)
}

playGame<- function(Mat,vec, win,lose,ties){
  n = nrow(Mat)
  for(i in 1:n){
    for(j in i:n){
      if(i != j){
        temp <- Mat[i,] - Mat[j,]
        playerB <- sum(temp <= 0)
        playerA <- sum(temp >= 0)
        if(playerB >  playerA ){
          vec[j] = vec[j] + 1
          win[j] = win[j] +1
          lose[i] = lose[i] + 1
        }
        else if(playerA > playerB){
          vec[i] = vec[i] + 1
          win[i] = win[i] + 1
          lose[j] = lose[j] + 1
        }
        else{
          vec[i] = vec[i] + 0.5
          vec[j] = vec[j] + 0.5
          ties[i] = ties[i] + 1
          ties[j] = ties[j] + 1
        }
      }
    }
  }
  gameDF<- data.frame(vec,win,lose,ties)
  return(gameDF)
}


winning.Scores <- read.csv("blottoscores_bonus_piazza.csv")
winning.Scores <- (as.matrix(winning.Scores))
winning.Scores <- subset(winning.Scores,select= c(8:17))
winning.Scores <- mapply(winning.Scores, FUN=as.numeric)
winning.Scores <- matrix(data=winning.Scores,ncol = 10)
scoreVec <- numeric(nrow(winning.Scores))
winVec <-numeric(nrow(winning.Scores))
loseVec <-numeric(nrow(winning.Scores))
tiesVec <-numeric(nrow(winning.Scores))

classDF <-  playGame(winning.Scores,scoreVec,winVec,loseVec,tiesVec )




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



