source("InternetDist.R")


genNums <- function(n,sd){
  mat <- matrix(data = replicate(n,rand_vect(10,100,sd)),nrow = n,ncol = 10, byrow = T)
  return(mat)
}

playGame<- function(Mat){
  n = nrow(Mat)
  score <- numeric(n)
  win <- numeric(n)
  lose <- numeric(n)
  ties <- numeric(n)
  for(i in 1:n){
    for(j in i:n){
      if(i != j){
        temp <- Mat[i,] - Mat[j,]
        playerB <- sum(temp < 0)
        playerA <- sum(temp > 0)
        if(playerA > playerB){
          score[i] = score[i] + 1
          win[i] = win[i] + 1
          lose[j] = lose[j] + 1
        }
        else if(playerB >  playerA ){
          score[j] = score[j] + 1
          win[j] = win[j] +1
          lose[i] = lose[i] + 1
        }
        else{
          score[i] = score[i] + 0.5
          score[j] = score[j] + 0.5
          ties[i] = ties[i] + 1
          ties[j] = ties[j] + 1
        }
      }
    }
  }
  gameDF<- data.frame(score,win,lose,ties,Mat)
  return(gameDF)
}


winning.Scores <- read.csv("blottoscores_bonus_piazza_fixed.csv")
winning.Scores <- (as.matrix(winning.Scores))
winning.Scores <- subset(winning.Scores,select= c(8:17))
winning.Scores <- mapply(winning.Scores, FUN=as.numeric)
winning.Scores <- matrix(data=winning.Scores,ncol = 10)


classDF <-  playGame(winning.Scores)
#classDF <-  playGame(matrix(mapply(winning.Scores[,8:17],FUN=as.numeric), ncol= 10))


R = 10
rounds = 0
sd = 1
repeat{
  best_vec <- list(0,numeric(10))
  
  n = 1000
  mat <- genNums(n)
  df<- as.data.frame(mat)
  
  df[(n+1):(n+nrow(winning.Scores)),] <- winning.Scores[1:77,]
  df <- playGame(mat)
  which.max(df$score)
  as.numeric(df[which.max(df$score),5:14])
  win_vec <- list(as.numeric(which.max(df$score)),as.vector(as.numeric(df[which.max(df$score),5:14])))
  # if(best_vec[[1]]< win_vec[[1]]){
  #   best_vec <- win_vec
  # }
  winning.Scores[77,] <- win_vec[[2]]
  classDF <-  playGame(winning.Scores)
  if(which.max(classDF$score) == 77){
    break
  }
}
n = 1
i = 1
sd=1
highest_vec = numeric(10)
repeat{
  
  if(i > 100){
    sd = sd + 1
    i = 0
  }
  mat <- genNums(n,sd)
  df<- as.data.frame(mat)
  
  df[(n+1):(n+nrow(winning.Scores)),] <- winning.Scores[1:77,]
  results <- playGame(df)
  #which.max(df$score)
  #as.numeric(df[which.max(df$score),5:14])

  if(which.max(results$score) == 1){
    highest_vec <- as.numeric(results[which.max(results$score),5:14])
    break
  }
  if(sd > 15){
    sd = 1
  }
  i = i + 1
}
df[1,1:10] <- c()
