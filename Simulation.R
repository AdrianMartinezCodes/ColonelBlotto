##rand_vect used from: 
rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}

genNums <- function(n){
  mat <- matrix(data = replicate(n,rand_vect(10,100,9)),nrow = n,ncol = 10, byrow = T)
  return(mat)
}


playGame<- function(Mat,Vec){
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
sort(c(9,5,20,4,15,16,15,3,6,1,3,3))

