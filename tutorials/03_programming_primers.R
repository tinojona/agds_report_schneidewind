### Programming Primers

### Exercises



## Gauss Variations

# for loop
summe <- 0

for (i in 1:100) {
  summe <- i + summe
}

print(summe)

# while loop
summe <- 0
k <- 1

while (k < 101) {
  summe = k + summe
  k <- k + 1
}

print(summe)

# multiples of 3 and 7
summe <- 0

for (i in 1:100){
  if(i %% 3 == 0 & i %% 7 == 0){
    summe <- summe + i
  }
}

print(paste0("The sum of multiples of 3 and 7 within 1-100 is: ", summe))



## Nested Loops
mymat <- matrix(c(6, 7, 3, NA, 15, 6, 7,
                  NA, 9, 12, 6, 11, NA, 3,
                  9, 4, 7, 3, 21, NA, 6,
                  rep(NA, 7)),
                nrow = 4, byrow = TRUE)

myvec <- c(8, 4, 12, 9, 15, 6)

# iterate through rows
for (i in 1:4){

  # fill up myvec again
  myvec <- c(8, 4, 12, 9, 15, 6)

  # iterate through cols
  for (k in 1:7){

    # check for NA
    if(is.na(mymat[i,k])){

      # extract maximum
      myval <- max(myvec)

      # replace with max value
      mymat[i,k] <- max(myvec, na.rm = T)

      # delete max value from myvec
      myvec <- myvec[! myvec %in% max(myvec)]


      # change -Inf back to NA
      if(mymat[i,k] == -Inf){
        mymat[i,k] <- NA
      }
    }
  }
}



## Interpolation

vec <- rep(NA,100)
vec[1:25] <- 6
vec[66:100] <- -20

# extract gradient
rate <- (6- -20) / length(26:65)

# assign gradient
for (i in 26:65){
  vec[i] = vec[i-1] - rate
}

# plot vec
plot(vec)

