library(doParallel)
library(foreach)
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
dk <- read.csv("randomer.csv")
dk <- lapply(split(dk, dk$Position), function(x) x[sample(20), ])
#dk <- dk[-1] 
dk <- dk[c("G","W","C","D","U")]
4*choose(4,3)*choose(4,2)*choose(4,2)*4
#rows <- list(t(1:4), combn(4,4), combn(4,2), t(1:4))
rows <- list(t(1:4), combn(4,3), combn(4,2), combn(4,2),t(1:4))  # these are possible combinations of each position
dims <- sapply(rows, NCOL)
inds <- expand.grid(mcmapply(`:`, 1, dims, mc.cores = (cores[1]-1)))             # indicies of combinations in 'rows'
dim(inds)
# [1] 444   4

## Function to extract a group
extract <- function(ind) {
  g <- inds[ind,]
  do.call(rbind, lapply(1:5, function(i) dk[[i]][rows[[i]][,g[[i]]], ]))
}

## So, one combination would be 
extract(1)
res <- mclapply(1:dim(inds), extract, mc.cores = (cores[1]-1))

points <- c();for(i in 1:dim(inds)){points <- c(points, sum(res[[i]]$AvgPointsPerGame))}

salary <- c();for(i in 1:dim(inds)){salary <- c(salary, sum(res[[i]]$Salary))}
