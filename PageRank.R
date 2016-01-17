library(igraph)
library('Matrix')
getAdjMatrix <- function(fname) {    
    g1 <- read.graph(fname,format="edgelist")
    adj <- get.adjacency(g1,type=c("upper"), sparse=TRUE)
    summary(g1)
    adj<-t(adj)
    return(adj)
}
adj <- getAdjMatrix("socialgraph_spc.dat")
getTransitionMatrix <- function(adj) {    
    N <- ncol(adj) 
    adj <-t(adj);
    c <- ifelse(is.finite(1/colSums(adj)),1/colSums(adj),0)   
    d <- Diagonal(N,c) 
    
    trans_adj <-adj %*% d  
    #print(trans_adj)
    z <- matrix(1/N, nrow = 1, ncol = N)   
    ind <- which(adj!=0, arr.ind = TRUE)
    z[ ,unique(ind[,2])] <- 0.15/N
  
    Trans_A <- list(trans_adj, z)    
    return(Trans_A)
}
myPageRank<-function(T,z,iteration) {
    N <- nrow(T)    
    xold <- matrix(1/N, nrow=N, ncol=1)    
    xnew <- matrix(1/N, nrow=N, ncol=1)
    for(i in 1:iteration) {
        xnew = ((0.85*T) %*% xold) + as.numeric((z %*% xold))
        xold  <- xnew
    }
    return(xold)
}
adj <- getAdjMatrix("socialgraph_spc.dat")
trans_A <- getTransitionMatrix(adj)
rank <- myPageRank(trans_A[[1]], trans_A[[2]], 1)
print(rank)

#hw3-priyanka-rao_transpose(adj) file takes transpose of adjacency matrix (A) because the read graph represent edges rowwise in matrix A . 
#to match the statement in  task 2 col j has "k outgoing edges" we take transpose of adjacency matrix