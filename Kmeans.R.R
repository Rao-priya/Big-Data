genData <- function(N,D,k) {
    set.seed(5)
    all_points <- matrix(sample(1:(N*D), (N*D), replace = TRUE), nrow = D, ncol = N)    
    centers <- all_points[ ,sample(1:N, k, replace = FALSE)]      
    l<-list(all_points,centers)
   # print(l)
    return(l)  
    
}
closest <- function(all_points, centers) {
    label <- matrix( ,nrow = 1, ncol = ncol(all_points))   
    for(i in 1:ncol(all_points)) {    
       res<-do.call(cbind, lapply(split(centers,col(centers)), function(x) dist(rbind(x,all_points[,i])))#colwise sub
        label[1,i] = apply(res,1,which.min)        
    }     
    n <- matrix(nrow = nrow(all_points), ncol = 0)
    for(j in 1:ncol(centers)) {
        point <- all_points[ ,which(label == j)]
        if(is.vector(point)) {
            point = as.matrix(point)            
        }         
        n <- cbind(n, rowMeans(point, na.rm=TRUE))
    }    
    newl <- list(label,n)
   # print(newl)
    return(newl)
}
myKmeans <- function(all_points,centers,iteration) {    
    for(i in 1:iteration) {         
       if(i <= 1) {
           newl <- closest(all_points, centers)                      
       }  else {
           newl <- closest(all_points, newl[[2]])        
       }          
    } 
    return newl
}

l<-genData(10000,10,20)
system.time(newl<-myKmeans(l[[1]],l[[2]],50))




