testCachedMatrix <- function(matrix)
{
    # first we load the matrix using makeCacheMatrix
    TestingMatrix <- makeCacheMatrix(matrix)
    print(TestingMatrix)
    
    # we would like to quantify whether there is a benefit to caching :-)
    StartTime<- Sys.time()
    cacheSolve(TestingMatrix)
    FirstRunDuration <- Sys.time() - StartTime
    print(paste("First run duration: ", FirstRunDuration))
    
    StartTime <- Sys.time()
    cacheSolve(TestingMatrix)
    SecondRunDuration <- Sys.time() - StartTime
    print(paste("Second run duration: ", SecondRunDuration))
    
    TimeSavedByCaching <- FirstRunDuration - SecondRunDuration
    print("")
    print(paste("Total time saved using caching: ", TimeSavedByCaching))
}