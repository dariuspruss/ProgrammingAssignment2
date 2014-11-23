makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) { ## set the value of the vector
                x <<- y
                m <<- NULL
        }
        get <- function() x ## get the value of the vector
        setmean <- function(mean) m <<- mean ## Set the value of the mean
        getmean <- function() m ## get the value of the mean
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}