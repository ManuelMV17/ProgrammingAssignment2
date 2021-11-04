## Funciones que almacenan en caché la inversa de una matriz.

## Esta función crea un objeto de matriz especial que puede almacenar en caché
## su inversa.

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## Esta función calcula la inversa de la matriz devuelta por 
## la función de arriba makeCacheMatrix. Si la inversa ya fue calculada,
## entonces la función cacheSolve debería de recupera el inverso de la caché.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Obteniendo datos en cache...")
        return(inv)
    }
    dat <- x$get()
    inv <- solve(dat, ...)
    x$setInverse(inv)
    inv
}
