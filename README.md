## Estas funciones permiten calcular y almacenar en caché la inversa de una matriz 
## para evitar cálculos repetitivos costosos.

## 'makeCacheMatrix' crea un objeto especial "matriz" que puede almacenar su inversa en caché.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Asigna la matriz
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Obtiene la matriz
  get <- function() x
  
  # Asigna la inversa
  setinverse <- function(inverse) inv <<- inverse
  
  # Obtiene la inversa
  getinverse <- function() inv
  
  # Retorna la lista de funciones internas
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## 'cacheSolve' calcula la inversa del objeto especial retornado por makeCacheMatrix.
## Si la inversa ya fue calculada (y la matriz no ha cambiado), la recupera de la caché.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # Verifica si la inversa ya está guardada en caché
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Si no está guardada, calcula la inversa
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  # Retorna el resultado
  inv
}
