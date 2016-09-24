test_lapply <- function() {
	print("lapply runs a function in a list")

	#ex1 - simple
	x <- list(a = 1:5, b = rnorm(10))
	ex1 <- lapply(x, mean)

	#ex2 - more complex
	x <- list(a = 1:4, b = rnorm(10), c = rnorm(20,1), d = rnorm(100,5))
	ex2 <- sapply(x, mean)

	#ex3 - runif
	x <- 1:4
	ex3 <- sapply(x, runif)

	#ex4 - runif with min and max
	x <- 1:4
	ex4 <- sapply(x, runif, min = 0, max = 10)

	#ex5 - funcao anonima
	x <- list(a = matrix(1:4, 2,2), b = matrix(1:6, 3,2))
	ex5 <- sapply(x, function(elt) elt[,1])

	list(ex1 = ex1, ex2 = ex2, ex3 = ex3, ex4 = ex4, ex5 = ex5)
}


test_apply <- function(){
	# function(X, MARGIN, FUN, ...)

	print("apply runs a function in an array")
	#ex1 - simple
	x <- matrix(rnorm(200), 20, 10)
	ex1a <- apply(x, 2, mean)
	ex1b <- apply(x, 1, sum)

	#ex2 - quantile
	x <- matrix(rnorm(200), 20, 10)
	ex2 <- apply(x, 1, quantile, probs = c(0.25, 0.75))

	#ex3 - array mean c(1,2)
	a <- array(rnorm(2*2*10), c(2,2,10))
	ex3a <- apply(a, c(1,2), mean)
	ex3b <- rowMeans(a, dims = 2)


	list(ex1a = ex1a, ex1b = ex1b, ex2 = ex2, ex3a = ex3a, ex3b = ex3b)
}

test_mapply <- function(){
	#function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAME = TRUE)
	print("mapply applys a function in parallel over a set of arguments")
}
