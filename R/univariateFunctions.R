#' linUni 
#' 
#' Simulate a linear univariate relationship
#' 
#' @param x A vector 
#' @param coef A numeric (supplied by \code{linUniParm})
#'
#' @return \code{coef*x}
#' @export

linUni <- function(x, coef){
	# coef <- runif(1, -2, 2)
	return(coef*x)
}

#' linUniParm
#' 
#' Draw random coefficient for linear univariate relationship
#' @export
linUniParm <- function(coefLower = -2, coefUpper = 2, ...){
	return(list(coef = runif(1, coefLower, coefUpper)))
}

#' polyUni
#' 
#' Simulate a polynomial univariate relationship
#' 
#' @param x A vector 
#' @param npoly A numeric indicating the number of polynomial terms to simulate
#' @param coef A vector indicating the coefficient for each polynomial term
#' @param deg A vector indicating the degree of each polynomial term
#' @param coef A numeric (supplied by \code{linUniParm})
#'
#' @export
#' @return \code{coef[1]*x^deg[1] + ... + coef[npoly]*x^deg[npoly]}

polyUni <- function(x, npoly, coef, deg){
	# npoly <- round(runif(1,0.5,3.5))
	# coef <- runif(npoly, -1, 1)
	# deg <- round(runif(npoly, 0.5, 3.5))
	tmp <- rep(0, length(x))
	for(i in 1:npoly){
		tmp <- tmp + coef[i]*x^deg[i]
	}
	return(tmp)
}

#' polyUniParm
#' 
#' Draw random coefficient for polynomial relationship univariate relationship.
#' See source code for distributions. 
#'  @export
polyUniParm <- function(npolyLower = 1, npolyUpper = 3, coefLower = -1, coefUpper = 1,
                        degLower = 1, degUpper = 3, ...){
	npoly <- round(runif(1, npolyLower - 0.5, npolyUpper + 0.5))
	coef <- runif(npoly, coefLower, coefUpper)
	deg <- round(runif(npoly, 0.5, 3.5))
	return(list(npoly = npoly, coef = coef, deg = deg))
}


#' sinUni
#' 
#' Simulate a sinusoidal univariate relationship
#' 
#' @param x A vector 
#' @param p The periodicity of the sin function
#' @param amp The amplitude of the sin function
#'
#' @return \code{amp*sin(p*x)}
#'  @export
sinUni <- function(x, p, amp){
	w <- amp*sin(p*x)
	return(w)
}

#' sinUniParm
#' 
#' Draw random periodicity and amplitude for sinusoidal relationship univariate relationship.
#' See source code for distributions. 
#'  @export
sinUniParm <- function(pLower = -1, pUpper = 1, ampLower = -1, ampUpper = 1, ...){
	p <- runif(1, pLower, pUpper)
	amp <- runif(1, ampLower, ampUpper)
	return(list(p = p, amp = amp))
}

#' jumpUni
#' 
#' Simulate a jump function univariate relationship
#' 
#' @param x A vector 
#' @param njump The number of jumps the function takes
#' @param jumpLoc A vector of the locations of those jumps
#' @param jumpVal A vector of length njump indicating the value of the function at each jump
#'
#' @return The value of the jump function. 
#'  @export

jumpUni <- function(x, njump, jumpLoc, jumpVal){
	tmp <- rep(0, length(x))
	for(i in 1:(length(jumpLoc)-1)){
		ind <- (x <= jumpLoc[i+1] & x >= jumpLoc[i])
		tmp[ind] <- jumpVal[i]
	}
	return(tmp)
}

#' jumpUniParm 
#' 
#' Generate parameters for \code{jumpUni}. Here jumpLoc is a function that takes x and njump as input and outputs 
#' the locations of jumps. By contrast, when jumpLoc is fed into jumpUni, it is just a vector
#' of numbers. See source code for how exactly it works. 
#' @export

jumpUniParm <- function(x, njumpLower = 1, njumpUpper = 5, jumpLower = -2, jumpUpper = 2,
                        jumpLoc = function(x,njump){ quantile(x, seq(0,1,length=njump+1)) }){
	njump <- round(runif(1, njumpLower - 0.5, njumpUpper + 0.5))
	jumpL <- do.call("jumpLoc", args = list(x = x, njump = njump))
	jumps <- runif(njump, jumpLower, jumpUpper)
	return(list(
	   njump = njump, jumpLoc = jumpL, jumpVal = jumps
	))
}
