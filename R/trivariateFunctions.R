
#' linTri
#' 
#' Simulate a trivariate univariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param x3 A vector
#' @param coef A numeric (supplied by \code{bivUniParm})
#'
#' @return \code{coef*x1*x2*x3}
#' @export
linTri <- function(x1,x2,x3,coef){
	return(coef*x1*x2*x3)
}
#' linTriParm
#' 
#' Generate parameters for \code{linTri}.

linTriParm <- function(coefLower = -2, coefUpper = 2,...){
	linUniParm(...,coefLower = coefLower, coefUpper = coefUpper)
}

#' polyTri
#' Simulate a polynomial trivariate relationship
#' 
#' @param x1 A vector
#' @param x2 A vector 
#' @param x3 A vector
#' @param npoly A numeric indicating the number of polynomial terms to simulate
#' @param coef A vector indicating the coefficient for each polynomial term
#' @param deg1 A vector indicating the degree of the first polynomial term
#' @param deg2 A vector indicating the degree of the second polynomial term
#' @param deg3 A vector indicating the degree of the second polynomial term
#' @param coef A numeric coefficient
#' 
#' @return Trivariate polynomial (see source code for details)
#' @export

polyTri <- function(x1, x2, x3, coef, deg1, deg2, deg3){
	tmp <- rep(0, length(x1))
	for(i in 1:npoly){
		tmp <- tmp + coef[i]*x1^deg1[i]*x2^deg2[i]*x3^deg3[i]
	}
	return(tmp)
}
#' polyBivParm
#' 
#' Generate parameters for \code{polyTri}. See source code.
#' @export
polyTriParm <- function(npolyLower = 1, npolyUpper = 2, 
                        coefLower = -0.25, coefUpper = 0.25, 
                        deg1Lower = 1, deg1Upper = 3, 
                        deg2Lower = 1, deg2Upper = 3,
                        deg3Lower = 1, deg3Upper = 3,
                        ...){
	npoly <- round(runif(1,npolyLower - 0.5, npolyUpper + 0.5))
	coef <- runif(npoly, coefLower, coefUpper)
	deg1 <- round(runif(npoly, deg1Lower - 0.5, deg1Upper + 0.5))
	deg2 <- round(runif(npoly, deg2Lower - 0.5, deg2Upper + 0.5))
	deg3 <- round(runif(npoly, deg3Lower - 0.5, deg3Upper + 0.5))
	return(list(npoly = npoly, coef = coef, deg1 = deg1, deg2 = deg2, deg3 = deg3))
}

#' sinTri
#' 
#' Simulate a sinusoidal trivariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param x3 A vector
#' @param p The periodicity of the sin function
#' @param amp The amplitude of the sin function
#'
#' @return \code{amp*sin(p*x1*x2*x3)}
#' @export
sinTri <- function(x1,x2,x3,p,amp){
	w <- amp*sin(p*x1*x2*x3)
	return(w)
}
#' sinTriParm
#' 
#' Generate parameters for \code{sinTri}. See source code. 
#' @export
sinTriParm <- function(pLower = -1, pUpper = 1, ampLower = -1, ampUpper = 1,...){
	sinUniParm(pLower = pLower, pUpper = pUpper, ampLower = ampLower, ampUpper = ampUpper, ...)
}

#' jumpTri
#' 
#' Simulate a jump function trivariate relationship
#' 
#' @param x1 A vector
#' @param x2 A vector 
#' @param x3 A vector
#' @param njump The number of jumps the function takes
#' @param jumpLoc1 A vector of the locations of those jumps for x1
#' @param jumpLoc2  A vector of the locations of those jumps for x2
#' @param jumpLoc3 A vector of the locations of those jumps for x3
#' @param jumpVal A vector of length njump indicating the value of the function at each jump
#'
#' @return The value of the jump function. 
#' @export
jumpTri <- function(x1, x2, x3, njump, jumpLoc1, jumpLoc2, jumpLoc3, jumpVal){
	tmp <- rep(0, length(x))
	for(i in 1:(length(jumpLoc1)-1)){
		ind <- (x1 <= jumpLoc1[i+1] & x1 >= jumpLoc1[i] & x2 <= jumpLoc2[i] & x2 >= jumpLoc2[i] &
		        x3 <= jumpLoc3[i] & x3 >= jumpLoc3[i+1])
		tmp[ind] <- jumpVal[i]
	}
	return(tmp)
}
#' jumpTriParm
#' 
#' Generate parameters for \code{jumpTri}. See source code and \code{?jumpUni}.
#' @export 
jumpTriParm <- function(x, njumpLower = 1, njumpUpper = 5, 
                        jumpLower = -2, jumpUpper = 2,
                        jumpLoc1 = function(x,njump){ quantile(x, seq(0,1,length=njump+1)) },
                        jumpLoc2 = function(x,njump){ quantile(x, seq(0,1,length=njump+1)) },
                        jumpLoc3 = function(x,njump){ quantile(x, seq(1,0,length=njump+1)) }
                        ){
	njump <- round(runif(1, njumpLower - 0.5, njumpUpper + 0.5))
	jumpL1 <- do.call("jumpLoc1", args = list(x = x1, njump = njump))
	jumpL2 <- do.call("jumpLoc2", args = list(x = x2, njump = njump))
	jumpL3 <- do.call("jumpLoc3", args = list(x = x3, njump = njump))
	jumps <- runif(njump, jumpLower, jumpUpper)
	return(list(
	   njump = njump, jumpLoc1 = jumpL1, jumpLoc2 = jumpL2, jumpLoc3 = jumpL3, jumpVal = jumps
	))
}

