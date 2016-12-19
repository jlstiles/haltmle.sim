#' linBiv
#' 
#' Simulate a bivariate univariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param coef A numeric (supplied by \code{bivUniParm})
#'
#' @return \code{coef*x1*x2}
#' @export
linBiv <- function(x1,x2,coef){
	#coef <- runif(1, -2, 2)
	return(coef*x1*x2)
}
#' linBivParm
#' 
#' Generate coefficient for \code{linBiv} 
#' @export
linBivParm <- function(coefLower = -2, coefUpper = 2,...){
	linUniParm(...,coefLower = coefLower, coefUpper = coefUpper)
}


#' polyBiv
#' Simulate a polynomial bivariate relationship
#' 
#' @param x1 A vector
#' @param x2 A vector 
#' @param npoly A numeric indicating the number of polynomial terms to simulate
#' @param coef A vector indicating the coefficient for each polynomial term
#' @param deg1 A vector indicating the degree of the first polynomial term
#' @param deg2 A vector indicating the degree of the second polynomial term
#' @param coef A numeric coefficient
#'
#' @return \code{coef[1]*x1^deg1[1]*x2^deg2[2] + ... + coef[npoly]*x1^deg1[npoly]*x2^deg2[2]}
#' @export

polyBiv <- function(x1,x2,npoly,coef,deg1,deg2){
	tmp <- rep(0, length(x1))
	for(i in 1:npoly){
		tmp <- tmp + coef[i]*x1^deg1[i]*x2^deg2[i]
	}
	return(tmp)
}
#'polyBivParm
#' 
#' Generate parameters for \code{polyBiv}.
#' @export
polyBivParm <- function(npolyLower = 1, npolyUpper = 2, 
                        coefLower = -0.25, coefUpper = 0.25, 
                        deg1Lower = 1, deg1Upper = 3, 
                        deg2Lower = 1, deg2Upper = 3,...){
	npoly <- round(runif(1,npolyLower - 0.5, npolyUpper + 0.5))
	coef <- runif(npoly, coefLower, coefUpper)
	deg1 <- round(runif(npoly, deg1Lower - 0.5, deg1Upper + 0.5))
	deg2 <- round(runif(npoly, deg2Lower - 0.5, deg2Upper + 0.5))
	return(list(npoly = npoly, coef = coef, deg1 = deg1, deg2 = deg2))
}

#' sinBiv
#' 
#' Simulate a sinusoidal bivariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param p The periodicity of the sin function
#' @param amp The amplitude of the sin function
#'
#' @return \code{amp*sin(p*x)}
#' @export
sinBiv <- function(x1,x2,amp,p){
	w <- amp*sin(p*x1*x2)
	return(w)
}

#' sinBivParm 
#' 
#' Generate parameters for \code{sinBiv}
#' @export
sinBivParm <- function(pLower = -1, pUpper = 1, ampLower = -1, ampUpper = 1,...){
	sinUniParm(pLower = pLower, pUpper = pUpper, ampLower = ampLower, ampUpper = ampUpper, ...)
}


#' jumpBiv
#' 
#' Simulate a jump function bivariate relationship
#' 
#' @param x1 A vector
#' @param x2 A vector 
#' @param njump The number of jumps the function takes
#' @param jumpLoc1 A vector of the locations of those jumps for x1
#' @param jumpLoc2  A vector of the locations of those jumps for x2
#' @param jumpVal A vector of length njump indicating the value of the function at each jump
#'
#' @return The value of the jump function. 
#' @export
jumpBiv <- function(x1, x2, njump, jumpLoc1, jumpLoc2, jumpVal){
	#njump <- round(runif(1, 0.5, 5.5))
	#jumpLoc1 <- quantile(x1,seq(0,1,length=njump+1))
	#jumpLoc2 <- quantile(x2,seq(0,1,length=njump+1))
	tmp <- rep(0, length(x))
	#jumps <- rep(NA, njump)
	for(i in 1:(length(jumpLoc1)-1)){
		ind <- (x1 <= jumpLoc1[i+1] & x1 >= jumpLoc1[i] & x2 <= jumpLoc2[i+1] & x2 >= jumpLoc2[i])
		tmp[ind] <- jumpVal[i]
	}
	return(tmp)
}

#' jumpBivParm
#' 
#' Generate parameters for \code{jumpBiv}. See source code to see what it does.
#' @export 
jumpBivParm <- function(x1, x2, njumpLower = 1, njumpUpper = 5, 
                        jumpLower = -2, jumpUpper = 2,
                        jumpLoc1 = function(x,njump){ quantile(x, seq(0,1,length=njump+1)) },
                        jumpLoc2 = function(x,njump){ quantile(x, seq(0,1,length=njump+1)) }
                        ){
	njump <- round(runif(1, njumpLower - 0.5, njumpUpper + 0.5))
	jumpL1 <- do.call("jumpLoc1", args = list(x = x1, njump = njump))
	jumpL2 <- do.call("jumpLoc2", args = list(x = x2, njump = njump))
	jumps <- runif(njump, jumpLower, jumpUpper)
	return(list(
	   njump = njump, jumpLoc1 = jumpL1, jumpLoc2 = jumpL2, jumpVal = jumps
	))
}
