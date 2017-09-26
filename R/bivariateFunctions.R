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
linBivParm <- function(coefLower = -4, coefUpper = 4,x1,x2){
	linUniParm(coefLower = coefLower, coefUpper = coefUpper)
}


#' linSplineBiv
#' @export
linSplineBiv <- function(x1, x2, nKnot, knotLoc, slopes){
	y <- rep(0, length(x1))
	kl <- c(min(x1*x2),knotLoc)
	# make basis
	for(k in 1:(nKnot+1)){
		xb <- pmax(0, x1*x2 - kl[k])
		y <- y + xb * slopes[k]
	}
	return(y)
}

#' linSplineBivParm
#' @export
linSplineBivParm <- function(x1,x2, slopeLower = -3, slopeUpper = 3, 
                             nKnotLower = 1, nKnotUpper = 5,
                             quantLower = 0.05, quantUpper = 0.95){
	nKnot <- round(runif(1, nKnotLower - 0.5, nKnotUpper + 0.5))
	slopes <- runif(nKnot + 1, slopeLower, slopeUpper)
	knotLoc <- sort(quantile(x1*x2, p = runif(nKnot, quantLower, quantUpper)))
	return(list(nKnot = nKnot, slopes = slopes, knotLoc = knotLoc))
}



#' linJumpBiv
#' 
#' Simulate a bivariate relationship with jump at random quantile of 
#' one variable.
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param coef A numeric (supplied by \code{linBivParm})
#' @param jumpLoc Location of randomly chosen quantile
#'
#' @return \code{coef*x1*x2}
#' @export
linJumpBiv <- function(x1,x2,coef,jumpLoc){
	return(coef*x1*as.numeric(x2 > jumpLoc))
}
#' linJumpBivParm
#' 
#' Generate coefficient for \code{linBiv} 
#' @export
linJumpBivParm <- function(coefLower = -4, coefUpper = 4,
                           quantLower = 0.1, quantUpper = 0.9, x1,x2){
	quant <- runif(1, quantLower, quantUpper)
	jumpLoc <- quantile(x2, p = quant)
	list(coef = runif(1, coefLower, coefUpper),
	     jumpLoc = jumpLoc)
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
                        deg2Lower = 1, deg2Upper = 3,x1,x2){
	npoly <- round(runif(1,npolyLower - 0.5, npolyUpper + 0.5))
	coef <- runif(npoly, coefLower, coefUpper)
	deg1 <- round(runif(npoly, deg1Lower - 0.5, deg1Upper + 0.5))
	deg2 <- round(runif(npoly, deg2Lower - 0.5, deg2Upper + 0.5))
	return(list(npoly = npoly, coef = coef, deg1 = deg1, deg2 = deg2))
}


#' polyJumpBiv
#' Simulate a polynomial bivariate relationship
#' 
#' @param x1 A vector
#' @param x2 A vector 
#' @param npoly A numeric indicating the number of polynomial terms to simulate
#' @param coef A vector indicating the coefficient for each polynomial term
#' @param deg1 A vector indicating the degree of the first polynomial term
#' @param jumpLoc Location of randomly chosen quantile
#' @param coef A numeric coefficient
#'
#' @return \code{coef[1]*x1^deg1[1]*x2^deg2[2] + ... + coef[npoly]*x1^deg1[npoly]*x2^deg2[2]}
#' @export

polyJumpBiv <- function(x1,x2,npoly,coef,deg1,jumpLoc){
	tmp <- rep(0, length(x1))
	for(i in 1:npoly){
		tmp <- tmp + coef[i]*x1^deg1[i]*as.numeric(x2 > jumpLoc)
	}
	return(tmp)
}
#'polyJumpBivParm
#' 
#' Generate parameters for \code{polyBiv}.
#' @export
polyJumpBivParm <- function(npolyLower = 1, npolyUpper = 2, 
                        coefLower = -0.25, coefUpper = 0.25, 
                        deg1Lower = 1, deg1Upper = 3,
                        quantLower = 0.1, quantUpper = 0.9,x1,x2){
	npoly <- round(runif(1,npolyLower - 0.5, npolyUpper + 0.5))
	coef <- runif(npoly, coefLower, coefUpper)
	deg1 <- round(runif(npoly, deg1Lower - 0.5, deg1Upper + 0.5))
	quant <- runif(1, quantLower, quantUpper)
	jumpLoc <- quantile(x2, p = quant)
	return(list(npoly = npoly, coef = coef, deg1 = deg1, jumpLoc = jumpLoc))
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
sinBivParm <- function(pLower = -1, pUpper = 1, ampLower = -1, ampUpper = 1,x1,x2){
	sinUniParm(pLower = pLower, pUpper = pUpper, ampLower = ampLower, ampUpper = ampUpper)
}

#' sinJumpBiv
#' 
#' Simulate a sinusoidal bivariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param p The periodicity of the sin function
#' @param amp The amplitude of the sin function
#' @param jumpLoc Location of randomly chosen quantile
#' @return \code{amp*sin(p*x)}
#' @export
sinJumpBiv <- function(x1,x2,amp,p,jumpLoc){
	w <- amp*sin(p*x1*as.numeric(x2 > jumpLoc))
	return(w)
}

#' sinJumpBivParm 
#' 
#' Generate parameters for \code{sinBiv}
#' @export
sinJumpBivParm <- function(pLower = -1, pUpper = 1, 
                           ampLower = -1, ampUpper = 1,
                           quantLower = 0.1, quantUpper = 0.9,x1,x2){
	p <- runif(1, pLower, pUpper)
	amp <- runif(1, ampLower, ampUpper)
	quant <- runif(1, quantLower, quantUpper)
	jumpLoc <- quantile(x2, p = quant)
	return(list(p = p, amp = amp, jumpLoc = jumpLoc))
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
	tmp <- rep(0, length(x1))
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


#' dNormAddBiv
#' 
#' Simulate a normal pdf relationship with two-way interaction
#' 
#' @param x1 A vector
#' @param x2 A vector
#' @param coef Multiplier in front of dnorm
#' @param mult1 Multiplier 1 inside dnorm
#' @param mult2 Multiplier 2 inside dnorm
#' @param mu Mean of dnorm
#' @param sig SD of dnorm
#' @export

dNormAddBiv <- function(x1,x2, coef, mult1, mult2, mu, sig){
	return(coef*dnorm(mult1*x1+mult2*x2, mu, sig))
}

#' dNormAddBivParm
#' 
#' Generate parameters for dNormAddBiv
#' @export
dNormAddBivParm <- function(x1,x2,coefLower=-5, coefUpper=5, mult1Lower=-1, mult1Upper=1, mult2Lower=-1, mult2Upper=1,
                            muLower = -2, muUpper = 2, sigLower = 0.5, sigUpper = 2){
	return(list(
       coef = runif(1, coefLower, coefUpper),
       mult1 = runif(1, mult1Lower, mult1Upper),
       mult2 = runif(1, mult2Lower, mult2Upper),
       mu = runif(1, muLower, muUpper),
       sig = runif(1, sigLower, sigUpper)  
    ))
}

#' dNormMultBiv
#' 
#' Simulate a normal pdf relationship with two-way interaction
#' 
#' @param x1 A vector
#' @param x2 A vector
#' @param coef Multiplier in front of dnorm
#' @param mult1 Multiplier 1 inside dnorm
#' @param mult2 Multiplier 2 inside dnorm
#' @export

dNormMultBiv <- function(x1,x2, coef, mult, mu, sig){
	return(coef*dnorm(mult*(x1*x2), mu, sig))
}

#' dNormMultBivParm
#' 
#' Generate parameters for dNormMultBiv
#' @export
dNormMultBivParm <- function(x1,x2,coefLower=-5, coefUpper=5, multLower=-1, multUpper=1,
                             muLower = -2, muUpper = 2, sigLower = 0.5, sigUpper = 2){
	return(list(
       coef = runif(1, coefLower, coefUpper),
       mult = runif(1, multLower, multUpper),
       mu = runif(1, muLower, muUpper),
       sig = runif(1, sigLower, sigUpper)    
    ))
}

#' dNormMultJumpBiv
#' 
#' Simulate a normal pdf relationship with two-way interaction
#' 
#' @param x1 A vector
#' @param x2 A vector
#' @param coef Multiplier in front of dnorm
#' @param mult Multiplier 1 inside dnorm
#' @param jumpLoc2 Location of random quantile jump
#' @export

dNormMultJumpBiv <- function(x1,x2, coef, mult, jumpLoc2, mu, sig){
	return(coef*dnorm(mult*(x1*as.numeric(x2 > jumpLoc2)), mu, sig))
}

#' dNormMultJumpBivParm
#' 
#' Generate parameters for dNormMultBiv
#' @export
dNormMultJumpBivParm <- function(x1,x2,coefLower=-5, coefUpper=5, multLower=-1, multUpper=1,
                             muLower = -2, muUpper = 2, sigLower = 0.5, sigUpper = 2,
                             quantLower = 0.1, quantUpper = 0.9){
    quant = runif(1, quantLower, quantUpper) 
	jl2 <- quantile(x2, p = quant)

	return(list(
       coef = runif(1, coefLower, coefUpper),
       mult = runif(1, multLower, multUpper),
       mu = runif(1, muLower, muUpper),
       sig = runif(1, sigLower, sigUpper),
       jumpLoc2 = jl2
    ))
}

#' pLogisAddBiv
#' 
#' @param x1 A vector
#' @param x2 A vector
#' @param coef Multiplier in front of plogis
#' @param mult1 Multiplier 1 inside plogis
#' @param mult2 Multiplier 2 inside plogis 
#' @param loc The location parameter for plogis
#' @param scale The scale parameter
#' @export
pLogisAddBiv <- function(x1,x2,coef, mult1, mult2, loc, scale){
	return(coef*plogis(mult1*x1 + mult2*x2, loc, scale))
}

#' pLogisAddBivParm
#' 
#' Generate parameters for \code{pLogisAddBiv}
#' 

pLogisAddBiv <- function(x1,x2,coefLower=-4, coefUpper=4, mult1Lower=-1, mult1Upper=1, mult2Lower=-1, mult2Upper=1,
                         locLower = -2, locUpper = 2, scaleLower = 0.25, scaleUpper = 2){
	return(list(
       coef = runif(1, coefLower, coefUpper),
       mult1 = runif(1, mult1Lower, mult1Upper),
       mult2 = runif(1, mult2Lower, mult2Upper),
       loc = runif(1, locLower, locUpper),
       scale = runif(1, scaleLower, scaleUpper)
    ))
}
