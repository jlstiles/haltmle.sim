#' linQuad
#' 
#' Simulate a trivariate univariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param coef A numeric (supplied by \code{bivUniParm})
#'
#' @return \code{coef*x1*x2*x3}
#' @export
linQuad <- function(x1,x2,x3,x4,coef){
	return(coef*x1*x2*x3*x4)
}
#' linQuadParm
#' 
#' Generate parameters for \code{linQuad}.
#' @export

linQuadParm <- function(coefLower = -2, coefUpper = 2,x1,x2,x3,x4){
	linUniParm(coefLower = coefLower, coefUpper = coefUpper)
}

#' linJump2Quad
#' 
#' Simulate a trivariate univariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param jumpLoc2 Random quantile for x2 jump
#' @param jumpLoc3 Random quantile for x3 jump
#' @param coef A numeric (supplied by \code{bivUniParm})
#'
#' @return \code{coef*x1*x2*x3}
#' @export
linJump2Quad <- function(x1,x2,x3,x4,coef,jumpLoc2,jumpLoc3){
	return(coef*x1*x4*as.numeric(x2 > jumpLoc2)*as.numeric(x3 > jumpLoc3))
}
#' linJump2QuadParm
#' 
#' Generate parameters for \code{linQuad}.
#' @export

linJump2QuadParm <- function(coefLower = -10, coefUpper = 10,
                           quantLower = 0.1, quantUpper = 0.9, x1,x2,x3,x4){
	quant2 = runif(1, quantLower, quantUpper)    
	quant3 = runif(1, quantLower, quantUpper)
	jl2 <- quantile(x2, p = quant2)
	jl3 <- quantile(x3, p = quant3)	
	list(coef = runif(1, coefLower, coefUpper),
	     jumpLoc2 = jl2, jumpLoc3 = jl3)
}

#' linJump1Quad
#' 
#' Simulate a trivariate univariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param jumpLoc3 A numeric quantile for x3
#' @param coef A numeric (supplied by \code{bivUniParm})
#'
#' @return \code{coef*x1*x2*x3}
#' @export
linJump1Quad <- function(x1,x2,x3,x4,coef,jumpLoc3){
	return(coef*x1*x2*x4*as.numeric(x3 > jumpLoc3))
}
#' linQuadParm
#' 
#' Generate parameters for \code{linQuad}.
#' @export

linJump1QuadParm <- function(coefLower = -10, coefUpper = 10,
                           quantLower = 0.1, quantUpper = 0.9, x1,x2,x3,x4){
	quant3 <- runif(1, quantLower, quantUpper)
	jl3 <- quantile(x2, p = quant3)
	list(coef = runif(1, coefLower, coefUpper),
	     jumpLoc3 = jl3)
}


#' polyQuad
#' Simulate a polynomial trivariate relationship
#' 
#' @param x1 A vector
#' @param x2 A vector 
#' @param x3 A vector
#' @param x4 A vector
#' @param npoly A numeric indicating the number of polynomial terms to simulate
#' @param coef A vector indicating the coefficient for each polynomial term
#' @param deg1 A vector indicating the degree of the first polynomial term
#' @param deg2 A vector indicating the degree of the second polynomial term
#' @param deg3 A vector indicating the degree of the second polynomial term
#' @param coef A numeric coefficient
#' 
#' @return Quadvariate polynomial (see source code for details)
#' @export

polyQuad <- function(x1, x2, x3, x4, npoly, coef, deg1, deg2, deg3, deg4){
	tmp <- rep(0, length(x1))
	for(i in 1:npoly){
		tmp <- tmp + coef[i]*x1^deg1[i]*x2^deg2[i]*x3^deg3[i]*x4^deg4[i]
	}
	return(tmp)
}
#' polyQuadParm
#' 
#' Generate parameters for \code{polyQuad}. See source code.
#' @export
polyQuadParm <- function(npolyLower = 1, npolyUpper = 1, 
                        coefLower = -0.1, coefUpper = 0.1, 
                        deg1Lower = 1, deg1Upper = 2, 
                        deg2Lower = 1, deg2Upper = 2,
                        deg3Lower = 1, deg3Upper = 2,
                        deg4Lower = 1, deg4Upper = 2,
                        x1,x2,x3,x4){
	npoly <- round(runif(1,npolyLower - 0.5, npolyUpper + 0.5))
	coef <- runif(npoly, coefLower, coefUpper)
	deg1 <- round(runif(npoly, deg1Lower - 0.5, deg1Upper + 0.5))
	deg2 <- round(runif(npoly, deg2Lower - 0.5, deg2Upper + 0.5))
	deg3 <- round(runif(npoly, deg3Lower - 0.5, deg3Upper + 0.5))
	deg4 <- round(runif(npoly, deg4Lower - 0.5, deg4Upper + 0.5))
	return(list(npoly = npoly, coef = coef, deg1 = deg1, deg2 = deg2, deg3 = deg3,
	            deg4 = deg4))
}

#' polyJump1Quad
#' Simulate a polynomial trivariate relationship
#' 
#' @param x1 A vector
#' @param x2 A vector 
#' @param x3 A vector
#' @param x4 A vector
#' @param npoly A numeric indicating the number of polynomial terms to simulate
#' @param coef A vector indicating the coefficient for each polynomial term
#' @param deg1 A vector indicating the degree of the first polynomial term
#' @param deg2 A vector indicating the degree of the second polynomial term
#' @param deg4 A vector indicating the degree of the fourth polynomial term
#' @param jumpLoc3 Random quantile for x3 jump
#' @param coef A numeric coefficient
#' 
#' @return Quadvariate polynomial (see source code for details)
#' @export

polyJump1Quad <- function(x1, x2, x3, x4, npoly, coef, deg1, deg2, deg4, jumpLoc3){
	tmp <- rep(0, length(x1))
	for(i in 1:npoly){
		tmp <- tmp + coef[i]*x1^deg1[i]*x2^deg2[i]*x4^deg4[i]*as.numeric(x3 > jumpLoc3)
	}
	return(tmp)
}
#' polyQuadParm
#' 
#' Generate parameters for \code{polyQuad}. See source code.
#' @export
polyJump1QuadParm <- function(npolyLower = 1, npolyUpper = 1, 
                        coefLower = -0.1, coefUpper = 0.1, 
                        deg1Lower = 1, deg1Upper = 2, 
                        deg2Lower = 1, deg2Upper = 2,
                        deg4Lower = 1, deg4Upper = 2,
                        quantLower = 0.1, quantUpper = 0.9, 
                        x1,x2,x3,x4){

	npoly <- round(runif(1,npolyLower - 0.5, npolyUpper + 0.5))
	coef <- runif(npoly, coefLower, coefUpper)
	deg1 <- round(runif(npoly, deg1Lower - 0.5, deg1Upper + 0.5))
	deg2 <- round(runif(npoly, deg2Lower - 0.5, deg2Upper + 0.5))
	deg4 <- round(runif(npoly, deg4Lower - 0.5, deg4Upper + 0.5))
	quant3 <- runif(1, quantLower, quantUpper)
	jl3 <- quantile(x3, p = quant3)
	return(list(npoly = npoly, coef = coef, deg1 = deg1, deg2 = deg2, 
	            deg4 = deg4, jumpLoc3 = jl3))
}

#' polyJump2Quad
#' Simulate a polynomial trivariate relationship
#' 
#' @param x1 A vector
#' @param x2 A vector 
#' @param x3 A vector
#' @param x4 A vector
#' @param npoly A numeric indicating the number of polynomial terms to simulate
#' @param coef A vector indicating the coefficient for each polynomial term
#' @param deg1 A vector indicating the degree of the first polynomial term
#' @param deg4 A vector indicating the degree of the first polynomial term
#' @param jumpLoc2 Random quantile for x2 jump
#' @param jumpLoc3 Random quantile for x2 jump
#' @param coef A numeric coefficient
#' 
#' @return Quadvariate polynomial (see source code for details)
#' @export

polyJump2Quad <- function(x1, x2, x3, x4, npoly, coef, deg1, deg4, jumpLoc2, jumpLoc3){
	tmp <- rep(0, length(x1))
	for(i in 1:npoly){
		tmp <- tmp + coef[i]*x1^deg1[i]*x4^deg4[i]*as.numeric(x2 > jumpLoc2)*as.numeric(x3 > jumpLoc3)
	}
	return(tmp)
}
#' polyJump2QuadParm
#' 
#' Generate parameters for \code{polyQuad}. See source code.
#' @export
polyJump2QuadParm <- function(npolyLower = 1, npolyUpper = 1, 
                        coefLower = -0.1, coefUpper = 0.1, 
                        deg1Lower = 1, deg1Upper = 2, 
                        deg4Lower = 1, deg4Upper = 2, 
                        quantLower = 0.1, quantUpper = 0.9,
                        x1,x2,x3,x4){
	npoly <- round(runif(1,npolyLower - 0.5, npolyUpper + 0.5))
	coef <- runif(npoly, coefLower, coefUpper)
	deg1 <- round(runif(npoly, deg1Lower - 0.5, deg1Upper + 0.5))
	deg4 <- round(runif(npoly, deg4Lower - 0.5, deg4Upper + 0.5))
	quant2 <- runif(1, quantLower, quantUpper)
	quant3 <- runif(1, quantLower, quantUpper)
	jl2 <- quantile(x2, p = quant2)
	jl3 <- quantile(x3, p = quant3)	
	return(list(npoly = npoly, coef = coef, deg1 = deg1, deg4 = deg4, jumpLoc2 = jl2,
	            jumpLoc3 = jl3))
}

#' sinQuad
#' 
#' Simulate a sinusoidal trivariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param p The periodicity of the sin function
#' @param amp The amplitude of the sin function
#'
#' @return \code{amp*sin(p*x1*x2*x3)}
#' @export
sinQuad <- function(x1,x2,x3,x4,p,amp){
	w <- amp*sin(p*x1*x2*x3*x4)
	return(w)
}
#' sinQuadParm
#' 
#' Generate parameters for \code{sinQuad}. See source code. 
#' @export
sinQuadParm <- function(pLower = -1, pUpper = 1, ampLower = -1, ampUpper = 1,x1,x2,x3,x4){
	sinUniParm(pLower = pLower, pUpper = pUpper, ampLower = ampLower, ampUpper = ampUpper)
}

#' sinJump2Quad
#' 
#' Simulate a sinusoidal trivariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param p The periodicity of the sin function
#' @param amp The amplitude of the sin function
#' @param jumpLoc2 Random quantile for x2 jump. 
#' @param jumpLoc3 Random quantile for x2 jump. 
#'
#' @return \code{amp*sin(p*x1*x2*x3)}
#' @export
sinJump2Quad <- function(x1,x2,x3,x4,p,amp,jumpLoc2,jumpLoc3){
	w <- amp*sin(p*x1*x4*as.numeric(x2 > jumpLoc2)*as.numeric(x3>jumpLoc3))
	return(w)
}
#' sinJump2QuadParm
#' 
#' Generate parameters for \code{sinQuad}. See source code. 
#' @export
sinJump2QuadParm <- function(pLower = -1, pUpper = 1, ampLower = -1, ampUpper = 1,
                            quantLower = 0.1, quantUpper = 0.9,x1,x2,x3,x4){
	p <- runif(1, pLower, pUpper)
	amp <- runif(1, ampLower, ampUpper)
	quant2 <- runif(1, quantLower, quantUpper)
	quant3 <- runif(1, quantLower, quantUpper)
	jl2 <- quantile(x2, quant = quant2)
	jl3 <- quantile(x3, quant = quant3)	
	return(list(p=p,amp=amp,jumpLoc2=jl2,jumpLoc3=jl3))
}

#' sinJump1Quad
#' 
#' Simulate a sinusoidal trivariate relationship
#' 
#' @param x1 A vector 
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param p The periodicity of the sin function
#' @param amp The amplitude of the sin function
#' @param jumpLoc3 Numeric quantile for x3
#'
#' @return \code{amp*sin(p*x1*x2*x3)}
#' @export
sinJump1Quad <- function(x1,x2,x3,x4,p,amp,jumpLoc3){
	w <- amp*sin(p*x1*x2*x4*as.numeric(x3>jumpLoc3))
	return(w)
}
#' sinJump1QuadParm
#' 
#' Generate parameters for \code{sinQuad}. See source code. 
#' @export
sinJump1QuadParm <- function(pLower = -1, pUpper = 1, ampLower = -1, ampUpper = 1,
                            quantLower = 0.1, quantUpper = 0.9,x1,x2,x3,x4){
	p <- runif(1, pLower, pUpper)
	amp <- runif(1, ampLower, ampUpper)
	quant3 <- runif(1, quantLower, quantUpper)
	jl3 <- quantile(x3, quant = quant3)	
	return(list(p=p,amp=amp,jumpLoc3=jl3))
}

#' jumpQuad
#' 
#' Simulate a jump function trivariate relationship
#' 
#' @param x1 A vector
#' @param x2 A vector 
#' @param x3 A vector
#' @param x4 A vector
#' @param njump The number of jumps the function takes
#' @param jumpLoc1 A vector of the locations of those jumps for x1
#' @param jumpLoc2  A vector of the locations of those jumps for x2
#' @param jumpLoc3 A vector of the locations of those jumps for x3
#' @param jumpVal A vector of length njump indicating the value of the function at each jump
#'
#' @return The value of the jump function. 
#' @export
jumpQuad <- function(x1, x2, x3, x4, njump, jumpLoc1, jumpLoc2, jumpLoc3, jumpLoc4,
                     jumpVal){
	tmp <- rep(0, length(x1))
	for(i in 1:(length(jumpLoc1)-1)){
		ind <- (x1 <= jumpLoc1[i+1] & x1 >= jumpLoc1[i] & x2 <= jumpLoc2[i] & x2 >= jumpLoc2[i] &
		        x3 <= jumpLoc3[i] & x3 >= jumpLoc3[i+1] & x4 <= jumpLoc4[i] & x4 >= jumpLoc4[i+1])
		tmp[ind] <- jumpVal[i]
	}
	return(tmp)
}
#' jumpQuadParm
#' 
#' Generate parameters for \code{jumpQuad}. See source code and \code{?jumpUni}.
#' @export 
jumpQuadParm <- function(x1,x2,x3, x4, njumpLower = 1, njumpUpper = 5, 
                        jumpLower = -2, jumpUpper = 2,
                        jumpLoc1 = function(x,njump){ quantile(x, seq(0,1,length=njump+1)) },
                        jumpLoc2 = function(x,njump){ quantile(x, seq(0,1,length=njump+1)) },
                        jumpLoc3 = function(x,njump){ quantile(x, seq(0,1,length=njump+1)) },
                        jumpLoc4 = function(x,njump){ quantile(x, seq(0,1,length=njump+1)) },...
                        ){
	njump <- round(runif(1, njumpLower - 0.5, njumpUpper + 0.5))
	jumpL1 <- do.call("jumpLoc1", args = list(x = x1, njump = njump))
	jumpL2 <- do.call("jumpLoc2", args = list(x = x2, njump = njump))
	jumpL3 <- do.call("jumpLoc3", args = list(x = x3, njump = njump))
	jumpL4 <- do.call("jumpLoc4", args = list(x = x4, njump = njump))
	jumps <- runif(njump, jumpLower, jumpUpper)
	return(list(
	   njump = njump, jumpLoc1 = jumpL1, jumpLoc2 = jumpL2, jumpLoc3 = jumpL3, 
	   jumpLoc4 = jumpL4, jumpVal = jumps
	))
}

#' dNormAddQuad
#' 
#' Simulate a normal pdf relationship with two-way interaction
#' 
#' @param x1 A vector
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param coef Multiplier in front of dnorm
#' @param mult1 Multiplier 1 inside dnorm
#' @param mult2 Multiplier 2 inside dnorm
#' @param mult3 Multiplier 3 inside dnnorm
#' @param mult4 Multiplier 3 inside dnnorm
#' @param mu Mean of dnorm
#' @param sig SD of dnorm
#' @export

dNormAddQuad <- function(x1,x2, x3, x4, coef, mult1, mult2, mult3, mult4, mu, sig){
	return(coef*dnorm(mult1*x1+mult2*x2+mult3*x3+mult4*x4, mu, sig))
}

#' dNormAddBivParm
#' 
#' Generate parameters for dNormAddQuad
#' @export
dNormAddQuadParm <- function(x1,x2,x3,x4,coefLower=-5, coefUpper=5, 
                            multLower=-1, multUpper=1, 
                            muLower = -2, muUpper = 2, 
                            sigLower = 0.5, sigUpper = 2){
	return(list(
       coef = runif(1, coefLower, coefUpper),
       mult1 = runif(1, multLower, multUpper),
       mult2 = runif(1, multLower, multUpper),
       mult3 = runif(1, multLower, multUpper),
       mult4 = runif(1, multLower, multUpper),
       mu = runif(1, muLower, muUpper),
       sig = runif(1, sigLower, sigUpper)  
    ))
}


#' dNormMultQuad
#' 
#' Simulate a normal pdf relationship with two-way interaction
#' 
#' @param x1 A vector
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param coef Multiplier in front of dnorm
#' @param mult Multiplier 1 inside dnorm
#' @export

dNormMultQuad <- function(x1,x2, x3, x4, coef, mult, mu, sig){
	return(coef*dnorm(mult*(x1*x2*x3*x4), mu, sig))
}

#' dNormMultQuadParm
#' 
#' Generate parameters for dNormMultQuad
#' @export
dNormMultQuadParm <- function(x1,x2,x3,x4,coefLower=-5, coefUpper=5, 
                             multLower=-1, multUpper=1,
                             muLower = -2, muUpper = 2, 
                             sigLower = 0.5, sigUpper = 2){
	return(list(
       coef = runif(1, coefLower, coefUpper),
       mult = runif(1, multLower, multUpper),
       mu = runif(1, muLower, muUpper),
       sig = runif(1, sigLower, sigUpper)    
    ))
}

#' dNormMultJump2Quad
#' 
#' Simulate a normal pdf relationship with two-way interaction
#' 
#' @param x1 A vector
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param coef Multiplier in front of dnorm
#' @param mult Multiplier 1 inside dnorm
#' @param jumpLoc2 Quantile for x2 jump
#' @param jumpLoc3 Quantile for x3 jump
#' @export

dNormMultJump2Quad <- function(x1,x2, x3, x4, coef, mult, mu, sig, jumpLoc2, jumpLoc3){

	return(coef*dnorm(mult*(x1*x4*as.numeric(x2 > jumpLoc2)*as.numeric(x3 > jumpLoc3)), mu, sig))
}

#' dNormMultJump2QuadParm
#' 
#' Generate parameters for dNormMultQuad
#' @export
dNormMultJump2QuadParm <- function(x1,x2,x3,x4,coefLower=-5, coefUpper=5, 
                             multLower=-1, multUpper=1,
                             muLower = -2, muUpper = 2, 
                             quantLower = 0.1, quantUpper = 0.9, 
                             sigLower = 0.5, sigUpper = 2){
	quant2 = runif(1, quantLower, quantUpper)
    quant3 = runif(1, quantLower, quantUpper)  
    
	jl2 <- quantile(x2, p = quant2)
	jl3 <- quantile(x3, p = quant3)
   	
	return(list(
       coef = runif(1, coefLower, coefUpper),
       mult = runif(1, multLower, multUpper),
       mu = runif(1, muLower, muUpper),
       sig = runif(1, sigLower, sigUpper),
    	jumpLoc2 = jl2, jumpLoc3 = jl3
    ))
}

#' dNormMultJump1Quad
#' 
#' Simulate a normal pdf relationship with two-way interaction
#' 
#' @param x1 A vector
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param coef Multiplier in front of dnorm
#' @param mult Multiplier 1 inside dnorm
#' @param jumpLoc3 Quantile for x3 jump
#' @export

dNormMultJump1Quad <- function(x1,x2, x3, x4, coef, mult, mu, sig, jumpLoc3){
	return(coef*dnorm(mult*(x1*x2*x4*as.numeric(x3 > jumpLoc3)), mu, sig))
}

#' dNormMultQuadParm
#' 
#' Generate parameters for dNormMultQuad
#' @export
dNormMultJump1QuadParm <- function(x1,x2,x3,x4,coefLower=-5, coefUpper=5, 
                             multLower=-1, multUpper=1,
                             muLower = -2, muUpper = 2, 
                             quantLower = 0.1, quantUpper = 0.9, 
                             sigLower = 0.5, sigUpper = 2){
    quant3 <- runif(1, quantLower, quantUpper)  
	jl3 <- quantile(x3, p = quant3)

	return(list(
       coef = runif(1, coefLower, coefUpper),
       mult = runif(1, multLower, multUpper),
       mu = runif(1, muLower, muUpper),
       sig = runif(1, sigLower, sigUpper),
       jumpLoc3 = jl3
    ))
}

#' pLogisAddQuad
#' 
#' @param x1 A vector
#' @param x2 A vector
#' @param x3 A vector
#' @param x4 A vector
#' @param coef Multiplier in front of plogis
#' @param mult1 Multiplier 1 inside plogis
#' @param mult2 Multiplier 2 inside plogis 
#' @param mult3 Multiplier 3 inside plogis
#' @param mult4 Multiplier 4 inside plogis
#' @param loc The location parameter for plogis
#' @param scale The scale parameter
#' @export
pLogisAddQuad <- function(x1,x2,x3,x4, coef, mult1, mult2, mult3, mult4, loc, scale){
	return(coef*plogis(mult1*x1 + mult2*x2 + mult3 * x3 + mult4*x4, loc, scale))
}

#' pLogisAddQuadParm
#' 
#' Generate parameters for \code{pLogisAddQuad}
#' @export

pLogisAddQuadParm <- function(x1,x2,x3,x4,coefLower=-4, coefUpper=4, 
                         multLower=-1, multUpper=1, 
                         locLower = -2, locUpper = 2, scaleLower = 0.25, scaleUpper = 2){
	return(list(
       coef = runif(1, coefLower, coefUpper),
       mult1 = runif(1, multLower, multUpper),
       mult2 = runif(1, multLower, multUpper),
       mult3 = runif(1, multLower, multUpper),
       mult4 = runif(1, multLower, multUpper),
       loc = runif(1, locLower, locUpper),
       scale = runif(1, scaleLower, scaleUpper)
    ))
}

