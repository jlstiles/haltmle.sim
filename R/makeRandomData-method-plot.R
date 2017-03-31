#' plot.makeRandomData
#' 
#' Plot the results of makeRandomData
#' @export

plot.makeRandomData <- function(object, g0 = TRUE, Q0 = TRUE, ask = FALSE){
	if(ask){
		par(ask = TRUE)
	}else{
		layout(matrix(1:(4*ncol(object$W)), nrow=4, byrow = TRUE))
	}

	if(!ask) par(oma = c(0, 2, 0, 0))
	# plot a histogram of each variable
	for(m in 1:ncol(object$W)){
		hist(object$W[,m], bty="n", xlab = bquote(W[.(m)]),
		     main = bquote(paste(W[.(m)] %~% .(object$distW[[m]]$fn), "(", .(paste(names(object$distW[[m]]$parm), round(unlist(object$distW[[m]]$parm),2), sep= "=", collapse=",")),")")))
	}

	# plot the propensity score for each variable
	mainTermsG0 <- length(object$fnG0$uni)
	for(m in 1:ncol(object$W)){
		# see if there are higher order interactions with W[,m]
		intM <- m %in% unlist(lapply(object$fnG0$biv, function(x){ x$whichColsW }))

		if(m <= length(object$fnG0$uni)){
			main <- bquote(paste(logit(g[0]) %~% .(object$fnG0$uni[[m]]$fn), "(", 
		                         .(paste(names(object$fnG0$uni[[m]]$parm), 
		                                 lapply(object$fnG0$uni[[m]]$parm, function(x){ paste(round(x,2),collapse=",") }),
		                                 sep= "=", collapse=",")),")"
		     					, .(ifelse(intM,"+ int.",""))))
		}else{
			if(intM){
				main <- bquote(paste(logit(g[0]) %~% "int."))
			}else{
				main <- bquote(paste(logit(g[0]) %~% "1"))
			}
		}
		

		plot(object$A ~ object$W[,m], xlab = bquote(W[.(m)]), ylab = "A", bty="n", col = "gray80",
		     main = main)
		points(object$g0 ~ object$W[,m], col = 1)
	}

	for(m in 1:ncol(object$W)){
		# see if there are higher order interactions with W[,m]
		intM <- (m+1) %in% unlist(lapply(object$fnG0$biv, function(x){ x$whichColsW }))

		if((m+1) <= length(object$fnQ0$uni)){
			main <- bquote(paste(Q[0] %~% .(object$fnQ0$uni[[m+1]]$fn), "(", 
		                         .(paste(names(object$fnQ0$uni[[m+1]]$parm), 
		                                 lapply(object$fnQ0$uni[[m+1]]$parm, function(x){ paste(round(x,2),collapse=",") }),
		                                 sep= "=", collapse=",")),")"
		     					, .(ifelse(intM," + int.",""))))
		}else{
			if(intM){
				main <- bquote(paste(Q[0] %~% "int."))
			}else{
				main <- bquote(paste(Q[0] %~% "1"))
			}
		}
		
		plot(object$Y ~ object$W[,m], xlab = bquote(W[.(m)]), ylab = "Y", bty="n", col = "gray80", main = main)
		points(object$Q0 ~ object$W[,m], col = 1)		
	}


	# plot the outcome regression for each variable
	mainTermsQ0 <- length(object$fnG0$uni)
	
	# plot for A
	plot(object$Y ~ object$A, xlab = expression(A), ylab = "Y", bty="n", col = "gray80")
	points(object$Q0 ~ object$A, col = 1)
}