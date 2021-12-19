#' Black-Scholes model for option pricing
#'
#' @param dS
#' @param dK
#' @param dr
#' @param iT
#' @param dSigma
#' @param type
#'
#' @return
#' @export
#'
#' @examples
BlackScholes <- function(dS, dK, dr, iT, dSigma, type){

  if(type=="C"){
    d1 <- (log(dS/dK) + (drf + dSigma^2/2)*iT) / (dSigma*sqrt(iT))
    d2 <- d1 - dSigma*sqrt(iT)

    value <- dS*pnorm(d1) - dK*exp(-drf*iT)*pnorm(d2)
    return(value)}

  if(type=="P"){
    d1 <- (log(dS/dK) + (drf + dSigma^2/2)*iT) / (dSigma*sqrt(iT))
    d2 <- d1 - dSigma*sqrt(iT)

    value <-  (dK*exp(-drf*iT)*pnorm(-d2) - dS*pnorm(-d1))
    return(value)}
}
