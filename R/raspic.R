#' @title
#' Reading an ASPIC program output file
#'
#' @description
#' This function reads ASPIC program output files (.fit or .bot), derived from the adjustment using ASPIC7g.exe program.
#'
#' @usage
#' raspic (filename)
#'
#' @param filename Name of the ASPIC output file (.fit or .bot) or its path.
#'
#' @return A list containing the following components.
#' \itemize{
#' \item{errorcode:}{ Logistic. It takes the value 0 if there was no error in the adjustment or 1 if an error occurs.}
#' \item{convmsg:}{ The attained type of convergence.}
#' \item{states:}{ Data frame containing the following columns:}\itemize{
#' \item{time: }{Years of the input time series.}
#' \item{Fest: }{Estimated fishing mortality for each year.}
#' \item{B0est: }{Estimated biomass at the beginning of each year.}
#' \item{Best: }{Estimated average biomass for each year.}
#' \item{Catch: }{Observed catches for each year.}
#' \item{Cest: }{Estimated catches for each year.}
#' \item{SPest: }{Estimated surplus production for each year.}
#' \item{FFmsy: }{Estimated relative fishing mortality for each year (F_t/F_{msy}).}
#' \item{BBmsy: }{Estimated relative biomass (by B_{msy}) for each year (B_t/B_{msy}).}
#' \item{CPUEobs: }{Observed catches per unit of effort for each year.}
#' \item{CPUEest: }{Estimated catches per unit of effort for each year.}
#' \item{residuals: }{Residuals of the adjustment.}
#' \item{weight: }{Statistical weight of the observations. }
#' \item{BKest: }{Estimated relative biomass (by K) for each year (B_t/K).}}
#' \item{timespent:}{ Numeric. Time requires by ASPIC7 to adjust the model.}
#' \item{operation:}{ Data frame containing details of the type of adjustment carried out in ASPIC7.}\itemize{
#' \item{ASPIC.operation: }{Model used in the adjustment. Possible values are the logistic Schaefer model or the generalized Pella-Tomlinson model.}
#' \item{Objective.function: }{Estimation method. Possible values are "Least squares", "Least absolute values" or "Maximum likelihood".}
#' \item{Estimated.contrast.index: }{Estimated contrast index in the input series.}}
#' \item{estimates:}{ Estimates derived from the adjustment:}\itemize{
#' \item{MSY: }{Maximum sustainable yield (MSY).}
#' \item{Fmsy: }{Fishing mortality that gives MSY.}
#' \item{Bmsy: }{Biomass at MSY.}
#' \item{K: }{Maximum allowed population size.}
#' \item{phi: }{Relation between the MSY biomass and K (B_{msy}/K).}
#' \item{shape: }{Relation between the biomass at t=0 and the MSY biomass (B_0/B_{msy}).}
#' \item{Bly.Bmsy: }{Relation between the biomass in the last year plus one and the MSY biomass.}
#' \item{Fly.Fmsy: }{Relation between the fishing mortality in the last year and the MSY fishing mortality.}
#' \item{q: }{The catchability coefficient.}}}
#'
#' @author
#' \itemize{
#' \item{Marta Cousido-Rocha}
#' \item{Anxo Paz Cuña}
#' \item{Santiago Cerviño López}
#' \item{María Grazia Pennino}
#' }
#'
#' @references 
#' Prager, M. (2016). User’s Guide for ASPIC Suite, version 7: A Stock–Production Model Incorporating Covariates and auxiliary programs. Portland, OR: Prager Consulting. Retrieved from http://www.mhprager.com/aspic7-guide.pdf.
#' 
#' @examples
#' # Uncomment the following lines.
#' #res<-raspic("aspic.fit")
#'
#' # We can access to the previosly mentioned information:
#'
#' # Adjustment info
#' #res$operation
#'
#' # Estimations of yield and B/K relation:
#' #SPest<-res$states$SPest
#' #BKest<-res$states$BKest
#'
#' # MSY, Fmsy and Bmsy
#' #res$estimates[1:3]
#'
#' # Production curve
#' #plot(res$states$BKest,SPest)
#'
#' @export


raspic<-function (filename) {
  out <- base::list()
  aspicres <- base::readLines(filename)
  ind <- base::grep("Number of years analyzed", aspicres)
  ind2 <- base::gregexpr("[0-9]+", aspicres[ind])
  nobs <- base::as.numeric(base::unlist(base::regmatches(aspicres[ind], ind2))[1])
  get.num <- function(str, aspicres) {
    ind <- base::grep(str, aspicres)
    ind2 <- base::gregexpr("[0-9.]*E[+-][0-9]*", aspicres[ind])
    base::as.numeric(base::unlist(base::regmatches(aspicres[ind], ind2)))[1]
  }
  errind <- base::grep("error code", aspicres)[1]
  out$errorcode <- base::regmatches(aspicres[errind], base::regexpr("[0-9]+$",
                                                                    aspicres[errind]))
  out$convmsg <- aspicres[errind + 2]
  norestartind <- base::grep("Number of restarts", aspicres)
  bkfrac <- get.num("^B1/K", aspicres)
  MSY <- get.num("^MSY", aspicres)
  Fmsy <- get.num("^Fmsy", aspicres)
  q <- get.num("^q", aspicres)
  Bmsy <- MSY/Fmsy
  K <- get.num("^K", aspicres)
  ind <- base::grep("ESTIMATED POPULATION TRAJECTORY", aspicres)
  states <- utils::read.table(filename, skip = ind + 6, sep = "",
                              nrows = nobs, strip.white = TRUE)
  ind2 <- base::grep("RESULTS FOR DATA SERIES", aspicres)
  states2 <- utils::read.table(filename, skip = ind2 + 6, sep = "",
                               nrows = nobs, strip.white = TRUE)
  base::colnames(states2) <- c("obs", "time", "CPUEobs", "CPUEest",
                               "Fest2", "Catch2", "Cest2", "residuals", "weight")
  states3<-base::cbind(states2$CPUEobs,states2$CPUEest,states2$residuals,states2$weight,states$V4/K)
  out$states<-base::cbind(states[,-1],states3)
  base::colnames(out$states) <- c("time", "Fest", "B0est",
                                  "Best", "Catch", "Cest", "SPest", "FFmsy", "BBmsy",
                                  "CPUEobs", "CPUEest","residuals", "weight","BKest")
  ind<-base::grep("Operation of ASPIC:", aspicres)
  a<-aspicres[ind]
  b<-base::strsplit(a, " ")[[1]]
  resu<-b[c(seq(5,11,by=1))]
  ASPIC.operation<-base::paste(resu[1],resu[2],resu[3],resu[4],resu[5],resu[6],resu[7], sep=" ")
  string <- base::strsplit(aspicres[base::length(aspicres)], " ")
  base::options(warn=-1)
  et <- base::as.numeric(string[[1]])
  et <- stats::na.omit(et)
  timespent <- et[1] * 60 * 60 + et[2] * 60 + et[3]
  out$timespent <- timespent
  ind<-base::grep("Objective function:", aspicres)
  a<-aspicres[ind]
  b<-base::strsplit(a, " ")[[1]]
  h<-base::which(b=="squares"| b=="absolute"|b=="likelihood")
  if (b[h]=="absolute") {h2<-c(h-1,h,h+1)} else {h2<-c(h-1,h)}
  resu<-b[h2]
  if (b[h]=="absolute") {objective.function<-base::paste(resu[1],resu[2],resu[3], sep=" ")} else {
    objective.function<-base::paste(resu[1],resu[2], sep=" ")}
  ind<-base::grep("Estimated contrast index", aspicres)
  a<-aspicres[ind]
  b<-base::strsplit(a, " ")[[1]]
  estimated.contrast.index<-stats::na.omit(base::as.numeric(b))
  base::options(warn=1)
  out$operation<-base::data.frame(ASPIC.operation,objective.function,estimated.contrast.index)
  phi<-Bmsy/K
  shape<-K/Bmsy
  Bly.Bmsy<-get.num("^B./Bmsy", aspicres)
  Fly.Fmsy<-get.num("^F./Fmsy", aspicres)
  out$estimates<-base::data.frame(MSY,Fmsy,Bmsy,K,phi,shape,Bly.Bmsy,Fly.Fmsy,q)
  return(out)
}

