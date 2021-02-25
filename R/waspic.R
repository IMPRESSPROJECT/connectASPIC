#' @title Writing an ASPIC program input file
#'
#' @description The function writes an ASPIC7 program input file (.a7inp) from the available information which must include at least relative biomass index and catch time series. Our function also allows to specify some adjustment options, like the initial value for the catchability coefficient, the fit mode (simple or bootstrap), the estimation method or the production curve shape.
#'
#' @usage
#' waspic (lst, filename = "aspic.a7inp")
#'
#' @param lst A list containg the following elements:  \itemize{
#'   \item {timeC:}{ Vector containing the years of the catch time series.}
#'   \item {obsC :} {Vector containing the catch series for the adjustment.}
#'   \item {obsI :} {Vector containing the observed relative biomass index.}
#'   \item {timeI :} {Vector containing the years of the biomass index.}
#'   \item {aspic :} {Optional list containing the following adjustment settings:} \itemize{
#'   \item {mode :} {Character string. The program mode, a character string of length 3. Possible values are "FIT" (fitting mode, by default) or "BOT" (bootstrap mode).}
#'   \item {verbosity :} {Integer. The amount of screen output during the run and the selected files for the future adjustment. It is recommended not to change it (for more, see Prager, 2016).}
#'   \item {nboot :} {Integer. The number of bootstrap trials, an integer smaller than 3000. In "FIT" mode, this value may be omitted. If present, it is ignored.}
#'   \item {ciperc :} {Integer. An optional integer used to set the width of confidence intervals in "BOT" mode. In "FIT" mode, this value may be omitted. If present, it is ignored.}
#'   \item {shape :} {Character string specifying model shape. Possible values are "LOGISTIC" (fits the logistic Schaefer model, default option) or "GENFIT" (fits the generalized Pella-Tomlinson model).}
#'   \item {conditioning :} {Character string specifying statistical conditioning in estimation algorithm. Possible values are "YLD" (condition on yield, recommended for most analyses, default option) or "EFT" (condition on fishing effort rate).}
#'   \item {objfn :} {Character string specifying the estimation method. Possible values are "SSE" (least squares, default option), "LAV" (least absolute values) or "MLE" (maximum likelihood).}
#'   \item {cv :} {Vector required only when the estimation option is MSE. It is the coefficient of variation (CV) of the data point (see Prager,2016). With other estimation options, the CV is ignored.}}
#'   \item {ini :} {Optional list containing initial values for the following parameters/quantities:} \itemize{
#'   \item {bk :} {numeric. Initial value for the relation between biomass at t=0 and K (by default bk=0.8).}
#'   \item {MSY :} {Numeric. Initial value for the maximum sustainable yield (MSY) (by default MSY=5.33).}
#'   \item {Fmsy :} {Numeric. Initial value for the fishing mortality at the MSY (by default Fmsy=0.378).}
#'   \item {q :} {Numeric. Initial value for capturability (by default q=0.5).}}}
#' @param filename A character string containing the ASPIC input file name (by default "aspic.a7inp").
#'
#' @return An input file for ASPIC7 program (.a7inp).
#'
#' @author
#' \itemize{
#' \item{Marta Cousido-Rocha}
#' \item{Anxo Paz Cuña}
#' \item{Santiago Cerviño López}
#' \item{Maria Grazia Pennino}
#' }
#'
#' @references 
#' Prager, M. (2016). User’s Guide for ASPIC Suite, version 7: A Stock–Production Model Incorporating Covariates and auxiliary programs. Portland, OR: Prager Consulting. Retrieved from http://www.mhprager.com/aspic7-guide.pdf.
#' @examples
#'
#' # Uncomment the following lines.
#' # years=seq(1980,2120,1)
#' # C<-c(
#' # 0.112, 0.827, 1.526, 2.198, 2.831, 3.419, 3.956, 4.439, 4.868,
#' # 5.243, 5.564, 5.836, 6.059, 6.238, 6.376, 6.475, 6.539, 6.570,
#' # 6.572, 6.548, 6.499, 6.428, 6.337, 6.229, 6.105, 5.967, 5.816,
#' # 5.654, 5.483, 5.303, 5.116, 4.924, 4.726, 4.525, 4.321, 4.010,
#' # 3.639, 3.335, 3.083, 2.875, 2.701, 2.556, 2.434, 2.333, 2.247,
#' # 2.176, 2.118, 2.069, 2.029, 1.996, 1.969, 1.946, 1.927, 1.909,
#' # 1.892, 1.874, 1.853, 1.828, 1.795, 1.753, 1.699, 1.630, 1.543,
#' # 1.433, 1.297, 1.130, 0.928, 0.686, 0.398, 0.058, 0.062, 0.237,
#' # 0.431, 0.641, 0.864, 1.095, 1.332, 1.570, 1.808, 2.042, 2.270,
#' # 2.492, 2.705, 2.909, 3.104, 3.288, 3.461, 3.625, 3.777, 3.920,
#' # 4.052, 4.175, 4.288, 4.393, 4.488, 4.575, 4.655, 4.726, 4.790,
#' # 4.847, 4.898, 4.942, 4.980, 5.011, 5.038, 4.921, 4.819, 4.729,
#' # 4.650, 4.580, 4.519, 4.465, 4.418, 4.376, 4.339, 4.305, 4.276,
#' # 4.250, 4.227, 4.206, 4.187, 4.171, 4.156, 4.142, 4.130, 4.120,
#' # 4.110, 4.102, 4.094, 4.087, 4.081, 4.075, 4.070, 4.066, 4.062,
#' # 4.058, 4.055, 4.052, 4.049, 4.047, 4.045)
#'
#' # BI<-c(
#' # 52.08, 57.61, 55.32, 52.18, 53.11, 52.77, 51.12, 46.12, 46.35,
#' # 46.12, 45.23, 42.05, 38.19, 36.33, 35.25, 33.08, 33.34, 31.53,
#' # 28.52, 27.00, 25.75, 24.16, 22.14, 21.11, 20.24, 18.89, 16.98,
#' # 16.04, 14.56, 14.10, 14.31, 12.52, 12.08, 10.84, 10.23,  9.16,
#' # 8.53,  8.56,  7.35,  7.65,  7.41,  7.00,  6.87,  6.27,  6.32,
#' # 6.86,  6.96,  7.42,  7.70,  7.28,  7.95,  8.09,  8.51,  9.01,
#' # 9.12, 10.07, 10.74, 11.60, 11.27, 12.90, 13.84, 14.57, 16.04,
#' # 17.28, 19.24, 19.40, 22.11, 22.97, 26.04, 29.22, 30.32, 32.36,
#' # 33.55, 37.63, 38.79, 39.55, 39.34, 41.86, 39.65, 42.66, 45.81,
#' # 43.47, 45.23, 42.33, 42.32, 41.39, 41.50, 39.59, 39.89, 38.83,
#' # 37.67, 38.38, 37.77, 38.90, 34.85, 36.09, 34.18, 35.77, 31.39,
#' # 32.88, 31.40, 30.90, 30.72, 28.99, 27.08, 27.76, 25.70, 26.18,
#' # 24.82, 26.05, 25.08, 25.43, 25.08, 25.16, 26.24, 22.79, 22.70,
#' # 24.29, 23.66, 23.45, 22.26, 23.01, 22.71, 23.04, 23.98, 22.77,
#' # 22.30, 23.24, 21.71, 23.45, 22.58, 24.47, 22.94, 23.57, 22.23,
#' # 22.47, 24.26, 23.24, 22.49, 22.65, 23.86)
#'
#'
#' #lst=list(timeC=years, obsC=C, obsI=BI, timeI=years,
#' #ini=list(bk=0.9, q=0.2, MSY=6, Fmsy=0.2),
#' #aspic=list(verbosity=106,ciperc=97,shape="GENFIT",
#' #objfn="MLE",cv=rep(0.1,length(years))))
#' #waspic(lst,"aspictest.a7inp")
#'
#' #lst2<-list(timeC=years, obsC=C, obsI=BI, timeI=years,
#' # ini=list(bk=0.7, q=0.4, MSY=5, Fmsy=0.15),
#' #aspic=list(mode="BOT",nboot=200))
#' #waspic(lst2,"aspictest2.a7inp")
#'
#' @export

waspic<- function (lst, filename = "aspic.a7inp") {
  x <- lst
  x$timeC <- base::floor(x$timeC)
  x$timeI <- base::floor(x$timeI)
  timeobs <- base::sort(base::unique(c(x$timeC, x$timeI)))
  nobs <- base::length(timeobs)
  if (!"mode" %in% base::names(x$aspic)) {
    x$aspic$mode <- "FIT"
  }
  if (!"verbosity" %in% base::names(x$aspic)) {
    x$aspic$verbosity <- 102
  }
  if (!"nboot" %in% base::names(x$aspic)) {
    x$aspic$nboot <- 100
  }
  if (!"ciperc" %in% base::names(x$aspic)) {
    x$aspic$ciperc <- 95
  }
  if (!"shape" %in% base::names(x$aspic)) {
    x$aspic$shape <- "LOGISTIC"
  }
  if (!"conditioning" %in% base::names(x$aspic)) {
    x$aspic$conditioning <- "YLD"
  }
  if (!"objfn" %in% base::names(x$aspic)) {
    x$aspic$objfn <- "SSE"
  }
  if (!"index" %in% base::names(x$aspic)) {
    x$aspic$index <- "CC"
  }
  if ( x$aspic$objfn == "MLE" | x$aspic$objfn == "MAP") {
    dat <- base::cbind(timeobs, base::rep(-1, nobs), base::rep(-1, nobs), base::rep(-1, nobs))
  } else {
    dat <- base::cbind(timeobs, base::rep(-1, nobs), base::rep(-1, nobs))}
  inds <- base::match(x$timeI, timeobs)
  dat[inds, 2] <- x$obsI
  inds <- base::match(x$timeC, timeobs)
  dat[inds, 3] <- x$obsC
  if ( x$aspic$objfn == "MLE" | x$aspic$objfn == "MAP") {
    dat[inds, 4] <- x$aspic$cv
  }
  base::cat("ASPIC-V7\n", file = filename)
  base::cat(base::paste("# File generated by SPiCT function write.aspic at",
                        Sys.time(), "\n"), file = filename, append = TRUE)
  base::cat("\"Unknown stock\"\n", file = filename, append = TRUE)
  base::cat("# Program mode (FIT/BOT), verbosity, N bootstraps, [opt] user percentile:\n",
            file = filename, append = TRUE)
  base::cat(base::paste0(x$aspic$mode, "  ", x$aspic$verbosity, "  ",
                         x$aspic$nboot, "  ", x$aspic$ciperc, "\n"), file = filename,
            append = TRUE)
  base::cat("# Model shape, conditioning (YLD/EFT), obj. fn. (SSE/LAV/MLE/MAP):\n",
            file = filename, append = TRUE)
  base::cat(base::paste0(x$aspic$shape, "  ", x$aspic$conditioning, "  ",
                         x$aspic$objfn, "\n"),file = filename, append = TRUE)
  base::cat("# N years, N series:\n", file = filename, append = TRUE)
  base::cat(base::paste0(nobs, "  ", 1, "\n"), file = filename,
            append = TRUE)
  base::cat("# Monte Carlo mode (0/1/2), N trials:\n", file = filename,
            append = TRUE)
  base::cat("0  30000\n", file = filename, append = TRUE)
  base::cat("# Convergence criteria (3 values):\n", file = filename,
            append = TRUE)
  base::cat("1.00E-08  3.00E-08  1.00E-04\n", file = filename, append = TRUE)
  base::cat("# Maximum F, N restarts, [gen. model] N steps/yr:\n",
            file = filename, append = TRUE)
  base::cat("8.00E+00  6  24\n", file = filename, append = TRUE)
  base::cat("# Random seed (large integer):\n", file = filename,
            append = TRUE)
  base::cat("1234\n", file = filename, append = TRUE)
  base::cat("# Initial guesses and bounds follow:\n", file = filename,
            append = TRUE)
  if (!"bk" %in% base::names(x$ini)) {
    x$ini$bk <- 0.8
  }
  if (!"MSY" %in% base::names(x$ini)) {
    x$ini$MSY <- 5.33
  }
  if (!"Fmsy" %in% base::names(x$ini)) {
    x$ini$Fmsy <- 0.378
  }
  if (!"q" %in% base::names(x$ini)) {
    x$ini$q <- 0.5
  }
  bk<-x$ini$bk
  base::cat(base::sprintf("B1K   %3.2E  1  %3.2E  %3.2E  penalty  %3.2E\n",
                          bk, 0.01 * bk,
                          100 * bk, 0), file = filename, append = TRUE)
  MSY <- x$ini$MSY
  base::cat(sprintf("MSY   %3.2E  1  %3.2E  %3.2E\n", MSY, 0.03 *
                      MSY, 5000 * MSY), file = filename, append = TRUE)
  Fmsy <- x$ini$Fmsy
  base::cat(base::sprintf("Fmsy  %3.2E  1  %3.2E  %3.2E\n", Fmsy, 0.01 *
                            Fmsy, 100 * Fmsy), file = filename, append = TRUE)
  q<-x$ini$q
  base::cat(base::sprintf("q     %3.2E  1  %3.2E  %3.2E  %3.2E\n",
                          q, 1, 0.001 * q,
                          100 * q), file = filename, append = TRUE)
  if (x$aspic$shape=="GENFIT") {
    base::cat("pos 40 1 10 90 8.0\n",file=filename,append=TRUE)
  }
  base::cat("DATA\n", file = filename, append = TRUE)
  base::cat("\"Combined-Fleet Index, Total Landings\"\n", file = filename,
            append = TRUE)
  base::cat(base::paste0(x$aspic$index,"\n"), file = filename, append = TRUE)
  if ( x$aspic$objfn == "MLE" | x$aspic$objfn == "MAP") {
  for (i in 1:nobs) {
    base::cat(base::sprintf("  %4i    % 6.4E    % 6.4E    %6.4E\n", timeobs[i],
                            dat[i, 2], dat[i, 3], dat[i, 4]), file = filename, append = TRUE)
  }} else {
    for (i in 1:nobs) {
      base::cat(base::sprintf("  %4i    % 6.4E    % 6.4E\n", timeobs[i],
                              dat[i, 2], dat[i, 3]), file = filename, append = TRUE)
    }}
}
