#' @title Fitting ASPIC calling from R the ASPIC7 program
#'
#' @description
#' The function opens the input file (.a7inp) in ASPIC7 program, once the input is read  ASPIC7g.exe run
#' ASPIC model based on such input information. Note that the adjustment is completed only if the time
#' fixed for such adjustment is greater than the required time for ASPIC7g.exe.
#' @usage
#' faspic(filename,t)
#'
#' @param filename Name of the ASPIC input file (.a7inp) or its path.
#' @param t integer. The time after which the execution of R is suspended either ASPIC program has finished the adjustment or not (see description). If it is less than the time required by the program to adjust ASPIC,  executable is closed before to obtain any results.
#'
#' @details ASPIC programs are available on http://www.mhprager.com/aspic.html. Only work in windows.
#'
#' @return The output files of  ASPIC7 program containing  the adjustment results.
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
#' 
#' @examples
#' # Uncomment the following lines.
#' #faspic("aspic.a7inp",t=120)
#'
#' @export

faspic<-function (filename,t) {
  base::shell("TASKKILL /IM aspic7g.exe")
  exe.file<-(filename)
  before.win.tasklist<-base::system2('tasklist',stdout=TRUE)
  before.pids<-base::substr(before.win.tasklist[-(1:3)],27,35)
  base::shell.exec(exe.file)
  after.win.tasklist<-base::system2('tasklist',stdout=TRUE)
  after.tasks<-base::substr(after.win.tasklist[-(1:3)],1,25)
  after.pids<-base::substr(after.win.tasklist[-(1:3)],27,35)
  initiated.pid.positions<-base::which(!(after.pids %in% before.pids))
  after.tasks<-base::gsub(" ","",after.tasks)
  correct.pid.position<-base::intersect(base::which(after.tasks %in% "aspic7g.exe" ),initiated.pid.positions)
  correct.pid<-base::gsub(" ","",after.pids[correct.pid.position])
  taskkill.cmd<-base::paste("taskkill","/PID",correct.pid)
  base::Sys.sleep(t)
  base::system(taskkill.cmd)
}
