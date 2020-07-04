#############################################################################################
#' @title loadpexm
#' @importFrom runjags findjags
#' @description Function to load the Just Another Gibbs Sampling (JAGS) module \code{pexm} into the \code{R} environment.
#' @param f.path This is an optional argument. If having difficulties to find the main file "\code{pexm.so}" (unix) or "\code{pexm.dll}" (windows), the user
#' may choose to manually include the corresponding quoted path here.
#' @return Indication of whether the JAGS module \pkg{pexm} was correctly loaded in the \code{R} environment.
#' @details The main purpose of the present function is to load the target JAGS module \pkg{pexm} into the R environment.
#' The target module allows the application of the Piecewise Exponential (PE) distribution in any Bayesian model implemented through JAGS.
#' In order to apply this loading routine, it is necessary to have \href{http://mcmc-jags.sourceforge.net}{JAGS} installed in the computer.
#' The supporting package \pkg{rjags} is also required and it should have been automatically installed together with \pkg{pexm}.
#' Important remark for Windows users: the JAGS version 4.3.0 is the one considered in the present implementation of \pkg{pexm}.
#' If working with another version, one must download the source code of \pkg{pexm} and then modify the file "Makevars.win" (directory \code{src});
#' simply adapt the path in \code{JAGS_ROOT} with the correct version number.
#' In order to build the present package, the source code of \pkg{runjags} (\emph{Denwood; 2016}) was used as an example.
#' @references
#' DOI:10.18637/jss.v071.i09 (\href{https://www.jstatsoft.org/article/view/v071i09}{Denwood; 2016})
#'
#' @examples
#' # Load the new module with the command below:
#' loadpexm()
#'
#' @export
#' @importFrom rjags load.module
#'
loadpexm <- function(fail=TRUE, silent=FALSE){

  intfun <- function(){
    # Make sure this is not the module-less version from sourceforge:
    if(pexmprivate$modulelocation=='')
      return('The internal pexm module is not installed - please reinstall the full version of the package from CRAN, or alternatively you can download a standalone version of the JAGS module from the sourceforge page at http://sourceforge.net/projects/pexm/')

    # Also makes sure JAGS is installed:
    if(!loadandcheckrjags(FALSE))
      return("The rjags package is required to use the internal pexm module - alternatively you can download a standalone version of the JAGS module from the sourceforge page at http://sourceforge.net/projects/pexm/")

    # Check the JAGS major version is as expected:
    if(packageVersion('rjags')$major < pexmprivate$minjagsmajor)
      return(paste('JAGS version ', pexmprivate$minjagsmajor, '.x.x to ', pexmprivate$maxjagsmajor, '.x.x is required for this version of the pexm module - please update JAGS and rjags',sep=''))
    if(packageVersion('rjags')$major > pexmprivate$maxjagsmajor)
      return(paste('This version of the pexm module was designed for JAGS version ', pexmprivate$minjagsmajor, '.x.x to ', pexmprivate$maxjagsmajor, '.x.x - please update the pexm package', sep=''))

    success <- try(rjags::load.module('pexm', pexmprivate$modulelocation))

    if(inherits(success, 'try-error')){

      rvers <- paste('version ', R.version$major, sep='')
      if(grepl('mac.binary', .Platform$pkgType, fixed=TRUE)){
        # A specific error may be because of SL vs Mavericks version on OS X for JAGS version 3.4:
        mavericks <- grepl('mavericks', .Platform$pkgType)
        if(mavericks)
          rvers <- paste(rvers, ' - Mavericks', sep='')
        else
          rvers <- paste(rvers, ' - Snow Leopard', sep='')
      }

      return(paste("The internal pexm module could not be loaded - perhaps the package was not built using the same versions of R [", rvers, "] and JAGS [version ", testjags(silent=TRUE)$JAGS.version, "] as available on this system?", sep=''))

    }
    return(TRUE)
  }

  retval <- intfun()

  if(retval==TRUE){
    invisible(TRUE)
  }else{
    if(fail)
      stop(retval)

    if(!silent)
      swcat(retval,'\n',sep='')

    invisible(FALSE)
  }
}

unload.pexmmodule <- function(){

  if(!loadandcheckrjags(FALSE))
    stop("The rjags package is required to use the internal pexm module - alternatively you can download a standalone version of the JAGS module from the sourceforge page at http://sourceforge.net/projects/pexm/")

  suppressWarnings(success <- try(rjags::unload.module('pexm')))

  if(inherits(success, 'try-error')){
    warning("There was a problem unloading the internal pexm module - if you installed this package from CRAN, please file a bug report to the package author")
    invisible(FALSE)
  }else{
    invisible(TRUE)
  }
}

loadpexm <- loadpexm
unload.pexmmodule <- unload.pexmmodule


# These utility functions are NOT exported, and are primarily used for unit testing.
# Availability and/or operation of these functions may change without warning.

dynloadmodule <- function(){

  # Sets environmental variables we need for Windows:
  if(.Platform$OS.type=='windows'){
    if(!loadandcheckrjags(FALSE))
      stop('The rjags package is required to load the internal dynlib')
  }

  if(pexmprivate$modulelocation==''){
    warning('The pexm module has not been installed with this version of the package - try again using the CRAN binary')
    invisible(FALSE)
  }

  # Check the JAGS major version is as expected:
  if(testjags(silent=TRUE)$JAGS.major < pexmprivate$minjagsmajor)
    return(paste('JAGS version ', pexmprivate$minjagsmajor, '.x.x to ', pexmprivate$maxjagsmajor, '.x.x is required for this version of the pexm module - please update JAGS and rjags',sep=''))
  if(testjags(silent=TRUE)$JAGS.major > pexmprivate$maxjagsmajor)
    return(paste('This version of the pexm module was designed for JAGS version ', pexmprivate$minjagsmajor, '.x.x to ', pexmprivate$maxjagsmajor, '.x.x - please update the pexm package', sep=''))

  # Find and load the pexm shared library (only required for these tests and using the rjags call 'load.modue()' so NOT loaded at runtime):
  slibpath <- file.path(pexmprivate$modulelocation, paste('pexm', .Platform$dynlib.ext, sep=''))
  swcat("Loading shared library from:  ", slibpath, "\n", sep="")
  success <- try(dyn.load(slibpath))

  if(inherits(success, 'try-error')){

    rvers <- paste('version ', R.version$major, sep='')
    if(grepl('mac.binary', .Platform$pkgType, fixed=TRUE)){
      # A specific error may be because of SL vs Mavericks version on OS X for JAGS version 3.4:
      mavericks <- grepl('mavericks', .Platform$pkgType)
      if(mavericks)
        rvers <- paste(rvers, ' - Mavericks', sep='')
      else
        rvers <- paste(rvers, ' - Snow Leopard', sep='')
    }

    return(paste("The pexm dynlib could not be loaded - perhaps the package was not built using the same versions of R [", rvers, "] and JAGS [version ", testjags(silent=TRUE)$JAGS.version, "] as available on this system?", sep=''))

  }

  pexmprivate$dynlibname <- success
  invisible(TRUE)

}

dynunloadmodule <- function(){

  if(is.null(pexmprivate$dynlibname)){
    warning('Unable to load the dynlib as it has not been loaded')
    invisible(FALSE)
  }
  # Find and unload the pexm shared library (only required for these tests and using the rjags call 'load.modue()' so NOT loaded at runtime):
  slibpath <- system.file("libs", paste(.Platform$r_arch, if(.Platform$r_arch!="") "/" else "", if(.Platform$OS.type=="unix") "pexm.so" else "pexm.dll", sep=""), package="pexm")
  swcat("Unloading shared library from:  ", slibpath, "\n", sep="")
  success <- try(dyn.unload(slibpath))
  if(inherits(success, 'try-error'))
    stop("The internal dynlib could not be unloaded - if you installed this package from CRAN, please file a bug report to the package author")

  pexmprivate$dynlibname <- NULL
  invisible(TRUE)
}




pexmprivate <- new.env()
# Use 'expression' for functions to avoid having to evaluate before the package is fully loaded:
assign("defaultoptions",list(jagspath=expression(findjags()), method=expression(if('rjags' %in% .packages(TRUE)){'rjags'}else{if(Sys.info()['user']=='nobody') 'simple' else 'interruptible'}), tempdir=TRUE, plot.layout=c(2,2), new.windows=TRUE, modules="", factories="", bg.alert='beep', linenumbers=TRUE, inits.warning=TRUE, rng.warning=TRUE, summary.warning=TRUE, blockcombine.warning=TRUE, blockignore.warning=TRUE, tempdir.warning=FALSE, nodata.warning=TRUE, adapt.incomplete='warning', repeatable.methods=FALSE, silent.jags=FALSE, silent.runjags=FALSE, predraw.plots=FALSE, force.summary=FALSE, mode.continuous=expression('modeest' %in% .packages(TRUE)), timeout.import=30, partial.import=FALSE, keep.crashed.files=TRUE, full.cleanup=FALSE, debug=FALSE), envir=pexmprivate)
assign("options", pexmprivate$defaultoptions, envir=pexmprivate)
assign("rjagsmethod",c('rjags','rjparallel'),envir=pexmprivate)
assign("bgmethod",c('background','bgparallel'),envir=pexmprivate)
assign("parallelmethod",c('parallel','bgparallel','snow','rjparallel','xgrid'),envir=pexmprivate)
assign("runjagsversion", "notset", envir=pexmprivate)
assign("simfolders", character(0), envir=pexmprivate)
assign("failedsimfolders", character(0), envir=pexmprivate)
assign("defaultsummarypars", list(vars=NA, mutate=NULL, psrf.target = 1.05, normalise.mcmc = TRUE, modeest.opts=list(), confidence=c(0.95), autocorr.lags=c(10), custom=NULL, silent.jags=expression(runjags.getOption('silent.jags')), plots=FALSE, plot.type=c('trace','ecdf','histogram','autocorr','key','crosscorr'), col=NA, summary.iters=10000, trace.iters=1000, separate.chains=FALSE, trace.options=list(), density.options=list(), histogram.options=list(), ecdfplot.options=list(), acplot.options=list()), envir=pexmprivate)
assign("minjagsmajor", 3, envir=pexmprivate)
assign("maxjagsmajor", 4, envir=pexmprivate)
assign("warned_version_mismatch", FALSE, envir=pexmprivate)


.onLoad <- function(libname, pkgname){
  # Get and save the library location, getting rid of any trailing / caused by r_arch being empty:
  modulelocation <- gsub('/$','', file.path(libname, pkgname, 'libs', if(.Platform$r_arch!="") .Platform$r_arch else ""))
  if(!file.exists(file.path(modulelocation, paste('pexm', .Platform$dynlib.ext, sep=''))))
    modulelocation <- ''
  pexmprivate$modulelocation <- modulelocation
}

.onAttach <- function(libname, pkgname){
  # Get and save the library location, getting rid of any trailing / caused by r_arch being empty:
  modulelocation <- gsub('/$','', file.path(libname, pkgname, 'libs', if(.Platform$r_arch!="") .Platform$r_arch else ""))
  if(!file.exists(file.path(modulelocation, paste('pexm', .Platform$dynlib.ext, sep=''))))
    modulelocation <- ''
  pexmprivate$modulelocation <- modulelocation
}


loadandcheckrjags <- function(stop=TRUE, silent=FALSE){

  fail <- FALSE

  if(!any(.packages(TRUE)=="rjags")){
    if(!silent)
      swcat("\nThe rjags package is not installed - either install the package from CRAN or from https://sourceforge.net/projects/mcmc-jags/files/rjags/\n")
    fail <- TRUE
  }

  if(!fail && !requireNamespace("rjags")){
    if(!silent)
      swcat("\nThe rjags package is installed, but could not be loaded - run the testjags() function for more detailed information\n", sep="")
    fail <- TRUE
  }
  if(!fail && packageVersion('rjags') < 3.9){
    if(!silent)
      swcat("\nPlease update the rjags package to version 3-9 or later\n", call.=FALSE)
    fail <- TRUE
  }

  if(fail && stop)
    stop("Loading the rjags package failed (diagnostics are given above this error message)", call.=FALSE)

  return(!fail)
}
