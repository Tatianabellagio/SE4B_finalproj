#' run model
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
run_model = function(data){
  model = MCMC.qpcr::mcmc.qpcr(data=data, fixed="species", random =
                      "individual", controls=c("r18S"), m.fix=1.2, nitt=510000,thin=500,burnin=10000) #runs informed model
  model
}
