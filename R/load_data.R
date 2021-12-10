#' Load data
#'
#' Function to load the Qpcr data (gene expression) of the butterfly fish
#' @return
#' @export
#'
#' @examples
load_data = function(){
  buttRflyfish::buttR_neuro %>%
    dplyr::mutate(species = relevel(factor(species), ref = 'C.bar'))
}
