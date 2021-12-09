#' function to load the data
#'
#' @return
#' @export
#'
#' @examples
load_data = function(){
  buttRflyfish::buttR_neuro %>%
    mutate(species = relevel(factor(species), ref = 'C.bar'))
}
