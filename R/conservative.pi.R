
#' Conservative prediction interval
#'
#' @param x data
#' @param alpha nominal error rate
#' @importFrom purrr map_df
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' example_data <- rnorm(500, 0, 5)
#' conservative.pi(example_data, alpha = 0.01)
#' }
#'

conservative.pi <- function(x, alpha){
  stopifnot(is.numeric(x))
  n = length(x)
  k = ceiling(n*(1-alpha) + 1.12*sqrt(n*alpha))
  ordered_data = sort(x)
  if(k >= n){
    warning("No finite interval at given coverage rate, returning min(x) and Inf.")
    res  = data.frame(low = ordered_data[1],
                      up = Inf,
                      length = Inf)
  }else{
    posi = 1:(n-k)
    res = posi %>% map_df(.f = function(low_posi){
      up_posi = low_posi + k
      interval_length = ordered_data[up_posi] - ordered_data[low_posi]
      return(data.frame(low = ordered_data[low_posi],
                        up = ordered_data[up_posi],
                        length = interval_length))
    })
  }
  unlist(unname(res[which.min(res$length), c('low', 'up')]))
}
