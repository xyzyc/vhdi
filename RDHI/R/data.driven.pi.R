# Generated from rdhi.Rmd: do not edit by hand

#' Approximate data driven prediction interval  
#' 
#' @param x data
#' @param alpha nominal error rate
#' @importFrom purrr map_df
#' @importFrom magrittr "%>%"
#' @export
data.driven.pi <- function(x, alpha){
  n = length(x)
  k = ceiling(n*(1-alpha) + 1.12*sqrt(n*alpha))
  ordered_data = sort(x)
  posi = 1:(n-k)
  res = posi %>% map_df(.f = function(low_posi){
    up_posi = low_posi + k
    interval_length = ordered_data[up_posi] - ordered_data[low_posi]
    return(data.frame(low = ordered_data[low_posi], 
                      up = ordered_data[up_posi], 
                      length = interval_length))
  })
  unlist(unname(res[which.min(res$length), c('low', 'up')]))
}
