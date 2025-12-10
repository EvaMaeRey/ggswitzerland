#' @export
stamp_relief <- function(...){ 
  
  annotate(geom = "raster",
      x = relief$x,
      y = relief$y,
      alpha = relief$value_point6_0,
    ...)
  
}

# I think there is something to be said for both names
#' @export
stamp_mountains <- stamp_relief
