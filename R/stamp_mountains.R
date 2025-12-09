#' @export
stamp_relief <- function(...){ 
  
  annotate(geom = "raster",
      x = relief$x,
      y = relief$y,
      alpha = relief$value_point6_0,
    ...)
  
}

#' @export
stamp_mountains <- stamp_relief
