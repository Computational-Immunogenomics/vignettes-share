ggit <- function( png_file ){
  require(png)
  require(grid)
  require(ggplot2)
  img <- readPNG( png_file ) 
  g <- rasterGrob(img, interpolate=TRUE)
  qplot(seq(0,1,1), seq(0,1,1), geom="blank") + theme_void() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + geom_blank()
}
