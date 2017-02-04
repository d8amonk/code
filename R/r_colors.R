r_colors <- function(){
  # returns a named list (python dict) of r color name keys and hex values
  r_colors <- rgb(t(col2rgb(colors())/255))
  names(r_colors) <- colors()
  return(r_colors)
}