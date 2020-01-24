theme_ggplot2_scale_discrete <- function(fname, aesthetic, colors, ...) {
  # restore to original function before using it to build new themed version
  restore_ggplot2_binding(fname)
  
  f <- function(..., aesthetics = aesthetic) {} 
  formals(f)$aesthetics = aesthetic
  body(f) <- bquote({
    discrete_scale(
      aesthetics, 
      'ggtheme', 
      ggtheme::make_pal_expand_lighter(.(colors)), 
      na.value = na.value, 
      ...)
  })
  
  # use ggplot2 formal definition for na.value
  existing_formals <- formals(getExportedValue("ggplot2", fname))
  formals(f)$na.value <- existing_formals$na.value
  environment(f) <- getNamespace("ggplot2")
  
  # modify function in the ggplot2 namespace
  modify_ggplot2_binding(fname, f)
}

theme_ggplot2_scales_discrete <- function(colors) {
  theme_ggplot2_scale_discrete("scale_color_discrete", "colour", colors)
  theme_ggplot2_scale_discrete("scale_colour_discrete", "colour", colors)
  theme_ggplot2_scale_discrete("scale_fill_discrete", "fill", colors)
}
