define_gradient <- function(colours, ..., low, mid, high, colors) {
  # convert colors to consistent naming convention
  if (!missing(colors)) colours <- colors 
  
  # handle situation where an already defined gradient is passed 
  if (!missing(colours) && inherits(colours, "ggtheme_gradient_spec")) 
    return(colours)
  
  # create scale_*_gradient* functions, handling specified arguments  
  if (!missing(colours)) {
    structure(
      list(colours = colours, ...), 
      colour = "scale_colour_gradientn",
      fill = "scale_fill_gradientn",
      class = c("ggtheme_gradientn_spec", "ggtheme_grad_spec", "list"))
  } else if (!any(missing(low), missing(mid), missing(high))) {
    structure(
      list(low = low, mid = mid, high = high, ...), 
      colour = "scale_colour_gradient2",
      fill = "scale_fill_gradient2",
      class = c("ggtheme_gradient2_spec", "ggtheme_grad_spec", "list"))
  } else if (!any(missing(low), missing(high))) {
    structure(
      list(low = low, high = high, ...), 
      colour = "scale_colour_gradient",
      fill = "scale_fill_gradient",
      class = c("ggtheme_gradient_spec", "ggtheme_grad_spec", "list"))
  } else {
    stop(
      "insufficient arguments provided to define_gradient. expected one of: \n",
      "  arbitrary gradient: 'colours' or 'colors'\n",
      "  divergent gradient: 'low', 'mid' and 'high'\n",
      "   two-tone gradient: 'low' and 'high'") 
  }
}

theme_ggplot2_scales_continuous <- function(...) {
  gradient_spec <- define_gradient(...)
  
  # extract target function names from gradient_spec
  colour_fname <- attr(gradient_spec, "colour")
  fill_fname   <- attr(gradient_spec, "fill")
  
  # restore original binding before re-modifying with themed version
  restore_ggplot2_binding(colour_fname)
  restore_ggplot2_binding(fill_fname)
  
  themed_colour_f <- getExportedValue("ggplot2", colour_fname)
  formals(themed_colour_f)[names(gradient_spec)] <- gradient_spec
  body(themed_colour_f) <- theme_gradients_fbody(gradient_spec, "colour")
  environment(themed_colour_f) <- getNamespace("ggplot2")
  
  themed_fill_f <- getExportedValue("ggplot2", fill_fname)
  formals(themed_fill_f)[names(gradient_spec)] <- gradient_spec
  body(themed_fill_f) <- theme_gradients_fbody(gradient_spec, "fill")
  environment(themed_fill_f) <- getNamespace("ggplot2")
  
  # modify function in the ggplot2 namespace
  modify_ggplot2_binding("scale_color_continuous", themed_colour_f)
  modify_ggplot2_binding("scale_colour_continuous", themed_colour_f)
  modify_ggplot2_binding("scale_fill_continuous", themed_fill_f)
}

theme_gradients_fbody <- function(gradient, aesthetic) {
  UseMethod("theme_gradients_fbody")
}

theme_gradients_fbody.ggtheme_gradientn_spec <- function(gradient, aesthetic) {
  bquote({
    colours <- if (!missing(colors)) colors else colours
    continuous_scale(.(aesthetic), 'ggtheme', 
        scales::gradient_n_pal(colours, values, space), guide = guide, ...)
  })
}

theme_gradients_fbody.ggtheme_gradient2_spec <- function(gradient, aesthetic) {
  bquote({
    continuous_scale(.(aesthetic), 'ggtheme', 
      scales::div_gradient_pal(low, mid, high, space), guide = guide, 
      rescaler = ggplot2:::mid_rescaler(mid = midpoint), ...)
  })
}

theme_gradients_fbody.ggtheme_gradient_spec <- function(gradient, aesthetic) {
  bquote({
    continuous_scale(.(aesthetic), 'ggtheme', 
      scales::seq_gradient_pal(low, high, space), guide = guide, ...)
  })
}
