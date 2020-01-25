.ggtheme <- list()



#' Title
#'
#' @param ... 
#' @param set 
#' @param theme 
#'
#' @return
#' @export
#'
set_theme_value <- function(..., set = NULL, theme, quoted = FALSE) {
  if (!quoted) 
    set <- substitute(set)
  
  if (missing(theme)) {
    theme <- getNamespace(packageName())[[".ggtheme"]]
    on.exit({
      unlockBinding(".ggtheme", getNamespace(packageName()))
      assign(".ggtheme", theme, envir = getNamespace(packageName()))
      lockBinding(".ggtheme", getNamespace(packageName()))
    })
  }
    
  theme_path <- rev(as.character(list(...)))
  
  if (!length(theme_path)) {
    if (is.null(names(theme[1])) || names(theme[1]) == "") theme[[1]] <- set
    else theme <- append(list(set), theme)
    return(theme)
  } 
  
  if (!theme_path[[1]] %in% names(theme)) {
    theme[[theme_path[[1]]]] <- list()
  }
  
  theme[[theme_path[[1]]]] <- do.call(set_theme_value, append(
    as.list(rev(theme_path[-1])), 
    list(set = set, theme = theme[[theme_path[[1]]]], quoted = TRUE)))
  
  theme
}


set_theme_values <- function(...) {
  dots <- list(...)
  if (length(dots))
    if (!getOption("ggtheme.experimental.geom_args", FALSE)) {
      experimental_message(paste0(
        wrap(indent = 2, 
          "Ellipsis arguments don't correspond to ggplot2::theme() parameter ",
          "names or aesthetics. If you intended to use the experimental ",
          "feature to set ggplot2::geom_* or ggplot2::stat_* argument ", 
          "defaults, please enable this explicitly by setting:"),
        "\n\n    > options(ggtheme.experimental.geom_args = TRUE)\n"))
    } else{
      theme_ggplot2_layer_args()
      for (i in seq_along(dots)) {
        dotname <- names(dots[i])
        dotname <- split_dotted_fname_argname_pair(dotname)
        dot <- dots[[i]]
        do.call(set_theme_value, append(as.list(dotname), list(set = dot)))
      }
    }
}



#' Title
#'
#' @param ... 
#' @param default 
#' @param theme 
#'
#' @return
#' @export
#' 
get_theme_value <- function(..., default = NULL, theme) {
  if (missing(theme)) theme = getNamespace(packageName())[[".ggtheme"]]
  theme_path <- rev(as.character(list(...)))
  
  if (!length(theme_path)) {
    if (is.null(names(theme[1])) || names(theme[1]) == "") return(theme[[1]])
    else return(default)
  } else if (!theme_path[[1]] %in% names(theme)) {
    if (is.null(names(theme[1])) || names(theme[1]) == "") return(theme[[1]])
    else return(default)
  } else{
    do.call(get_theme_value, append(
      as.list(rev(theme_path[-1])), 
      list(default = default, theme = theme[[theme_path[[1]]]])))
  }
}



#' @export
ggtheme <- function(..., discrete_scale_colors, discrete_scale_colours, 
    continuous_scale_gradient) {
  
  if (!missing(discrete_scale_colors)) 
    discrete_scale_colours <- discrete_scale_colors
  
  if (!missing(discrete_scale_colours))
    theme_ggplot2_scales_discrete(discrete_scale_colours)
  
  if (!missing(continuous_scale_gradient))
    theme_ggplot2_scales_continuous(continuous_scale_gradient)
  
  dots <- list(...)
  dots <- unpack_list_aes(dots)
  
  if (!length(dots)) return(invisible())
  if (is.null(names(dots)) || any(names(dots) == ""))
    stop("All ellipsis arguments must be named or an aesthetic.")
  
  # theme any params that can be set via ggplot2::theme_update
  is_theme_dot <- names(dots) %in% names(formals(ggplot2::theme))
  do.call(ggplot2::theme_update, dots[is_theme_dot])
  dots <- dots[!is_theme_dot]
  
  # theme any params that can be set via ggplot2::update_geom_defaults or
  # ggplot2::update_stat_defaults
  is_aes_dot <- do.call(update_ggproto_defaults_aes, dots)
  dots <- dots[!is_aes_dot]
  
  # experimental option to allow overwriting of ggplot2 geom argument defaults
  do.call(set_theme_values, dots)
}
