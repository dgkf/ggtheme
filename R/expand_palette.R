#' @export
make_pal_expand_lighter <- function(colors) {
  f <- function(n) {}
  body(f) <- bquote(pal_expand_lighter(.(colors), n))
  f
}

pal_expand_lighter <- function(colors, n) {
  if (n <= length(colors)) return(colors[1:n])
  n_added_palettes <- n %/% length(colors)
  lighten_amts <- (0:n_added_palettes) / (n_added_palettes / 0.8)
  lightened_colors <- lapply(lighten_amts, colorspace::lighten, col = colors)
  c(do.call(rbind, lightened_colors))[1:n]
}