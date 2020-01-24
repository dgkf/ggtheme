wrap <- function(..., width = 0.9 * getOption("width"), indent = 0, exdent = 0) {
  paste(collapse = "\n", strwrap(
    width = width, 
    indent = indent, 
    exdent = exdent, 
    paste0(...)))
}

experimental_message <- function(...) {
  if (getOption("ggtheme.experimental.notifications", TRUE))
    message("[ggtheme]\n", ..., "\n",
      "disable experimental feature messages using: \n\n",
      "    > options(ggtheme.experimental.notifications = FALSE)\n")
}