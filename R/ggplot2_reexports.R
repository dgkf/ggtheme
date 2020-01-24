#' placeholder for ggplot2 namespace backup
.ggplot2 <- list()



#' get ggplot2 environment attached in global search path
search_ggplot2_env <- function() {
  if (!"package:ggplot2" %in% search()) return(NULL)
  pos <- which("package:ggplot2" == search())
  Reduce(function(l, r) parent.env(l), 1:(pos-1), init = .GlobalEnv)
}



#' cache ggplot2 objects into ggtheme, allowing them to be modified directly
#' within ggplot2 and restored as needed
#' 
#' This is necessary because ggplot2's internal versions of scale_* functions
#' will be used with precedence over user functions or masked versions available
#' in a global environment.
#' 
#' @import ggplot2
#' 
.onLoad <- function(libname, pkgname) {
  ggplot2_exports <- as.list(getNamespace("ggplot2"))[getNamespaceExports("ggplot2")]
  ns <- getNamespace("ggtheme")
  unlockBinding(".ggplot2", ns)
  assign(".ggplot2", getNamespace("ggplot2"), envir = getNamespace("ggtheme"))
  lockBinding(".ggplot2", ns)
  setHook("detach", restore_ggplot2_binding)
}



#' restore a single binding within ggplot2 namespace back to original value
restore_ggplot2_binding <- function(x = getNamespaceExports("ggplot2"), ...) {
  for (xi in x) modify_ggplot2_binding(xi, .ggplot2[[xi]], ...)
}



#' modify a locked binding within an environment (ggplot2 & ggtheme by default)
modify_ggplot2_binding <- function(x, value, ...) {
  envir <- search_ggplot2_env()
  
  # break on ggplot2 reexports
  binding_env <- environment(get(x, envir = envir))
  if (is.null(binding_env) || !isNamespace(binding_env)) return()
  binding_ns <- getNamespaceName(binding_env)
  if (binding_ns != "ggplot2") return()
      
  # modify binding
  unlockBinding(x, env = envir)
  assign(x, value, envir = envir)
  lockBinding(x, env = envir)
}
