theme_ggplot2_layer_args <- function() {
  exports <- getNamespaceExports("ggplot2")
  names(exports) <- exports
  exports <- lapply(exports, getExportedValue, ns = "ggplot2")
  
  # restore original ggplot2 bindings
  restore_ggplot2_binding()
  
  # convert all formal arguments for ggplot2 layer-generating functions to 
  # instead redirect to ggtheme_get calls
  for (ei in seq_along(exports)) {
    e <- exports[[ei]]
    if (!is.function(e)) next
    if (!identical(names(formals(e))[1:2], c("mapping", "data"))) next
    
    modified_fmls <- formals(e)
    modified_i <- setdiff(names(formals(e)), c("mapping", "data", "geom", "stat", "..."))
    modified_fmls[modified_i] <- Map(function(argname, argval) {
      bquote(get_theme_value(.(names(exports)[[ei]]), .(argname), default = .(argval)))
    }, names(modified_fmls[modified_i]), modified_fmls[modified_i])
    formals(e) <- modified_fmls
    
    modify_ggplot2_binding(names(exports)[[ei]], e)
  }
}
