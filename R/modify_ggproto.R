update_all_aes_defaults <- function(...) {
  dots <- list(...)
  objs <- lapply(getNamespaceExports("ggplot2"), getExportedValue, ns = "ggplot2")
  objs <- objs[vapply(objs, is_geom_or_stat, logical(1L))]
  
  # return TRUE if any aesthetics are updated
  # NOTE: this logic probably needs to be cleaned up such that it returns TRUE
  # if the geom/stat exists and the aesthetic is any valid aesthetic name
  any(vapply(objs, FUN.VALUE = logical(1L), function(obj) {
    new_defaults <- dots[is_nonna_default_aes(obj, names(dots))]
    update_aes_defaults(obj, ggplot2:::new_aes(new_defaults))
  }))
}


is_nonna_default_aes <- function(obj, aesnames) {
  std_aes_names <- ggplot2:::standardise_aes_names(aesnames)
  nonna_default_aesnames <- names(Filter(Negate(function(i) {
    is.atomic(i) && is.na(i)
  }), as.list(obj$default_aes)))
  std_aes_names %in% nonna_default_aesnames
}


is_default_aes <- function(obj, aesnames) {
  std_aes_names <- ggplot2:::standardise_aes_names(aesnames)
  std_aes_names %in% names(obj$default_aes)
}


update_aes_defaults <- function(x, new) {
  if (!length(new))  
    return(FALSE)
  
  if (inherits(x, "Geom")) {
    ggplot2::update_geom_defaults(x, new)
    return(TRUE)
  }
  
  if (inherits(x, "Stat")) {
    ggplot2::update_stat_defaults(x, new)
    return(TRUE)
  }
  
  FALSE
}


update_ggproto_defaults_aes <- function(...) {
  dots <- list(...)
  
  # rearrange dots so all general arguments come before geom/stat-specific ones
  is_geom_or_stat <- starts_with_geom_or_stat(names(dots))
  dots <- dots[c(which(!is_geom_or_stat), which(is_geom_or_stat))]
  
  dots <- Map(function(dotname, dot) {
    dotname <- split_dotted_fname_argname_pair(dotname)
    if (starts_with_geom_or_stat(dotname[[1]])) {
      args <- list(dot)
      names(args) <- dotname[[2]]
      obj <- get_ggproto(dotname[[1]])
      if (is_default_aes(obj, dotname[[2]]))
        update_aes_defaults(obj, ggplot2:::new_aes(args))
      else
        FALSE
    } else {
      arg <- list(dot)
      names(arg) <- dotname[[1]]
      x <- do.call(update_all_aes_defaults, arg)
    }
  }, dotname = names(dots), dot = dots)
  
  unlist(dots)
}


unpack_list_aes <- function(x) {
  if (is.null(names(x))) names(x) <- rep("", length(x))
  Reduce(append, Map(function(xname, xvalue) {
    if (inherits(xvalue, class(aes(1L)))) as.list(xvalue)
    else {
      x <- list(xvalue)
      names(x) <- xname
      x
    }
  }, names(x), x))
}


is_geom_or_stat <- function(x) inherits(x, "Geom") || inherits(x, "Stat")
starts_with_geom_or_stat <- function(x) grepl("^(geom|stat)", x)
starts_with_geom <- function(x) grepl("^geom", x)
starts_with_stat <- function(x) grepl("^stat", x)

split_dotted_fname_argname_pair <- function(x) {
  if (starts_with_geom_or_stat(x)) {
    x <- strsplit(x, "\\.")[[1]]
    if (length(x) > 1) x[[2]] <- paste0(x[-1], collapse = ".")
  }
  x
}


layer_call_formal <- function(geom_f, formal = c("geom", "stat")) {
  formal <- match.arg(formal, c("geom", "stat"))
  layer_call <- Filter(function(i) is.call(i) && i[[1]] == "layer", body(geom_f))
  if (!length(layer_call)) return(NULL)
  
  layer_fml  <- as.character(as.list(layer_call)[[1]][formal])
  if (!layer_fml %in% getNamespaceExports("ggplot2")) return(NULL)
  getExportedValue("ggplot2", layer_fml)
}


get_ggproto_aes <- function(x) {
  if (!inherits(x, "ggproto")) 
    return(NULL)
  
  if (inherits(x, "Geom"))
    getExportedValue("ggplot2", class(x$geom)[[1]])$default_aes
  else if (inherits(x, "Stat"))
    getExportedValue("ggplot2", class(x$stat)[[1]])$default_aes
}


get_ggproto <- function(x) {
  if (is.character(x)) {
    obj <- tryCatch(
      getExportedValue("ggplot2", x)(), 
      error = function(e) NULL)
    if (starts_with_geom(x)) obj$geom
    else if (starts_with_stat(x)) obj$stat
    else obj
  } else x
}