guess_cat_num <- function(var, descr)  {
  # if var is missing, return "?"
  if (missing(var))  {
    warning("no variable to guess")
    return("?")
  }
  
  # all factors are cat
  if (is.factor(var)) {
    return("cat")
  }
  # for unsupported classes return "oth"
  if (class(var)[1] %in% c("numeric", "integer", "character", "logical", "Date", "POSIXct"))  {
    var_class <- class(var)[1]
  } else {
    return("oth")
  }
  
  # variable type
  var_type <- typeof(var)
  
  # number of unique values
  if (missing(descr))  {
    var_unique <- length(unique(var))
  } else {
    var_unique <- descr$unique
  }
  
  # treat Date always as cat
  if (var_class == "Date")  {
    return("cat")
  }
  
  # Decide on type and number of unique values
  if (var_type %in% c("integer", "double")) {
    if (var_unique < 10)  {
      return("cat")
    } else {
      return("num")
    }
  } else  {
    return("cat")
  }
} # guess_cat_num



