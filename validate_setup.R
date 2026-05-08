if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}

getwd()
library(haven)
library(data.table)
#library(sjPlot)
library(tinytable)

# reference data, assume this is the truth

ref26 <- read_sav("Protocol_variables_HBSC2025_26.sav") 

# numeric vars with value labels get class double, no value labels get class numeric,
# rest are -- from read_sav
#
# infer min/max values for doubles from value labels, for numerics min 0 max Inf,
# negatives wouldn't make any sense

get_var_info <- function(x, varname){
  #var_class <- class(x)
  
  var_label <- attr(x, "label")
  # isweight <- grepl("^Weight", var_label) && inherits(x, "numeric")
  # is_numeric_notweight <- inherits(x, "numeric")
  is_char <- inherits(x, "character")
  is_unbounded_num <- inherits(x, "numeric")
  val_labels <- attr(x, "labels")
  allowed_vals <- if (is_char) NA else unname(val_labels)
  alval_min <- if (is_unbounded_num) 0 else min(allowed_vals)
  alval_max <- if (is_unbounded_num) Inf else max(allowed_vals)
  type = if (inherits(x, "double")) "double" else if (is_unbounded_num) "numeric" else "character"
  out <- data.frame(varname = varname,
                    minval = alval_min, 
                    maxval = alval_max,
                    type = type)
  return(out)
}


# get legal ranges, classes for all vars in blank reference data

ranges <- lapply(names(ref26), \(x) get_var_info(ref26[[x]], varname = x)) |> data.table::rbindlist()

tt(ranges,
   theme = "striped",
   caption = "HBSC survey variable names, legal ranges, and (R) classes (as inferred by `read_sav()`)",
   ) |> tt_save(output = "var_ranges_classes.html")




# 
# 
# 
# 
# lapply(ref26, \(x) attr(x, "labels"))
# 
# 
# 
# lapply(ref26, \(x) attr(x, "na_range"))
# 
# ## reference_labels to pdf
# 
# 
# 
# view_df(ref26)
# 
# 
# test <- read_sav("FILE")
# 
# dttest <- as.data.table(test) # 
# 
# ## column labels
# 
# tb_col_labels <- lapply(test, \(x) attr(x, "label"))
# 
# #dt_col_labels <- dttest[, lapply(.SD, \(x) attr(x, "label"))]
# 
# dt_col_labels <- lapply(dttest, \(x) attr(x, "label"))
# 
# all.equal(col_labels, dt_col_labels)
# 
# 
# # value labels
# 
# val_labels_tb <- lapply(test, \(x) attr(x, "labels")) #tibble
# val_labels_dt <- lapply(dttest, \(x) attr(x, "labels")) #DT
# all.equal(val_labels_tb, val_labels_dt)
# 
# # plot labels
# 
# view_df(test)
