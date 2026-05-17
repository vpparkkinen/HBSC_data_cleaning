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
library(pointblank)

# all variable names from 2026 protocol
doc_vars <- c(
  "grade",
  "monthbirth", "yearbirth",
  "sex",
  "health",
  "lifesat",
  "headache", "stomachache", "backache", "feellow", "irritable", 
  "nervous", "sleepdificulty", "dizzy",
  "physact60",
  "thinkbody",
  "breakfastwd", "breakfastwe",
  "fruits_2", "vegetables_2", "sweets_2", "softdrinks_2",
  "fmeal",
  "toothbr",
  "timeexe_1",
  "who5_1", "who5_2", "who5_3", "who5_4", "who5_5",
  "lonely",
  "injured12m",
  "bodyweight", "bodyheight",
  "alcltm", "alc30d_2",
  "drunkltm", "drunk30d",
  "smokltm", "smok30d_2",
  "esmokltm", "esmok30d",
  "cannabisltm_2", "cannabis30d_2",
  "likeschool",
  "schoolpressure",
  "studtogether", "studhelpful", "studaccept",
  "teacheraccept", "teachercare", "teachertrust",
  "bulliedothers",
  "beenbullied",
  "cbulliedothers",
  "cbeenbullied",
  "fight12m",
  "motherhome1", "fatherhome1", "stepmohome1", "stepfahome1", "fosterhome1", "elsehome1", "brothernum", "sisternum",
  "fasfamcar", "fasbedroom", "fascomputers", "fasbathroom", "fasdishwash", "fasholidays",
  "countryborn", "countrybornmo", "countrybornfa",
  "employfa", "employmo", "employnotfa", "employnotmo",
  "talkfather", "talkstepfa", "talkmother", "talkstepmo",
  "famhelp", "famsup", "famtalk", "famdec",
  "friendhelp", "friendcounton", "friendshare", "friendtalk",
  "emconlfreq1", "emconlfreq2", "emconlfreq3", "emconlfreq4",
  "emcsocmed1", "emcsocmed2", "emcsocmed3", "emcsocmed4", "emcsocmed5", "emcsocmed6", "emcsocmed7", "emcsocmed8", "emcsocmed9",
  "hadsex",
  "agesex",
  "contraceptcondom",
  "contraceptpill"
  )



adm_id_vars <- c("id1", "id2", "id3", "id4", "month", "year", "adm")

man_vars <- c(adm_id_vars, doc_vars)

length(man_vars)
#dmc_vars <- c("hbsc", "SEQNO", "cluster", "countryno", "region", "age", "agecat", "mbmi")
#length(dmc_vars)





printout_ranges <- FALSE

# reference data, assume this is the truth

ref26 <- read_sav("Protocol_variables_HBSC2025_26.sav") 

# numeric vars with value labels get class double, no value labels get class numeric,
# rest are -- from read_sav
#
# infer min/max values for doubles from value labels, for numerics min 0 max Inf,
# negatives wouldn't make any sense

# all variables from protocol reference dataset
cn <- colnames(ref26)

# check that all mandatory variables are included IN THE REFERENCE .sav FILE
mvi <- sapply(man_vars, \(x) x %in% cn) 
all(mvi)
#man_vars[!mvi]



length(cn) == length(unique(cn)) #check that the canonical varnames are unique


get_var_info <- function(x, varname){
  #var_class <- class(x)
  
  var_label <- attr(x, "label")
  # isweight <- grepl("^Weight", var_label) && inherits(x, "numeric")
  # is_numeric_notweight <- inherits(x, "numeric")
  is_char <- inherits(x, "character")
  is_unbounded_num <- inherits(x, "numeric")
  val_labels <- attr(x, "labels")
  allowed_vals <- if (is_char) NA else unname(val_labels)
  # no var is actually allowed to be negative
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
ranges <- lapply(names(ref26), 
                 \(x) get_var_info(ref26[[x]], varname = x))
ranges <- do.call(rbind, ranges)

if (printout_ranges){
  tt(ranges,
     theme = "striped",
     caption = "HBSC survey variable names, legal ranges, and (R) classes (as inferred by `read_sav()`)",
  ) |> tt_save(output = "var_ranges_classes.html")
}

tdat <- read_sav("TEST_DATA_IN.sav")

tdat2 <- zap_labels(tdat)

look_for(tdat)
generate_dictionary(tdat)
td_cn <- colnames(tdat)
# 
# for (n in td_cn){
#   if (inherits(tdat[[n]], "double")) class(tdat[[n]]) <- "numeric"
# }

write_sav(tdat, "TI_R_roundtrip.sav")

# varnames are unique?
length(td_cn) == length(unique(td_cn))

# varnames are valid HBSC2026 varnames?
sapply(td_cn, \(x) x %in% cn) |> all()

# all mandatory vars included?
sapply(man_vars, \(x) x %in% td_cn) |> all()

# which optional vars are included
indata_opt_vars <- setdiff(td_cn, man_vars)

# var classes as in HBSC reference data
check_var_classes <- function(indat, refdat, all_varnames){
  sapply(td_cn, \(x) identical(class(ref26[[x]]), class(tdat[[x]]))) |> all()
}

sapply(td_cn, \(x) identical(class(ref26[[x]]), class(tdat[[x]]))) |> all()

identical(class(ref26$lifesat), class(tdat$lifesat))



iswhole <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}






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
