
summaryTable <- function(data,
                         vars,
                         group,
                         ci_cat = "clopper-pearson",
                         conf_level = 0.95,
                         digits_cat = 1){
  if(ci_cat == "clopper-pearson"){
    ci_cat_gt <- "exact"
  }else if(ci_cat == "wilson" |
           ci_cat == "wilson.no.correct" |
           ci_cat == "jeffreys"|
           ci_cat == "agresti.coull"){
    ci_cat_gt <- ci_cat
  }else{
    stop(paste0("The chosen CI method '", ci_cat, "' does not exist or is not yet implemented."))
  }

  # check whether 'group' was given
  if(missing(group)){
    data %>%
      select(all_of(vars)) %>%
      tbl_summary(digits = all_categorical() ~ digits_cat) %>%
      add_ci(all_categorical() ~ ci_cat_gt,
             conf.level = conf_level,
             pattern = "{stat} ({ci})")

  }else{
    data %>%
      select(c(all_of(vars), all_of(group))) %>%
      tbl_summary(by = group,
                  digits = all_categorical() ~ digits_cat) %>%
      add_ci(all_categorical() ~ ci_cat_gt,
             conf.level = conf_level,
             pattern = "{stat} ({ci})")
  }
}
