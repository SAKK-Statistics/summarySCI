summaryTable_missing <-function(data, vars=names(data), test_cat = NULL, group=NULL, missing=TRUE) {

  # check if required packages are install and do so if not
  list.of.packages <- c("gtsummary", "dplyr", "Hmisc")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) {install.packages(new.packages)}

  #load packages
  library(gtsummary)
  library(dplyr)
  library(Hmisc)

  # If missings shouldn't count  for the demoninator for percentages
  # If a group was defined
  # If no p value needed
  if (missing==FALSE) {
    if (!missing(group)) {
      if (missing(test_cat)){
        t<-data|>
          dplyr::select(vars, group)|>
          tbl_summary(by=group)
      }
      # If test
      else if (!is.na(test_cat)) {
        t<-data|>
          dplyr::select(vars, group)|>
          tbl_summary(by=group)|>
          add_p(test=list(all_categorical() ~ test_cat))
      }
    }
    # If no group was defined
    else  {
      t<-data|>
        dplyr::select(vars)|>
        tbl_summary()
    }
  }

  # If missings should count for the demoninator for percentages (default)
  else {
    data2<-data
    data3<-data
    # If a group was defined
    if (!missing(group)) {
      # If no p value (test) needed
      if (missing(test_cat)){
        for (i in colnames(data2|>
                           dplyr::select(vars, group))) {
          if (is.factor(data2[[i]])==TRUE | is.character(data2[[i]])) {
            data2[[i]]<- forcats::fct_explicit_na(as.factor(data2[[i]]))
            if (!is.null(attr(data[[i]], "label"))) {
              label(data2[[i]]) <- attr(data[[i]], "label")
            }
          }
          else if (all(data2[[i]] %in% c(0, 1, NA))) {
            data2[[i]]<- forcats::fct_explicit_na(factor(data2[[i]], labels=c("No", "Yes")))
            if (!is.null(attr(data[[i]], "label"))) {
              label(data2[[i]]) <- attr(data[[i]], "label")
            }
          }
          t<-data2|>
            tbl_summary(by=group,
                        type = list(all_dichotomous() ~ "categorical"))
        }
      }
      # If test
      else if (!is.na(test_cat)) {
        for (i in colnames(data2|>
                           dplyr::select(vars, group))) {
          if (is.factor(data2[[i]])==TRUE | is.character(data2[[i]])) {
            data2[[i]]<- forcats::fct_explicit_na(as.factor(data2[[i]]))
            if (!is.null(attr(data[[i]], "label"))) {
              label(data2[[i]]) <- attr(data[[i]], "label")
            }
          }
          else if (all(data2[[i]] %in% c(0, 1, NA))) {
            data2[[i]]<- forcats::fct_explicit_na(factor(data2[[i]], labels=c("No", "Yes")))
            if (!is.null(attr(data[[i]], "label"))) {
              label(data2[[i]]) <- attr(data[[i]], "label")
            }
          }
          t00<-data2|>
            tbl_summary(by=group,
                        type = list(all_dichotomous() ~ "categorical"))
        }
        # if no missing
        if (missing==FALSE){
          for (i in colnames(data|>
                             dplyr::select(vars, group))) {
            if (all(data[[i]] %in% c(0, 1, NA))) {
              data[[i]]<- factor(data[[i]], labels=c("No", "Yes"))
              if (!is.null(attr(data[[i]], "label"))) {
                label(data[[i]]) <- attr(data[[i]], "label")
              }
            }
          }
          t<-data|>
            dplyr::select(vars, group)|>
            tbl_summary(by=group, missing="no",
                        type = list(all_dichotomous() ~ "categorical"), )|>
            add_p(test=list(all_categorical() ~ test_cat))
        }
        # if with missing
        if (missing==TRUE){
          for (i in colnames(data3|>
                             dplyr::select(vars, group))) {
            if (all(data3[[i]] %in% c(0, 1, NA))) {
              data3[[i]]<- factor(data3[[i]], labels=c("No", "Yes"))
              if (!is.null(attr(data[[i]], "label"))) {
                label(data3[[i]]) <- attr(data[[i]], "label")
              }
            }
          }
          t0<-data3|>
            dplyr::select(vars, group)|>
            tbl_summary(by=group, missing="no",
                        type = list(all_dichotomous() ~ "categorical"))|>
            add_p(test=list(all_categorical() ~ test_cat))|>
            modify_column_hide(c("stat_1", "stat_2"))

          t<- tbl_merge(tbls = list(t00, t0))|>
            modify_spanning_header(everything()~NA_character_)

        }
        #if with both missing/non-missing
        if (missing=="both"){

          for (i in colnames(data3|>
                             dplyr::select(vars, group))) {
            if (all(data3[[i]] %in% c(0, 1, NA))) {
              data3[[i]]<- factor(data3[[i]], labels=c("No", "Yes"))
              if (!is.null(attr(data[[i]], "label"))) {
                label(data3[[i]]) <- attr(data[[i]], "label")
              }
            }
          }
          t0<-data3|>
            dplyr::select(vars, group)|>
            tbl_summary(by=group, missing="no",
                        type = list(all_dichotomous() ~ "categorical"))|>
            add_p(test=list(all_categorical() ~  test_cat))


          t<- tbl_merge(tbls = list(t00, t0))|>
            modify_spanning_header(c("stat_1_1", "stat_2_1") ~ "**With missing**",
                                   c("stat_1_2", "stat_2_2", "p.value_2") ~ "**Without missing**")

        }
      }
    }
    # If no group was defined
    else if (missing(group)) {
      for (i in colnames(data2|>
                         dplyr::select(vars))) {
        if (is.factor(data2[[i]])==TRUE | is.character(data2[[i]])) {
          data2[[i]]<- forcats::fct_explicit_na(as.factor(data2[[i]]))
          if (!is.null(attr(data[[i]], "label"))) {
            label(data2[[i]]) <- attr(data[[i]], "label")
          }
        }
        else if (all(data2[[i]] %in% c(0, 1, NA))) {
          data2[[i]]<- forcats::fct_explicit_na(factor(data2[[i]], labels=c("No", "Yes")))
          if (!is.null(attr(data[[i]], "label"))) {
            label(data2[[i]]) <- attr(data[[i]], "label")
          }
        }}
      t00<-data2|>
        dplyr::select(vars)|>
        tbl_summary(type = list(all_dichotomous() ~ "categorical"))

      for (i in colnames(data3|>
                         dplyr::select(vars, group))) {
        if (all(data3[[i]] %in% c(0, 1, NA))) {
          data3[[i]]<- factor(data3[[i]], labels=c("No", "Yes"))
          if (!is.null(attr(data[[i]], "label"))) {
            label(data3[[i]]) <- attr(data[[i]], "label")
          }
        }
      }
      t0<-data3|>
        dplyr::select(vars)|>
        tbl_summary(missing="no",
                    type = list(all_dichotomous() ~ "categorical"))


      if (missing=="both"){
        t<- tbl_merge(tbls = list(t00, t0))|>
          modify_spanning_header(c("stat_0_1") ~ "**With missing**",
                                 c("stat_0_2") ~ "**Without missing**")
      }
      if (missing==FALSE){
        t<-data3|>
          dplyr::select(vars)|>
          tbl_summary(type = list(all_dichotomous() ~ "categorical"))
      }
      if (missing==TRUE){
        for (i in colnames(data2|>
                           dplyr::select(vars))) {
          if (is.factor(data2[[i]])==TRUE | is.character(data2[[i]])) {
            data2[[i]]<- forcats::fct_explicit_na(as.factor(data2[[i]]))
            if (!is.null(attr(data[[i]], "label"))) {
              label(data2[[i]]) <- attr(data[[i]], "label")
            }
          }
          else if (all(data2[[i]] %in% c(0, 1, NA))) {
            data2[[i]]<- forcats::fct_explicit_na(factor(data2[[i]], labels=c("No", "Yes")))
            if (!is.null(attr(data[[i]], "label"))) {
              label(data2[[i]]) <- attr(data[[i]], "label")
            }
          }
          t<-data2|>
            dplyr::select(vars)|>
            tbl_summary(type = list(all_dichotomous() ~ "categorical"))
        }
      }
    }
  }
  return(t)
}
