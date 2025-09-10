# This code prevents the following NOTE from appearing
# when CHECKING the package
#  summaryByVisit: no visible binding for global variable 'row_type'


utils::globalVariables(c("by_col",
                         "group_num",
                         ".x",
                         "t1",
                         "tbl_indent_id1",
                         "variable",
                         "var_type",
                         "row_type",
                         "var_label",
                         "add_n_stat_1",
                         "add_n_stat_2",
                         "add_n_stat_3",
                         "stat_0",
                         "stat_1",
                         "stat_2",
                         "stat_3",
                         "row_type"))
