test_that("without a group, one line is drawn from all patients", {
  d <- hb_fixture()
  res <- draw_quietly(lineplotSCI(d, hb, visit_nr))
  expect_equal(rownames(res$centre), "hb")
  truth <- with(d[!is.na(d$hb), ], tapply(hb, visit_nr, stats::median))
  expect_equal(unname(res$centre[1, ]), as.vector(truth[colnames(res$centre)]))
  expect_equal(sum(res$n), sum(!is.na(d$hb)))
  # group_names relabels the single line
  expect_no_error(draw_quietly(lineplotSCI(d, hb, visit_nr,
                                           group_names = "All patients")))
})

test_that("bare, quoted and stored column names give the same result", {
  d <- hb_fixture()
  bare   <- draw_quietly(lineplotSCI(d, hb, visit_nr, Arm, visit = visit))
  quoted <- draw_quietly(lineplotSCI(d, "hb", "visit_nr", "Arm", visit = "visit"))
  v <- "hb"; o <- "visit_nr"; g <- "Arm"; lab <- "visit"
  stored <- draw_quietly(lineplotSCI(d, v, o, g, visit = lab))
  expect_equal(bare, quoted)
  expect_equal(bare, stored)
})

test_that("visit accepts a vector of labels", {
  d <- hb_fixture()
  expect_no_error(draw_quietly(lineplotSCI(d, hb, visit_nr, Arm,
                                           visit = paste0("C", 1:9))))
  expect_error(draw_quietly(lineplotSCI(d, hb, visit_nr, Arm, visit = c("a", "b"))),
               "one entry per tick")
})

test_that("informative errors", {
  d <- hb_fixture()
  expect_error(lineplotSCI(d, nope, visit_nr), "not found in 'data': nope")
  expect_error(lineplotSCI(d, hb, visit_nr, nope), "not found in 'data': nope")
  expect_error(lineplotSCI(d, hb), "'var' and 'order' must be specified")
  expect_error(lineplotSCI(var = hb, order = visit_nr), "'data' must be specified")
})
