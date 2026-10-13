test_that("every type dispatches to its function", {
  d <- hb_fixture()
  expect_s3_class(summarizeSCI("table", data = d, vars = "hb", group = "Arm",
                               as_flex_table = FALSE),
                  "gtsummary")
  expect_no_error(draw_quietly(summarizeSCI("boxplot", data = d, var = hb,
                                            time = visit_nr, group = Arm)))
  expect_equal(
    draw_quietly(summarizeSCI("lineplot", data = d, var = hb,
                              order = visit_nr, group = Arm)),
    draw_quietly(lineplotSCI(d, hb, visit_nr, Arm)))
  expect_equal(
    draw_quietly(summarizeSCI("barplot", data = d, var = hb_cat,
                              time = visit_nr, group = Arm)),
    draw_quietly(stackedbarplotSCI(d, hb_cat, time = visit_nr, group = Arm)))
})

test_that("bare and quoted column names survive the pass through '...'", {
  d <- hb_fixture()
  expect_equal(
    draw_quietly(summarizeSCI("lineplot", data = d, var = hb,
                              order = visit_nr, group = Arm)),
    draw_quietly(summarizeSCI("lineplot", data = d, var = "hb",
                              order = "visit_nr", group = "Arm")))
  expect_equal(
    draw_quietly(summarizeSCI("barplot", data = d, var = hb_cat, group = Arm)),
    draw_quietly(summarizeSCI("barplot", data = d, var = "hb_cat", group = "Arm")))
})

test_that("unknown types are rejected", {
  expect_error(summarizeSCI("surv"), "should be one of")
})
