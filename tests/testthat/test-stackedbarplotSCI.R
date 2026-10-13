test_that("levels of one variable are counted per group and time point", {
  d <- hb_fixture()
  res <- draw_quietly(stackedbarplotSCI(d, hb_cat, time = visit_nr, group = Arm))
  for (g in c("Arm A", "Arm B")) for (t in 1:9) {
    x <- d$hb_cat[d$Arm == g & d$visit_nr == t]
    truth <- table(factor(x, levels = c("High", "Low", "Normal")))
    expect_equal(as.vector(res$counts[, g, as.character(t)]), as.vector(truth))
    expect_equal(res$n[g, as.character(t)], sum(!is.na(x)))
  }
  expect_equal(unname(apply(res$percent, c(2, 3), sum)),
               matrix(100, 2, 9))
})

test_that("quoted and bare column names give the same result", {
  d <- hb_fixture()
  a <- draw_quietly(stackedbarplotSCI(d, hb_cat, time = visit_nr, group = Arm))
  b <- draw_quietly(stackedbarplotSCI(d, "hb_cat", time = "visit_nr", group = "Arm"))
  expect_equal(a, b)
})

test_that("time points are ordered numerically, factor levels are kept", {
  d <- three_arm_fixture()
  d$visit[d$visit == "Visit 4"] <- "Visit 10"
  res <- draw_quietly(stackedbarplotSCI(d, grade, time = visit, group = arm))
  expect_equal(dimnames(res$counts)[[3]],
               c("Visit 1", "Visit 2", "Visit 3", "Visit 10"))
  d$grade <- factor(d$grade, levels = c("G4", "G3", "G2", "G1"))
  res <- draw_quietly(stackedbarplotSCI(d, grade, time = visit, group = arm))
  expect_equal(dimnames(res$counts)[[1]], c("G4", "G3", "G2", "G1"))
})

test_that("missing values are dropped or shown as their own segment", {
  d <- three_arm_fixture()
  res <- draw_quietly(stackedbarplotSCI(d, grade, group = arm, missing = TRUE,
                                        missing_text = "Unknown"))
  expect_equal(dimnames(res$counts)[[1]][5], "Unknown")
  expect_equal(sum(res$counts["Unknown", , ]), sum(is.na(d$grade)))
  expect_equal(sum(res$n), nrow(d))
  res <- draw_quietly(stackedbarplotSCI(d, grade, group = arm))
  expect_equal(sum(res$n), sum(!is.na(d$grade)))
})

test_that("several Yes/No columns are counted against the number of patients", {
  ae <- data.frame(id      = c(1, 1, 2, 3, 4, 5),
                   arm     = c("A", "A", "A", "B", "B", "B"),
                   fatigue = c("Yes", "No", "No", "Yes", NA, "Yes"),
                   nausea  = c("Yes", "Yes", "No", "No", "Yes", "No"))
  res <- draw_quietly(stackedbarplotSCI(ae, var = c("fatigue", "nausea"),
                                        group = arm, id = id))
  expect_equal(res$counts[, "A", 1], c(fatigue = 1, nausea = 2))
  expect_equal(res$counts[, "B", 1], c(fatigue = 2, nausea = 1))
  expect_equal(res$n[, 1], c(A = 2, B = 3))     # distinct patients
  expect_equal(res$percent["nausea", "A", 1], 100)

  ae$nausea[1] <- "maybe"
  expect_error(stackedbarplotSCI(ae, var = c("fatigue", "nausea"), group = arm),
               "Yes' / 'No")
})

test_that("informative errors", {
  d <- hb_fixture()
  expect_error(stackedbarplotSCI(d, nope), "not found")
  expect_error(draw_quietly(stackedbarplotSCI(d, hb_cat, group = Arm,
                                              group_names = "only one")),
               "one entry per group")
  expect_error(stackedbarplotSCI(d, hb_cat, level_names = c("a", "b")),
               "one entry per segment")
})
