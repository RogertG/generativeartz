formula <- list(
  x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2)),
  y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
)

test_that("generate_data uses range and step", {
  df <- suppressMessages(generate_data(formula, range_from = 0, range_to = 1, step = 0.5))
  expect_equal(nrow(df), 9)
  expect_true(all(c("x_i", "y_i", "x", "y") %in% names(df)))
})

test_that("generate_data rejects malformed formulas", {
  expect_error(generate_data(list(a = quote(x_i))), "`formula` must be a list")
})

test_that("an empty log has no NA rows", {
  log <- suppressMessages(check_logfile_existence(tempfile()))
  expect_equal(nrow(log), 0)
})

test_that("unknown parameters are rejected", {
  expect_error(generate_img(formula, 1, colour = "red", img_path = tempdir(),
                            logfile_path = tempfile()), "Unknown argument")
})

test_that("images are logged and regenerated identically", {
  dir <- tempfile()
  log_path <- file.path(dir, "log.csv")
  img_path <- file.path(dir, "img")
  paths <- suppressMessages(generate_img(
    formula, nr_of_img = 2, polar = TRUE, color = "red", step = 0.2,
    width = 1, height = 1, img_path = img_path, logfile_path = log_path
  ))
  expect_length(paths, 2)
  expect_true(all(file.exists(paths)))

  log <- suppressMessages(check_logfile_existence(log_path))
  expect_equal(nrow(log), 2)
  expect_false(anyNA(log$file_name))
  expect_true(all(log$polar))
  expect_equal(log$color, c("red", "red"))
  expect_equal(log$step, c(0.2, 0.2))

  seed <- log$seed[1]
  expect_equal(get_seed_from_logfile(seed, log_path), seed)
  expect_equal(get_formula_from_logfile(seed, logfile_path = log_path), formula)

  new_path <- suppressMessages(regenerate_img(seed, width = 1, height = 1,
                                              img_path = img_path, logfile_path = log_path))
  expect_true(file.exists(new_path))
  log <- suppressMessages(check_logfile_existence(log_path))
  expect_equal(nrow(log), 3)
  expect_equal(log$seed[3], seed)
  expect_true(log$polar[3])
  expect_equal(log$color[3], "red")
  expect_equal(log$step[3], 0.2)

  # the regenerated image uses the same random numbers as the original
  set.seed(seed)
  original <- suppressMessages(generate_data(formula, step = 0.2))
  set.seed(seed)
  recreated <- suppressMessages(generate_data(
    get_formula_from_logfile(seed, logfile_path = log_path), step = log$step[3]))
  expect_equal(original, recreated)
})

test_that("regenerate_img works with logs written by older versions", {
  dir <- tempfile()
  dir.create(dir)
  log_path <- file.path(dir, "log.csv")
  writeLines(c(
    "file_name\tseed\tformula_x\tformula_y",
    "NA\tNA\tNA\tNA",
    "old.png\t1821\trunif(1, -1, 1) * x_i^2 - sin(y_i^2)\trunif(1, -1, 1) * y_i^3 - cos(x_i^2)"
  ), log_path)
  path <- suppressMessages(regenerate_img(1821, polar = TRUE, step = 0.5, width = 1, height = 1,
                                          img_path = dir, logfile_path = log_path))
  expect_true(file.exists(path))
  log <- suppressMessages(check_logfile_existence(log_path))
  expect_equal(nrow(log), 2)
  expect_true(log$polar[2])
})

test_that("regenerate_img fails clearly for unknown seeds", {
  log_path <- tempfile()
  expect_error(suppressMessages(regenerate_img(1, logfile_path = log_path)), "not found")
})

test_that("doubles are logged with full precision", {
  log_path <- tempfile()
  suppressMessages(generate_logfile_entry(check_logfile_existence(log_path), formula, 1,
                                          "a.png", logfile_path = log_path))
  log <- suppressMessages(check_logfile_existence(log_path))
  expect_identical(log$range_to, 2 * pi)
  expect_identical(log$alpha, 0.2)
})
