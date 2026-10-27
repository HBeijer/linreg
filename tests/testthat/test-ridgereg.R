test_that("ridge coefficients agree with MASS", {
  # Skip this test if the optional reference package is unavailable.
  skip_if_not_installed("MASS")
  f <- Petal.Length ~ Sepal.Length + Sepal.Width
  x <- as.matrix(iris[c("Sepal.Length", "Sepal.Width")])
  n <- nrow(x)

  # Compare no penalty, a smaller penalty and a larger penalty.
  for (penalty in c(0, 2, 100)) {
    # Adjust lambda because MASS and our functions standardise differently.
    reference <- MASS::lm.ridge(f, data = iris, lambda = penalty * n / (n - 1))
    b <- unname(coef(reference))

    # Convert MASS's original-unit coefficients to our standardised units.
    # b[1] is the intercept; b[-1] contains the predictor coefficients.
    expected <- c(b[1] + sum(b[-1] * colMeans(x)), b[-1] * apply(x, 2, sd))

    # Run the same comparison for both fitting functions.
    for (fit in list(ridgereg, ridgereg_qr)) {
      # Ignore names here and allow tiny numerical rounding differences.
      expect_equal(unname(coef(fit(f, iris, penalty))),
                   unname(expected), tolerance = 1e-8)
    }
  }
})

test_that("models and numerical predictions behave correctly", {
  f <- Petal.Length ~ Sepal.Length + Sepal.Width
  new_rows <- iris[1:3, c("Sepal.Length", "Sepal.Width")]

  # Both algorithms should solve the same ridge problem.
  expect_equal(coef(ridgereg(f, iris, 2)), coef(ridgereg_qr(f, iris, 2)))

  for (fit in list(ridgereg, ridgereg_qr)) {
    model <- fit(f, iris, 2)

    # Check the model class, coefficient names and printed output.
    expect_s3_class(model, "ridgereg")
    expect_named(coef(object = model), colnames(model$X))
    expect_output(print(model), "Coefficients:")

    # Predictions should be a vector with one value per training row.
    expect_length(predict(model), nrow(iris))
    expect_null(dim(predict(model)))

    # Without newdata, return the saved fitted values.
    expect_equal(predict(model), as.numeric(model$fitted_values))

    # Reusing training rows should reproduce their fitted predictions.
    expect_equal(predict(model, new_rows), predict(model)[1:3])

    # An observation's prediction should be the same alone or in a batch.
    expect_equal(predict(model, new_rows[1, , drop = FALSE]),
                 predict(model, new_rows)[1])
  }
})

test_that("prediction handles categories and missing values", {
  for (fit in list(ridgereg, ridgereg_qr)) {
    model <- fit(Petal.Length ~ Sepal.Width + Species, iris, 2)

    # This batch contains only one species and one missing predictor.
    new_rows <- data.frame(Sepal.Width = c(3.5, NA),
                           Species = c("setosa", "setosa"))
    values <- predict(model, new_rows)

    # Preserve both rows and the missing predictor should give an NA prediction.
    expect_length(values, 2L)
    expect_equal(values[1], predict(model, iris[1, , drop = FALSE]))
    expect_true(is.na(values[2]))

    # Reject a category that was not present in the training definitions.
    new_rows$Species[1] <- "unknown_species"
    expect_error(predict(model, new_rows), "new level")
  }
})

test_that("invalid inputs are rejected appropriately", {
  f <- Petal.Length ~ Sepal.Length + Sepal.Width

  # x2 depends exactly on x1 and constant has zero standard deviation.
  d <- data.frame(y = c(2, 1, 5, 7), x1 = 1:4, x2 = 2 * (1:4), constant = 1)

  for (fit in list(ridgereg, ridgereg_qr)) {
    # Reject negative, missing, infinite, multiple, text or complex penalties.
    for (penalty in list(-1, NA, Inf, c(1, 2), "2", 1 + 1i)) {
      expect_error(fit(f, iris, penalty), "lambda")
    }

    # Check the errors promised by our input validation.
    expect_error(fit(y ~ 0 + x1, d, 2), "intercept")
    expect_error(fit(Species ~ Sepal.Width, iris, 2), "response")
    expect_error(fit(y ~ constant, d, 2), "standard deviations")
    expect_error(fit(y ~ x1 + x2, d, 0), "full column rank")

    # A positive penalty should allow dependent predictor columns.
    expect_length(predict(fit(y ~ x1 + x2, d, 2)), nrow(d))
  }
})
