context("test-tile_maker")


test_that("classes work", {
  expect_equal(ico("star") %>% class(), "shiny.tag")
  expect_equal(solo_box(value = 10, txt = "blah", former = 5) %>%
    class(), "shiny.tag")
  solo_gradient_box(value = 10, txt = "blah", former = 5) %>%
    class() %>%
    expect_equal(., "shiny.tag")
  multi_box(
    values = c(3, 45), title = "Important <br>button",
    number_zoom = 300, icons = c("apple", "calendar"),
    color = "warning", txt = c("times", "reports")
  ) %>%
    class() %>%
    expect_equal(., "shiny.tag")

  tile_matrix(
    data = head(iris),
    values = Sepal.Length,
    txt = Species
  ) %>%
    class() %>%
    expect_equal(., "shiny.tag")

  div_maker(
    subtitle = "Quantativity factors", textModifier = "h1",
    solo_gradient_box(value = 70),
    solo_box(value = 34)
  ) %>%
    class() %>%
    expect_equal(., "shiny.tag")

  finisher(
    title = "Hello",
    divs = solo_box(
      value = 3.57, txt = "Times apple eaten",
      icon = "apple"
    )
  ) %>%
    class() %>%
    expect_equal(., "shiny.tag")
})

test_that("'former' calc and chevrons work", {
  expect_equal(solo_box(value = 10, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("chevron-up", x = .) %>%
    sum(), 1)
  expect_equal(solo_box(value = 4, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("chevron-down", x = .) %>%
    sum(), 1)
  expect_equal(solo_box(value = 10, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("100%", x = .) %>%
    sum(), 1)

  expect_equal(solo_gradient_box(value = 10, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("chevron-up", x = .) %>%
    sum(), 1)
  expect_equal(solo_gradient_box(value = 4, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("chevron-down", x = .) %>%
    sum(), 1)
  expect_equal(solo_gradient_box(value = 10, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("100%", x = .) %>%
    sum(), 1)

  expect_equal(solo_box(value = 10, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("100%", x = .) %>%
    sum(), 1)

  tile_matrix(
    data = iris %>% head(4),
    values = Sepal.Length, former = Sepal.Width,
    txt = Species
  ) %>%
    unlist() %>%
    grepl("chevron-up", x = .) %>%
    sum() %>%
    expect_equal(., 4)
})

test_that("raw_comparisons parameter works correctly", {
  # Test that former=0 automatically uses raw format (no Inf%)
  expect_equal(solo_box(value = 10, txt = "test", former = 0) %>%
    unlist() %>%
    grepl("last: 0", x = .) %>%
    sum(), 1)
  
  # Test that former=0 does not contain Inf or NaN
  expect_equal(solo_box(value = 10, txt = "test", former = 0) %>%
    unlist() %>%
    grepl("Inf|NaN", x = .) %>%
    sum(), 0)
  
  # Test raw_comparisons=TRUE shows "last: X" format
  expect_equal(solo_box(value = 10, txt = "test", former = 5, raw_comparisons = TRUE) %>%
    unlist() %>%
    grepl("last: 5", x = .) %>%
    sum(), 1)
  
  # Test raw_comparisons=TRUE does not show percentage
  expect_equal(solo_box(value = 10, txt = "test", former = 5, raw_comparisons = TRUE) %>%
    unlist() %>%
    grepl("%", x = .) %>%
    sum(), 0)
  
  # Test raw_comparisons=FALSE with non-zero former shows percentage
  expect_equal(solo_box(value = 10, txt = "test", former = 5, raw_comparisons = FALSE) %>%
    unlist() %>%
    grepl("100%", x = .) %>%
    sum(), 1)
  
  # Test solo_gradient_box with former=0
  expect_equal(solo_gradient_box(value = 10, txt = "test", former = 0) %>%
    unlist() %>%
    grepl("last: 0", x = .) %>%
    sum(), 1)
  
  # Test solo_gradient_box with raw_comparisons=TRUE
  expect_equal(solo_gradient_box(value = 10, txt = "test", former = 5, raw_comparisons = TRUE) %>%
    unlist() %>%
    grepl("last: 5", x = .) %>%
    sum(), 1)
})

test_that("all colors work", {
  expect_equal(solo_gradient_box(value = 10, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("danger", x = .) %>%
    sum(), 2)
  expect_equal(solo_gradient_box(value = 80, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("warning", x = .) %>%
    sum(), 2)
  expect_equal(solo_gradient_box(value = 95, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("success", x = .) %>%
    sum(), 2)
})

test_that("errors error out", {
  expect_error(tile_matrix(
    data = head(iris),
    values = Species,
    txt = Species
  ))
})

test_that("protections work", {
  multi_box(
    values = c(3, 45), number_zoom = 300, icons = c("apple", "calendar"),
    color = "warning",
    txt = c("times", "reports")
  ) %>%
    class() %>%
    expect_equal(., "shiny.tag")

  multi_box(
    values = c(3, 45), title = "Important <br>button",
    number_zoom = 300, color = "warning",
    txt = c("times", "reports")
  ) %>%
    class() %>%
    expect_equal(., "shiny.tag")

  # tile_matrix(data = iris %>%
  #               dplyr::mutate(Sepal.Length = ifelse(Sepal.Length < 6,
  #                                            NA,Sepal.Length)),
  #             values = Sepal.Length, txt = Species) %>%
  #   unlist() %>% grepl(pattern = "0.001", x = .) %>% sum %>%
  #   expect_equal(., 83)

  expect_warning(
    tile_matrix(
      data = iris %>%
        dplyr::mutate(Sepal.Length = ifelse(Sepal.Length < 6,
          NA, Sepal.Length
        )),
      values = Sepal.Length, txt = Species
    )
  )

  expect_warning(
    tile_matrix(
      data = data.frame(v = c(1, 1, 1), f = c(1, 1, NA)),
      values = v, former = f
    )
  )

  # tile_matrix(data = data.frame(v = c(1,1,1), f = c(1,1,NA)),
  #             values = v, former = f) %>%
  #   unlist() %>% grepl(pattern = "99900%", x = .) %>% sum %>%
  #   expect_equal(., 1)

  multi_box(
    values = c(3, 45), title = "Important <br>button",
    number_zoom = 300, icons = c("apple", "calendar"), color = "warning"
  ) %>%
    unlist() %>%
    grepl(" ", x = .) %>%
    sum() %>%
    expect_equal(3)
})


# -------------------------------------------------------------------------

context("relative mode/not-relative")

solo_gradient_box(value = 4) %>% expect_message()

expect_error(solo_gradient_box(
  value = 40, target = 50,
  thresholdHigh = 80, thresholdLow = 60, relative = TRUE))

solo_gradient_box(
  value = 40, former = 50,
  thresholdHigh = 105, thresholdLow = 95, relative = TRUE) %>%
  grepl("danger", .) %>%
  expect_true()

solo_gradient_box(
  value = 40, former = 40,
  thresholdHigh = 105, thresholdLow = 95, relative = TRUE) %>%
  grepl("warning", .) %>%
  expect_true()

solo_gradient_box(
  value = 40, former = 35,
  thresholdHigh = 105, thresholdLow = 95, relative = TRUE) %>%
  grepl("success", .) %>%
  expect_true()

expect_warning(solo_gradient_box(
  value = 40, former = 35,
  thresholdHigh = 80, thresholdLow = 95, relative = TRUE))


solo_gradient_box(value = 40, relative = TRUE) %>%
  expect_error()

solo_gradient_box(value = 40, former = 35, thresholdHigh = 90, relative = TRUE) %>%
  expect_warning()

solo_gradient_box(value = 40, former = 35,
                  thresholdHigh = 105, thresholdLow = 90, relative = TRUE) %>%
  expect_warning()


# -------------------------------------------------------------------------

context('pretty')

solo_gradient_box(
  value = 40345.13124123, former = 35,
  thresholdHigh = 80, thresholdLow = 95, pretty = ",") %>%
  grepl("40,345\n", .) %>%
  expect_true()

solo_gradient_box(
  value = 445.13124123, former = 35,
  thresholdHigh = 80, thresholdLow = 95, pretty = ",") %>%
  grepl("445.1\n", .) %>%
  expect_true()

solo_gradient_box(
  value = 4445.13124123, former = 35,
  thresholdHigh = 80, thresholdLow = 95, pretty = " ") %>%
  grepl("4 445\n", .) %>%
  expect_true()

solo_gradient_box(
  value = 4445.13124123, former = 35,
  thresholdHigh = 80, thresholdLow = 95, pretty = ".") %>%
  grepl("4.445\n", .) %>%
  expect_true() %>%
  expect_warning()

test_that("currency units appear before value", {
  # Test that $ appears before the value
  expect_equal(solo_box(value = 42, txt = "Price", units = "$") %>%
    unlist() %>%
    paste(collapse = " ") %>%
    grepl("\\$42", x = .) %>%
    sum(), 1)

  # Test that £ appears before the value
  expect_equal(solo_box(value = 100, txt = "Cost", units = "£") %>%
    unlist() %>%
    paste(collapse = " ") %>%
    grepl("£100", x = .) %>%
    sum(), 1)

  # Test that non-currency units still appear after the value
  expect_equal(solo_box(value = 42, txt = "Weight", units = "kg") %>%
    unlist() %>%
    paste(collapse = " ") %>%
    grepl("42.*kg", x = .) %>%
    sum(), 1)

  # Test solo_gradient_box with currency
  expect_equal(solo_gradient_box(value = 50, txt = "Revenue", units = "$") %>%
    unlist() %>%
    paste(collapse = " ") %>%
    grepl("\\$50", x = .) %>%
    sum(), 1)

  # Test solo_gradient_box with non-currency
  expect_equal(solo_gradient_box(value = 75, txt = "Score", units = "%") %>%
    unlist() %>%
    paste(collapse = " ") %>%
    grepl("75.*%", x = .) %>%
    sum(), 1)
})

iris_shared <- crosstalk::SharedData$new(iris)
# devtools::install_github("kent37/summarywidget")
sw <- summarywidget::summarywidget(iris_shared)

solo_box(value = sw, test = T)
