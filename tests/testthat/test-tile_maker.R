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
    color = "#FFC107", txt = c("times", "reports")
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
  # Test that solo_gradient_box uses pastel colors instead of bootstrap classes
  expect_equal(solo_gradient_box(value = 10, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("#FFCDD2", x = .) %>%  # pastel red
    sum(), 1)
  expect_equal(solo_gradient_box(value = 80, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("#FFF9C4", x = .) %>%  # pastel yellow
    sum(), 1)
  expect_equal(solo_gradient_box(value = 95, txt = "blah", former = 5) %>%
    unlist() %>%
    grepl("#C8E6C9", x = .) %>%  # pastel green
    sum(), 1)
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
    color = "#FFC107",
    txt = c("times", "reports")
  ) %>%
    class() %>%
    expect_equal(., "shiny.tag")

  multi_box(
    values = c(3, 45), title = "Important <br>button",
    number_zoom = 300, color = "#FFC107",
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
    number_zoom = 300, icons = c("apple", "calendar"), color = "#FFC107"
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
  grepl("#FFCDD2", .) %>%  # pastel red
  expect_true()

solo_gradient_box(
  value = 40, former = 40,
  thresholdHigh = 105, thresholdLow = 95, relative = TRUE) %>%
  grepl("#FFF9C4", .) %>%  # pastel yellow
  expect_true()

solo_gradient_box(
  value = 40, former = 35,
  thresholdHigh = 105, thresholdLow = 95, relative = TRUE) %>%
  grepl("#C8E6C9", .) %>%  # pastel green
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

test_that("width_percent and height_percent parameters work correctly", {
  # Test solo_box with width_percent as number
  box1 <- solo_box(value = 42, txt = "Test", width_percent = 50)
  box1_str <- unlist(box1) %>% paste(collapse = " ")
  expect_equal(grepl("width: 50%;", box1_str), TRUE)
  
  # Test solo_box with width_percent as string with %
  box2 <- solo_box(value = 42, txt = "Test", width_percent = "32%")
  box2_str <- unlist(box2) %>% paste(collapse = " ")
  expect_equal(grepl("width: 32%;", box2_str), TRUE)
  
  # Test solo_box with height_percent as number
  box3 <- solo_box(value = 42, txt = "Test", height_percent = 75)
  box3_str <- unlist(box3) %>% paste(collapse = " ")
  expect_equal(grepl("height: 75%;", box3_str), TRUE)
  
  # Test solo_box with both width_percent and height_percent
  box4 <- solo_box(value = 42, txt = "Test", width_percent = 33, height_percent = 50)
  box4_str <- unlist(box4) %>% paste(collapse = " ")
  expect_equal(grepl("width: 33%;", box4_str), TRUE)
  expect_equal(grepl("height: 50%;", box4_str), TRUE)
  
  # Test solo_gradient_box with width_percent
  box5 <- solo_gradient_box(value = 75, txt = "Gradient", width_percent = 25, target = 100)
  box5_str <- unlist(box5) %>% paste(collapse = " ")
  expect_equal(grepl("width: 25%;", box5_str), TRUE)
  
  # Test multi_box with height_percent
  box6 <- multi_box(values = c(1, 2), txt = c("A", "B"), height_percent = "40%")
  box6_str <- unlist(box6) %>% paste(collapse = " ")
  expect_equal(grepl("height: 40%;", box6_str), TRUE)
  
  # Test solo_box_ct with both parameters
  box7 <- solo_box_ct(value = 10, txt = "CT Test", width_percent = 48, height_percent = 60)
  box7_str <- unlist(box7) %>% paste(collapse = " ")
  expect_equal(grepl("width: 48%;", box7_str), TRUE)
  expect_equal(grepl("height: 60%;", box7_str), TRUE)
  
  # Test that existing link styling is preserved when using percentages
  box8 <- solo_box(value = 42, txt = "Test", width_percent = 50, link = "http://example.com")
  box8_str <- unlist(box8) %>% paste(collapse = " ")
  expect_equal(grepl("cursor: pointer;", box8_str), TRUE)
  expect_equal(grepl("width: 50%;", box8_str), TRUE)
})

test_that("width_percent and height_percent parameters are optional", {
  # Test that functions work without the new parameters (backward compatibility)
  box1 <- solo_box(value = 42, txt = "Test")
  expect_equal(class(box1), "shiny.tag")
  
  box2 <- solo_gradient_box(value = 75, target = 100)
  expect_equal(class(box2), "shiny.tag")
  
  box3 <- multi_box(values = c(1, 2), txt = c("A", "B"))
  expect_equal(class(box3), "shiny.tag")
  
  box4 <- solo_box_ct(value = 10, txt = "Test")
  expect_equal(class(box4), "shiny.tag")
})

test_that("simplified color API works correctly", {
  # Test that default color changed to "#DDF4FF"
  box1 <- solo_box(value = 42, txt = "Test")
  box1_str <- unlist(box1) %>% paste(collapse = " ")
  expect_equal(grepl("background-color: #DDF4FF", box1_str), TRUE)
  expect_equal(grepl("panel-default", box1_str), TRUE)  # Should always use panel-default
  
  # Test custom hex color support
  box2 <- solo_box(value = 42, txt = "Test", color = "#FF5733")
  box2_str <- unlist(box2) %>% paste(collapse = " ")
  expect_equal(grepl("background-color: #FF5733", box2_str), TRUE)
  expect_equal(grepl("panel-default", box2_str), TRUE)  # Should use default panel
  
  # Test text color support
  box3 <- solo_box(value = 42, txt = "Test", text_color = "white")
  box3_str <- unlist(box3) %>% paste(collapse = " ")
  expect_equal(grepl("color: white", box3_str), TRUE)
  
  # Test multi_box with new defaults
  multi1 <- multi_box(values = c(1, 2), txt = c("A", "B"))
  multi1_str <- unlist(multi1) %>% paste(collapse = " ")
  expect_equal(grepl("background-color: #DDF4FF", multi1_str), TRUE)
  expect_equal(grepl("panel-default", multi1_str), TRUE)
  
  # Test solo_box_ct with new defaults
  ct1 <- solo_box_ct(value = 10, txt = "Test")
  ct1_str <- unlist(ct1) %>% paste(collapse = " ")
  expect_equal(grepl("background-color: #DDF4FF", ct1_str), TRUE)
  expect_equal(grepl("panel-default", ct1_str), TRUE)
})

iris_shared <- crosstalk::SharedData$new(iris)
# devtools::install_github("kent37/summarywidget")
sw <- summarywidget::summarywidget(iris_shared)

solo_box(value = sw, test = T)
