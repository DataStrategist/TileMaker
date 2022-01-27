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

context("relative mode")

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
