context("test-clickable-links")

test_that("panels without links don't have anchor tags", {
  # Test solo_box without link
  box_no_link <- solo_box(value = 42, txt = "Test", color = "info")
  html_string <- as.character(box_no_link)
  
  # Should not contain <a> tag
  expect_false(grepl("<a", html_string))
  # Should contain panel div
  expect_true(grepl("panel", html_string))
  # Should not contain button tag
  expect_false(grepl("<button", html_string))
  
  # Test solo_gradient_box without link
  grad_box_no_link <- solo_gradient_box(value = 80, txt = "Test")
  html_string_grad <- as.character(grad_box_no_link)
  
  # Should not contain <a> tag
  expect_false(grepl("<a", html_string_grad))
  # Should contain panel div
  expect_true(grepl("panel", html_string_grad))
  # Should not contain button tag
  expect_false(grepl("<button", html_string_grad))
  
  # Test solo_box_ct without link
  box_ct_no_link <- solo_box_ct(value = 42, txt = "Test", color = "info")
  html_string_ct <- as.character(box_ct_no_link)
  
  # Should not contain <a> tag
  expect_false(grepl("<a", html_string_ct))
  # Should contain panel div
  expect_true(grepl("panel", html_string_ct))
  # Should not contain button tag
  expect_false(grepl("<button", html_string_ct))
  
  # Test multi_box without link
  multi_no_link <- multi_box(values = c(1, 2), txt = c("A", "B"), color = "info")
  html_string_multi <- as.character(multi_no_link)
  
  # Should not contain <a> tag
  expect_false(grepl("<a", html_string_multi))
  # Should contain panel div
  expect_true(grepl("panel", html_string_multi))
  # Should not contain button tag
  expect_false(grepl("<button", html_string_multi))
})

test_that("panels with links do have anchor tags", {
  # Test solo_box with link
  box_with_link <- solo_box(value = 42, txt = "Test", color = "info", link = "https://example.com")
  html_string <- as.character(box_with_link)
  
  # Should contain <a> tag with href
  expect_true(grepl("<a", html_string))
  expect_true(grepl("href.*=.*https://example.com", html_string))
  # Should contain panel div
  expect_true(grepl("panel", html_string))
  # Should not contain button tag
  expect_false(grepl("<button", html_string))
  
  # Test solo_gradient_box with link
  grad_box_with_link <- solo_gradient_box(value = 80, txt = "Test", link = "https://example.com")
  html_string_grad <- as.character(grad_box_with_link)
  
  # Should contain <a> tag with href
  expect_true(grepl("<a", html_string_grad))
  expect_true(grepl("href.*=.*https://example.com", html_string_grad))
  # Should contain panel div
  expect_true(grepl("panel", html_string_grad))
  # Should not contain button tag
  expect_false(grepl("<button", html_string_grad))
  
  # Test solo_box_ct with link
  box_ct_with_link <- solo_box_ct(value = 42, txt = "Test", color = "info", link = "https://example.com")
  html_string_ct <- as.character(box_ct_with_link)
  
  # Should contain <a> tag with href
  expect_true(grepl("<a", html_string_ct))
  expect_true(grepl("href.*=.*https://example.com", html_string_ct))
  # Should contain panel div
  expect_true(grepl("panel", html_string_ct))
  # Should not contain button tag
  expect_false(grepl("<button", html_string_ct))
  
  # Test multi_box with link
  multi_with_link <- multi_box(values = c(1, 2), txt = c("A", "B"), color = "info", link = "https://example.com")
  html_string_multi <- as.character(multi_with_link)
  
  # Should contain <a> tag with href
  expect_true(grepl("<a", html_string_multi))
  expect_true(grepl("href.*=.*https://example.com", html_string_multi))
  # Should contain panel div
  expect_true(grepl("panel", html_string_multi))
  # Should not contain button tag
  expect_false(grepl("<button", html_string_multi))
})

test_that("empty string link is treated as no link", {
  # Test solo_box with empty string link
  box_empty_link <- solo_box(value = 42, txt = "Test", color = "info", link = "")
  html_string <- as.character(box_empty_link)
  
  # Should not contain <a> tag when link is empty string
  expect_false(grepl("<a", html_string))
  # Should contain panel div
  expect_true(grepl("panel", html_string))
  # Should not contain button tag
  expect_false(grepl("<button", html_string))
})