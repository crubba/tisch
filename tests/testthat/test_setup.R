context("tisch setup")

test_that("Tisch object is setup correctly", {

  data("browsershares")
  
  tt <- tisch(browsershares, rows = c("Year", "Month"), sep = " >> ") + 
    theme(
      row_style = ragged(indent = 0.5),
      columns_style = hierarchical(),
      column_justification = "central",
      replace_NA = "not available",
      text_size = "scriptsize"
    ) + 
    caption("Caption_Test") +
    footnote("Footnote_Test") +
    label("Label_Test")
    
  
  expect_that(tt@annotations$caption, equals("Caption_Test"))
  expect_that(tt@annotations$label, equals("Label_Test"))
  expect_that(tt@annotations$footnote, equals("Footnote_Test"))

})
