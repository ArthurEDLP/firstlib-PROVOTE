test_that("summary_commune génère un résumé sans erreur", {
  expect_no_error(summary_commune(df_Nantes))
})

test_that("summary_commune échoue si plusieurs communes sont présentes", {
  expect_error(summary_commune(df_Gers), "L'objet doit contenir 1 communes")
})
