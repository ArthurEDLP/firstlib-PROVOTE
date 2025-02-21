test_that("summary_departement génère un résumé sans erreur", {
  expect_no_error(summary_departement(df_Gers))
})

test_that("summary_departement échoue si le data frame contient une seule commune", {
  expect_error(summary_departement(df_Nantes), "L'objet doit contenir plus d'une commune")
})
