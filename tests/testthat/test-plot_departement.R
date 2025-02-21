test_that("plot_departement génère un graphique sans erreur", {
  expect_no_error(plot_departement(df_Gers))
})

test_that("plot_departement échoue si le dataframe n'a pas assez de colonnes", {
  df_incomplet <- df_Gers[, 1:5]  # Supprime des colonnes
  expect_error(plot_departement(df_incomplet), "Le dataframe doit contenir 16 colonnes")
})
