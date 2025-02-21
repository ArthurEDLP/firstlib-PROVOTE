test_that("plot_commune génère un graphique sans erreur", {
  expect_no_error(plot_commune(df_Nantes))
})

test_that("plot_commune échoue si le dataframe n'a pas assez de colonnes", {
  df_incomplet <- df_Nantes[, 1:5]  # Supprime des colonnes
  expect_error(plot_commune(df_incomplet), "Le dataframe doit contenir 16 colonnes")
})
