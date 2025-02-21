test_that("creer_departement assigne correctement la classe Département", {
  result <- creer_departement(df_Gers)
  expect_s3_class(result, "Département")
})

test_that("creer_departement échoue si plusieurs départements sont présents", {
  df_multi_departement <- elus_data
  df_multi_departement$`Libellé du département`
  expect_error(creer_departement(df_multi_departement), "L'objet doit contenir 1 département unique")
})

test_that("creer_departement échoue si une unique commune est présente", {
  df_unique_commune <- df_Nantes
  expect_error(creer_departement(df_unique_commune), "L'objet doit contenir plus d'une commune")
})
