library(usethis)

elus_data <- readr::read_delim("C:/Users/arthu/OneDrive/Bureau/Master/R avancé/semaine_5_td/elus_sample.csv")

df_Nantes <- subset(elus_data, elus_data$`Libellé de la commune` %in% c("Nantes") & elus_data$`Code de la commune` %in% c(44109))

df_Gers <- subset(elus_data, elus_data$`Libellé du département` %in% c("Gers"))

usethis::use_data(elus_data, overwrite = TRUE)

usethis::use_data(df_Nantes, overwrite = TRUE)

usethis::use_data(df_Gers, overwrite = TRUE)

