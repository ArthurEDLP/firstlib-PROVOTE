#' Compter le nombre d'adjoints dans un dataframe
#'
#' @description
#' Cette fonction prend en entrée un dataframe et renvoie le nombre d'adjoints présents dans ce dataframe.
#' Elle s'assure que la colonne 'Libellé de la fonction' existe avant de compter le nombre d'adjoints.
#' @param df Un dataframe représentant une commune ou un département.
#' @return Un entier correspondant au nombre d'adjoints dans le dataframe.

compter_nombre_d_adjoints <- function(df){

  if (length(df) != 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }

  colonnes_requises <- c("Libellé de la fonction")
  if (!all(colonnes_requises %in% colnames(df))) {
    stop("Le dataframe doit contenir la colonne : Libellé de la fonction")
  }
  # renvoie un vecteur avec TRUE ou FALSE en fonction de l'apparition ou non
  adjoint_TRUE <- grepl("adjoint", df$Libellé.de.la.fonction)

  Nbre_adjoint <- df[adjoint_TRUE, ]

  return(nrow(Nbre_adjoint))

}


#' Trouver l'élu le plus âgé dans un dataframe
#'
#' @description
#' Cette fonction prend en entrée un dataframe et renvoie les informations (nom, prénom et date de naissance)
#' de l'élu le plus âgé. Le dataframe doit contenir une colonne de dates de naissance au format 'dd/mm/yyyy'.
#' @param df Un dataframe représentant une commune ou un département.
#' @return Un vecteur avec le nom, prénom et la date de naissance de l'élu le plus âgé.

trouver_l_elu_le_plus_age <- function(df){

  if (length(df) != 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }

  df$`Date de naissance` <- as.Date(df$`Date de naissance`, format = "%d/%m/%Y")

  df[which.min(df$`Date de naissance`),c("Nom de l'élu", "Prénom de l'élu", "Date de naissance")]

}

#' Une fonction pour avoir la distribution de l'âge dans un data.frame
#'
#' @return Renvoie un vecteur avec les différents quartiles de l'âge de mon df: 0%, 25%, 50%, 75%, 100%
#' @description
#' Prend en compte un data frame et renvoie les quartiles d'âge du df
#' @param df data.frame d'une commune ou département

calcul_distribution_age <- function(df){

  if (length(df) < 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }


  df$`Date de naissance` <- as.Date(df$`Date de naissance`, format = "%d/%m/%Y")

  df$Age <- as.numeric(difftime(Sys.Date(), df$`Date de naissance`, units = "weeks")) %/% 52.25
  # Sys.date pour la date du jour, difftime() pour comparer 2 dates dans l'unité voulu,
  # division entière par le nbre moyen de semaines

  quantile(df$Age, probs = c(0, 0.25, 0.5, 0.75, 1))

}

#' Une fonction pour avoir un graphique du nombre des différents codes professionels
#'
#' @return Renvoie un graphique en barre horizontal de la fréquence des différents codes professionels
#' @description
#' Prend en compte un data frame, ne garde que les élus et leurs codes pro puis renvoie un graphique en barre horizontal
#' @param df data.frame d'une commune ou département

plot_code_professions <- function(df){

  if (length(df) != 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }
  # je ne séléctionne que les lignes avec Maire(ex: maire-adjoint, Maire, ...)
  Nbre_Maire_ad <- grepl("Maire", df$Libellé.de.la.fonction, fixed  = TRUE)
  # je créer un nouveau data frame qui ne prendra que les occurences précédentes
  df_Maire_ad <- df[Nbre_Maire_ad,]

  Nbre_Maire_d <- !grepl("adjoint", df_Maire_ad$Libellé.de.la.fonction, fixed  = TRUE)

  df_Maire_d <- df_Maire_ad[Nbre_Maire_d,]

  Nbre_Maire <- !grepl("délégué", df_Maire_d$Libellé.de.la.fonction, fixed  = TRUE)

  df_Maire <- df_Maire_d[Nbre_Maire,]

  print(
    barplot(
      table(df_Maire$`Code de la catégorie socio professionnelle`),
      horiz = TRUE,
      xlab = "fréquence d'apparition d'élu de la catégorie socio",
      ylab = "code catégorie socio",
      main = paste("Département:", df[1, 2])

    )
  )

}

#' Résumé des informations d'une commune
#'
#' @description
#' Cette fonction prend un dataframe représentant une commune et affiche un résumé des informations, y compris :
#' - Le nombre d'élus
#' - La distribution des âges
#' - L'élu le plus âgé
#'
#' @param x data.frame représentant une commune.
#' @return Aucune valeur retournée. La fonction affiche directement le résumé dans la console.
#' @export
#' @examples
#' summary_commune(df_Nantes)
summary_commune <- function(x){

  if (length(table(x$`Libellé de la commune`)) != 1) {
    stop("L'objet doit contenir 1 communes")
  }


  nom_de_la_commune <- as.character(x[1,6])

  # je sors un vect avec TRUE pour chaque occurence de Maire et je compte nbre TRUE
  Nbre_elu <- sum(grepl("Maire", x$Libellé.de.la.fonction, fixed  = TRUE))

  nom_vieux <- as.character(trouver_l_elu_le_plus_age(x)[1, 1]) # cat n'aime pas les listes
  age_vieux <- as.character(calcul_distribution_age(x)[5])

  distribution_age <- calcul_distribution_age(x)

  cat("Nom de la commune: ",nom_de_la_commune, "\n")
  cat("Nombre d'élus: ",Nbre_elu, "\n")
  cat("Distribution des âges: ", "min", distribution_age[1],
      ", 25% à", distribution_age[2],
      ", 50% à", distribution_age[3],
      ", 75% à", distribution_age[4],
      ", 100% à", distribution_age[5],"\n")
  cat("Nom et âge de l'élu le plus ancien: ",nom_vieux, age_vieux,"ans", "\n", "\n")
}

#' Résumé des informations d'un département
#'
#' @description
#' Cette fonction prend un dataframe représentant un département et affiche un résumé des informations, y compris :
#' - Le nombre de communes
#' - Le nombre d'élus
#' - La distribution des âges
#' - Les élus les plus jeunes et les plus âgés
#'
#' @param x data.frame représentant un département.
#' @return Aucune valeur retournée. La fonction affiche directement le résumé dans la console.
#' @export
#' @examples
#' summary_departement(df_Gers)
summary_departement <- function(x){

  if (length(table(x$`Libellé de la commune`)) == 1) {
    stop("L'objet doit contenir plus d'une commune")
  }

  if (length(table(x$`Libellé du département`)) != 1) {
    stop("L'objet doit contenir 1 département unique")
  }

  nom_du_departement <- as.character(x[1,2])

  nbre_communes <- length(table(x$`Libellé de la commune`))

  Nbre_elu <- sum(grepl("Maire", x$Libellé.de.la.fonction, fixed  = TRUE))

  vect_distribution_age <- calcul_distribution_age(x)

  x$`Date de naissance` <- as.Date(x$`Date de naissance`, format = "%d/%m/%Y")
  x$Age <- as.numeric(difftime(Sys.Date(), x$`Date de naissance`, units = "weeks")) %/% 52.25

  jeune <- x[which.min(x$Age), c("Nom de l'élu", "Age", "Libellé de la commune")]
  jeune_nom <- as.character(jeune[1, 1])
  jeune_Age <- as.character(jeune[1, 2])
  jeune_commune <- as.character(jeune[1, 3])

  vieux <- x[which.max(x$Age), c("Nom de l'élu", "Age", "Libellé de la commune")]
  vieux_nom <- as.character(vieux[1, 1])
  vieux_Age <- as.character(vieux[1, 2])
  vieux_commune <- as.character(vieux[1, 3])

  # renvoie un df avec les moyennes par communes
  moyenne_par_commune <- aggregate(Age ~ `Libellé de la commune`, data = x, FUN = mean)
  haute_moy_age <- moyenne_par_commune[which.max(moyenne_par_commune$Age),]
  faible_moy_age <- moyenne_par_commune[which.min(moyenne_par_commune$Age),]

  commune_haute <- x[grepl(haute_moy_age[1, 1], x$`Libellé de la commune`), ]
  distribution_age_haute <- calcul_distribution_age(commune_haute)

  commune_faible <- x[grepl(faible_moy_age[1, 1], x$`Libellé de la commune`), ]
  distribution_age_faible <- calcul_distribution_age(commune_faible)

  cat("Nom du département:", nom_du_departement, "\n",
      "Nombre de communes: ", nbre_communes, "\n",
      "Nombre d'élus: ", Nbre_elu, "\n")

  cat("Distribution des âges: ", "min", vect_distribution_age[1],
      ", 25% à", vect_distribution_age[2],
      ", 50% à", vect_distribution_age[3],
      ", 75% à", vect_distribution_age[4],
      ", 100% à", vect_distribution_age[5],"\n",
      "le plus ancien:", vieux_nom, "\n", vieux_Age,"ans", "\n", vieux_commune, "\n",
      "le plus récent:", jeune_nom, "\n", jeune_Age,"ans", "\n", jeune_commune, "\n")

  cat("Commune avec plus haute moyenne d'âge: ",haute_moy_age[1, 1], round(haute_moy_age[1, 2]), "ans", "\n",
      "Distribution des âges: ", "min", distribution_age_haute[1],
      ", 25% à", distribution_age_haute[2],
      ", 50% à", distribution_age_haute[3],
      ", 75% à", distribution_age_haute[4],
      ", 100% à", distribution_age_haute[5],"\n")

  cat("Commune avec plus faible moyenne d'âge: ",faible_moy_age[1, 1], round(faible_moy_age[1, 2]), "ans", "\n",
      "Distribution des âges: ", "min", distribution_age_faible[1],
      ", 25% à", distribution_age_faible[2],
      ", 50% à", distribution_age_faible[3],
      ", 75% à", distribution_age_faible[4],
      ", 100% à", distribution_age_faible[5],
      "\n", "\n"
  )


}


plot_commune <- function(df){

  if (length(df) < 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }
  # je ne séléctionne que les lignes avec Maire(ex: maire-adjoint, Maire, ...)
  Nbre_Maire_ad <- grepl("Maire", df$Libellé.de.la.fonction, fixed  = TRUE)
  # je créer un nouveau data frame qui ne prendra que les occurences précédentes
  df_Maire_ad <- df[Nbre_Maire_ad,]

  Nbre_Maire_d <- !grepl("adjoint", df_Maire_ad$Libellé.de.la.fonction, fixed  = TRUE)

  df_Maire_d <- df_Maire_ad[Nbre_Maire_d,]

  Nbre_Maire <- !grepl("délégué", df_Maire_d$Libellé.de.la.fonction, fixed  = TRUE)

  df_Maire <- df_Maire_d[Nbre_Maire,]

  print(
    barplot(
      table(df_Maire$`Code.de.la.catégorie.socio-professionnelle`),
      horiz = TRUE,
      xlab = "frqc d'apparition d'élu de la catégorie socio",
      ylab = "Libellé des codes professionnels pour les élus",
      main = paste("Commune: ",df[1, 6], "Département:", df[1, 2])

    )
  )

}


plot_departement <- function(df){

  if (length(df) < 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }
  # je ne séléctionne que les lignes avec Maire(ex: maire-adjoint, Maire, ...)
  Nbre_Maire_ad <- grepl("Maire", df$Libellé.de.la.fonction, fixed  = TRUE)
  # je créer un nouveau data frame qui ne prendra que les occurences précédentes
  df_Maire_ad <- df[Nbre_Maire_ad,]

  Nbre_Maire_d <- !grepl("adjoint", df_Maire_ad$Libellé.de.la.fonction, fixed  = TRUE)

  df_Maire_d <- df_Maire_ad[Nbre_Maire_d,]

  Nbre_Maire <- !grepl("délégué", df_Maire_d$Libellé.de.la.fonction, fixed  = TRUE)

  df_Maire <- df_Maire_d[Nbre_Maire,]

  Nbre_co <- length(table(df$`Libellé de la commune`))

  print(
    barplot(
      head(table(df_Maire$`Code.de.la.catégorie.socio-professionnelle`), n = 10),
      horiz = TRUE,
      xlab = "frqc d'apparition d'élu de la catégorie socio",
      ylab = "Libellé des 10 codes professionnels les plus représentés pour les départements",
      main = paste("Nombre de communes: ",Nbre_co,
                   "Département:", df[1, 2])

    )
  )

}

creer_commune <- function(df){
  if (length(table(df$`Libellé de la commune`)) != 1) {
    stop("L'objet doit contenir 1 commune unique")
  }

  class(df) <- "Commune"
  return(df)
}

creer_departement <- function(df){
  if (length(table(df$`Libellé du département`)) != 1) {
    stop("L'objet doit contenir 1 département unique")
  }

  if (length(table(df$`Libellé de la commune`)) == 1) {
    stop("L'objet doit contenir plus d'une commune")
  }

  class(df) <- "Département"
  return(df)
}

