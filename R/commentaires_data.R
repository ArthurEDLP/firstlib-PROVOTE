library(usethis)

#  Instead of documenting the data directly, you document the name of the dataset and save it in R/.

#' Données sur les élus
#'
#' @description
#' Un sous-ensemble de données sur les élus, extraites d'un fichier CSV
#' contenant des informations telles que le nom, l'âge, la profession.
#'
#' @format ## `elus_data`
#' Un data frame avec 16 lignes et 9752 colonnes :
#' \describe{
#'   \item{Code.du.département}{Le nombre attribué à un département}
#'   \item{Libellé du département}{Le nom du département}
#'   \item{Nom de l'élu}{Le nom de l'élu}
#'   \item{Prénom de l'élu}{Le prénom de l'élu}
#'   \item{Code sexe}{M si Masculin et F si Feminin}
#'   \item{Date de naissance}{La date de naissance au format dd/mm/aaaa}
#'   ...
#' }
#' @source Données extraites de "elus_sample.csv"
"elus_data"

#' Données sur les élus de la commune de Nantes
#'
#' @description
#' Un sous-ensemble de données sur les élus de Nantes, extraites d'un fichier CSV
#' contenant des informations telles que le nom, l'âge, la profession. Utilisation pour les exemples.
#'
#' @format ## `df_Nantes`
#' Un data frame avec 16 lignes et 66 colonnes :
#' \describe{
#'   \item{Code.du.département}{Le nombre attribué à un département}
#'   \item{Libellé du département}{Le nom du département}
#'   \item{Nom de l'élu}{Le nom de l'élu}
#'   \item{Prénom de l'élu}{Le prénom de l'élu}
#'   \item{Code sexe}{M si Masculin et F si Feminin}
#'   \item{Date de naissance}{La date de naissance au format dd/mm/aaaa}
#'   ...
#' }
#' @source Données extraites de "elus_data"
"df_Nantes"

#' Données sur les élus du département du Gers
#'
#' @description
#' Un sous-ensemble de données sur les élus du Gers, extraites d'un fichier CSV
#' contenant des informations telles que le nom, l'âge, la profession. Utilisation pour les exemples.
#'
#' @format ## `df_Gers`
#' Un data frame avec 16 lignes et 4932 colonnes :
#' \describe{
#'   \item{Code.du.département}{Le nombre attribué à un département}
#'   \item{Libellé du département}{Le nom du département}
#'   \item{Nom de l'élu}{Le nom de l'élu}
#'   \item{Prénom de l'élu}{Le prénom de l'élu}
#'   \item{Code sexe}{M si Masculin et F si Feminin}
#'   \item{Date de naissance}{La date de naissance au format dd/mm/aaaa}
#'   ...
#' }
#' @source Données extraites de "elus_data"
"df_Gers"
