% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_semaine5.R
\name{trouver_l_elu_le_plus_age}
\alias{trouver_l_elu_le_plus_age}
\title{Trouver l'élu le plus âgé dans un dataframe}
\usage{
trouver_l_elu_le_plus_age(df)
}
\arguments{
\item{df}{Un dataframe représentant une commune ou un département.}
}
\value{
Un vecteur avec le nom, prénom et la date de naissance de l'élu le plus âgé.
}
\description{
Cette fonction prend en entrée un dataframe et renvoie les informations (nom, prénom et date de naissance)
de l'élu le plus âgé. Le dataframe doit contenir une colonne de dates de naissance au format 'dd/mm/yyyy'.
}
