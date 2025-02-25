% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_semaine5.R
\name{summary_departement}
\alias{summary_departement}
\title{Résumé des informations d'un département}
\usage{
summary_departement(x)
}
\arguments{
\item{x}{data.frame représentant un département.}
}
\value{
Aucune valeur retournée. La fonction affiche directement le résumé dans la console.
}
\description{
Cette fonction prend un dataframe représentant un département et affiche un résumé des informations, y compris :
\itemize{
\item Le nombre de communes
\item Le nombre d'élus
\item La distribution des âges
\item Les élus les plus jeunes et les plus âgés
}
}
\examples{
summary_departement(df_Gers)
}
