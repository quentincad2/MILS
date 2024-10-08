% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redi.R
\name{redi}
\alias{redi}
\title{redi}
\usage{
redi(
  base,
  N1 = 8,
  N2 = 28,
  lambda1 = 2/(8 + 1),
  lambda2 = 2/(28 + 1),
  indic,
  fun,
  formatdate = "\%d/\%m/\%Y",
  replace = F,
  keep,
  FOSTER = F,
  rpeload = NULL
)
}
\arguments{
\item{base}{jeu de données comprenant les variables "Date","Athlete" et les variables de charge.}

\item{N1}{nombre de jours à prendre en compte pour la charge chronique pour l'EWMA}

\item{N2}{nombre de jours à prendre en compte pour la charge aigue pour l'EWMA}

\item{lambda1}{lambda pour la charge aigue pour le REDI}

\item{lambda2}{lambda pour la charge chronique pour le REDI}

\item{indic}{les variables à prendre en compte pour calculer les différents indicateurs}

\item{fun}{si plusieurs sessions le même jour, comment aggreger les données}

\item{formatdate}{format des dates}

\item{replace}{me rappelle plus}

\item{keep}{pas encore en place}

\item{rpeload}{variable indiquant la rpeSession si FOSTER=TRUE}

\item{FORSTER}{calcul les indicateurs de FOSTER}
}
\value{
un data.frame avec les différents indicateurs de charge
}
\description{
calcul les differents indicateurs de charge
}
\examples{
NULL
}
