% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_extract_spec.R
\name{data_extract_spec}
\alias{data_extract_spec}
\title{Data extract input for \code{teal} modules}
\usage{
data_extract_spec(dataname, select = NULL, filter = NULL, reshape = FALSE)
}
\arguments{
\item{dataname}{(\code{character})
The name of the dataset to be extracted.}

\item{select}{(\code{NULL} or \code{select_spec}-S3 class or \code{delayed_select_spec})
Columns to be selected from the input dataset mentioned in \code{dataname}.
The setup can be created using \link{select_spec} function.}

\item{filter}{(\code{NULL} or \code{filter_spec} or its respective delayed version)
Setup of the filtering of key columns inside the dataset.
This setup can be created using the \link{filter_spec} function.
Please note that if both select and filter are set to \code{NULL}, then the result
will be a filter spec UI with all variables as possible choices and a select
spec with multiple set to \code{TRUE}.}

\item{reshape}{(\code{logical})
whether reshape long to wide.
Note that it will be used only in case of long dataset with multiple
keys selected in filter part.}
}
\value{
\code{data_extract_spec} object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

The Data extract input can be used to filter and select columns from a data set.
This function enables such an input in \code{teal}.
Please use the constructor function \link{data_extract_spec} to set it up.
}
\note{
No checks based on columns can be done because the data is only referred to by name.
}
\section{Module Development}{

\code{teal.transform} uses this object to construct a UI element in a module.
}

\examples{
adtte_filters <- filter_spec(
  vars = c("PARAMCD", "CNSR"),
  sep = "-",
  choices = c("OS-1" = "OS-1", "OS-0" = "OS-0", "PFS-1" = "PFS-1"),
  selected = "OS-1",
  multiple = FALSE,
  label = "Choose endpoint and Censor"
)

data_extract_spec(
  dataname = "ADTTE",
  filter = adtte_filters,
  select = select_spec(
    choices = c("AVAL", "BMRKR1", "AGE"),
    selected = c("AVAL", "BMRKR1"),
    multiple = TRUE,
    fixed = FALSE,
    label = "Column"
  )
)

data_extract_spec(
  dataname = "ADSL",
  filter = NULL,
  select = select_spec(
    choices = c("AGE", "SEX", "USUBJID"),
    selected = c("SEX"),
    multiple = FALSE,
    fixed = FALSE
  )
)
data_extract_spec(
  dataname = "ADSL",
  filter = filter_spec(
    vars = variable_choices("ADSL", subset = c("AGE"))
  )
)

dynamic_filter <- filter_spec(
  vars = choices_selected(variable_choices("ADSL"), "COUNTRY"),
  multiple = TRUE
)
data_extract_spec(
  dataname = "ADSL",
  filter = dynamic_filter
)
}
\references{
\link{select_spec} \link{filter_spec}
}
