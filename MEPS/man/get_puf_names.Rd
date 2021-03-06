% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_puf_names.R
\name{get_puf_names}
\alias{get_puf_names}
\title{Get MEPS Public Use File Names}
\usage{
get_puf_names(year, type, web = T)
}
\arguments{
\item{year}{(optional) Data year, between 1996 and most current PUF release. If omitted, files from all years will be returned}

\item{type}{(optional) File type of desired MEPS file. Options are 'PIT' (Point-in-time file), 'FYC' (Full-year consolidated), 'Conditions' (Conditions file), 'Jobs' (Jobs file), 'PRPL' (Person-Round-Plan), 'PMED' (Prescription Medicines Events), 'DV' (Dental Visits), 'OM' (Other medical events), 'IP' (Inpatient Stays), 'ER' (Emergency Room Visits), 'OP' (Outpatient Visits), 'OB' (Office-based visits), 'HH' (Home health), 'CLNK' (conditions-event link file), 'RXLK' (PMED - events link file), and 'PMED.Multum' (Multum Lexicon addendum files for 1996-2013)}

\item{web}{if TRUE, returns names of .zip files from web, otherwise, returns names of .ssp files after download}
}
\description{
This is a lookup function that returns a single requested file name or list of names for specified MEPS data file. Internet access is required, since the function reads from the HHS-AHRQ GitHub page.
}
\examples{
## Get file name for full-year-consolidated (FYC) file from 2005
get_puf_names(2005,'FYC')

## Get file names for all PUFs in 2014
get_puf_names(2014)

## Get file names for PMED event files, all years
get_puf_names(type='PMED')

## Return all files, all years
get_puf_names()

## Compare names of .ssp files with those on website links
get_puf_names(year = 1996, type = 'DV')
get_puf_names(year = 1996, type = 'DV', web=F)
}
