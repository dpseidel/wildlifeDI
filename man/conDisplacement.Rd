% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conDisplacement.R
\name{conDisplacement}
\alias{conDisplacement}
\title{Calculate net displacement from contacts}
\usage{
conDisplacement(ltraj, def = "all", idcol = "burst")
}
\arguments{
\item{ltraj}{an object of the class \code{ltraj} which should be output from the function \code{conPhase}.}

\item{def}{how to define the point-of-contact. The default is to define it as all fixes in a phase \code{type = 'all'}, alternatively contacts can be defined as a single point along the phase defined as one of: \code{'first','last','minDist','minTime'}, which corresponds to the first fix int he contact phase, the last fix in the contact phase, the fix with the minimum time difference and the fix with the closest contact distance.}

\item{idcol}{column id associated with IDs of individuals, default is the 'burst'}
}
\value{
An ltraj object with a new 'displacement' column in infolocs.
}
\description{
Calculate the net-displacement (distance) of fixes before and after a contact from that contact point.
}
\details{
This function is used to compute the net displacement away from contacts by an animal before and after a contact phase. Net displacement represents an important contextual variable, related to the mobility of the individual.
}
\examples{
\dontrun{
data(does)
doecons <- conProcess(does,tc=15*60,dc=50)
doephas <- conPhase(doecons,pc=60*60)
disp_f <- conDisplacement(doephas,def='first')
disp_l <- conDisplacement(doephas,def='last')
}

}
\seealso{
conPhase, conContext
}
\keyword{contacts}
