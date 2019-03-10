\name{operatingChar}
\alias{operatingChar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluates Operating Characteristics for Adaptive Group-Sequential Design
}
\description{
Evaluates Operating Characteristics for Adaptive Group-Sequential Design with Population Enrichment in Phase 3 Randomized Controlled Trials with Co-primary Endpoints
}
\usage{
operatingChar(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{Type 1 error rate}
  \item{information.fraction}{Information fraction}
  \item{th1}{theta1}
  \item{th2}{theta2}
  \item{int.l1}{Initial value for stage 1 lower limit}
  \item{int.u1}{Initial value for stage 1 upper limit}
  \item{int.l2}{Initial value for stage 2 lower limit}
  \item{int.u2}{Initial value for stage 2 upper limit}
  \item{f1}{Group 1 proportion}
}

\value{
\item{Stage1.OC}{Stage 1 operating charactistics}
\item{Stage2.OC}{Stage 2 operating charactistics}
\item{Overall.OC}{Overall operating charactistics}
}

\author{
Jaejoon Song, Arup K Sinha
}


\examples{

library(agseDesign)
# Evaluating operating characteristics for theta = (0,0,0,0) scenario
d_0_0_0_0 <- operatingChar(alpha=0.05,
                           information.fraction=0.5,
                           f1=0.25,
                           th1=c(0,0),
                           th2=c(0,0),
                           int.l1=1,
                           int.u1=2.0,
                           int.l2=1,
                           int.u2=1)
d_0_0_0_0
\dontrun{
# Evaluating operating characteristics for theta = (0,0,1,0) scenario
d_0_0_1_0 <- operatingChar(alpha=0.05,
                           information.fraction=0.5,
                           f1=0.25,
                           th1=c(0,0),
                           th2=c(1,0),
                           int.l1=1,
                           int.u1=2.0,
                           int.l2=1,
                           int.u2=1)

d_0_0_1_0


# Evaluating operating characteristics for theta = (1,1,0,0) scenario
d_1_1_0_0 <- operatingChar(alpha=0.05,
                           information.fraction=0.5,
                           f1=0.25,
                           th1=c(1,1),
                           th2=c(0,0),
                           int.l1=1,
                           int.u1=2.0,
                           int.l2=1,
                           int.u2=1)

d_1_1_0_0

# Evaluating operating characteristics for theta = (1,1,1,0) scenario
d_1_1_1_0 <- operatingChar(alpha=0.05,
                           information.fraction=0.5,
                           f1=0.25,
                           th1=c(1,1),
                           th2=c(1,0),
                           int.l1=1,
                           int.u1=2.0,
                           int.l2=1,
                           int.u2=1)

d_1_1_1_0
}

}

\keyword{ operating characteristics}
