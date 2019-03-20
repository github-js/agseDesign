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
set.seed(123)
# Evaluating operating characteristics for theta = (0,0,0,0) scenario
d_0_0_0_0 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(0,0),
                          th2=c(0,0))
d_0_0_0_0
\dontrun{
# Evaluating operating characteristics for theta = (0,0,1,0) scenario
d_0_0_1_0 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(0,0),
                          th2=c(1,0))
d_0_0_1_0

# Evaluating operating characteristics for theta = (0,0,0,1) scenario
d_0_0_0_1 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(0,0),
                          th2=c(0,1))
d_0_0_0_1

# Evaluating operating characteristics for theta = (1,0,0,0) scenario
d_1_0_0_0 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(1,0),
                          th2=c(0,0))
d_1_0_0_0

# Evaluating operating characteristics for theta = (0,1,0,0) scenario
d_0_1_0_0 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(0,1),
                          th2=c(0,0))
d_0_1_0_0


# Evaluating operating characteristics for theta = (1,1,0,0) scenario
d_1_1_0_0 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(1,1),
                          th2=c(0,0))
d_1_1_0_0


# Evaluating operating characteristics for theta = (1,1,1,0) scenario
d_1_1_1_0 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(1,1),
                          th2=c(1,0))
d_1_1_1_0

# Evaluating operating characteristics for theta = (1,1,0,1) scenario
d_1_1_0_1 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(1,1),
                          th2=c(0,1))
d_1_1_0_1


# Evaluating operating characteristics for theta = (1,0,1,0) scenario
d_1_0_1_0 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(1,0),
                          th2=c(1,0))
d_1_0_1_0


# Evaluating operating characteristics for theta = (1,1,1,1) scenario
d_1_1_1_1 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(1,1),
                          th2=c(1,1))
d_1_1_1_1

# Evaluating operating characteristics for theta = (2,2,0,0) scenario
d_2_2_0_0 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(2,2),
                          th2=c(0,0))
d_2_2_0_0



# Evaluating operating characteristics for theta = (2,2,1,0) scenario
d_2_2_0_1 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(2,2),
                          th2=c(0,1))
d_2_2_0_1



# Evaluating operating characteristics for theta = (2,2,1,0) scenario
d_2_2_1_0 <- operatingChar(alpha=0.025,
                          information.fraction=0.5,
                          f1=0.75,
                          th1=c(2,2),
                          th2=c(1,0))
d_2_2_1_0


options(scipen=999)
manuscriptTable2 <- data.frame(rbind(
c(d_0_0_0_0$effect.size, round(d_0_0_0_0$Overall.OC,4)),
c(d_0_0_1_0$effect.size, round(d_0_0_1_0$Overall.OC,4)),
c(d_0_0_0_1$effect.size, round(d_0_0_0_1$Overall.OC,4)),
c(d_1_0_0_0$effect.size, round(d_1_0_0_0$Overall.OC,4)),
c(d_0_1_0_0$effect.size, round(d_0_1_0_0$Overall.OC,4)),
c(d_1_1_0_0$effect.size, round(d_1_1_0_0$Overall.OC,4)),
c(d_1_1_1_0$effect.size, round(d_1_1_1_0$Overall.OC,4)),
c(d_1_1_0_1$effect.size, round(d_1_1_0_1$Overall.OC,4)),
c(d_1_0_1_0$effect.size, round(d_1_0_1_0$Overall.OC,4)),
c(d_1_1_1_1$effect.size, round(d_1_1_1_1$Overall.OC,4)),
c(d_2_2_0_0$effect.size, round(d_2_2_0_0$Overall.OC,4)),
c(d_2_2_1_0$effect.size, round(d_2_2_1_0$Overall.OC,4)),
c(d_2_2_0_1$effect.size, round(d_2_2_0_1$Overall.OC,4))))

}

}

\keyword{ operating characteristics}
