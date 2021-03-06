#' @export

operatingChar <-function(alpha,information.fraction,th1,th2,f1){

  if( any(f1 < 0 | f1 > 1) ) stop('f1 should be between 0 and 1')
  if( any(information.fraction < 0 | information.fraction > 1) ) stop('information.fraction should be between 0 and 1')

  myresults <- list()

  t=information.fraction
  alpha.Star<-(alpha*alpha)
  au1.Star<-alpha.Star*t
  al1.Star<-(1-alpha.Star)*t

  au2.Star<-alpha.Star*(1-t)
  al2.Star<-(1-alpha.Star)*(1-t)

  int.u1<-qnorm(au1.Star,mean=0,sd =1,lower.tail=FALSE)
  int.l1<-qnorm(al1.Star,mean=0,sd =1,lower.tail=TRUE)

  Stage1.Limits.Opt <- optim(c(int.l1,int.u1),
                             Stage1.Boundary.Values,
                             D=c(1,1,1,1),
                             th1=c(0,0),
                             th2=c(0,0),
                             au=au1.Star,
                             al=al1.Star,
                             optimize=T,
                             method="Nelder-Mead",
                             control=list(maxit=1000))

  Limits.1<-cbind(Stage1.Limits.Opt$par[1],Stage1.Limits.Opt$par[2])
  myresults$Stage1.Limits <- as.vector(Limits.1)
  names(myresults$Stage1.Limits) <- c('Lower Limit','Upper Limit')

  int.l2<-Stage1.Limits.Opt$par[2]
  int.u2<-Stage1.Limits.Opt$par[2]

  Stage2.Limits.Opt <- optim(c(int.l2,int.u2),
                             Stage2.Boundary.Values,
                             lims1=c(Stage1.Limits.Opt$par[1],Stage1.Limits.Opt$par[2]),
                             th1=c(0,0),
                             th2=c(0,0),
                             D=c(1,1,1,1),
                             Ds2=c(1,1,1,1),
                             au2=au2.Star,
                             al2=al2.Star,
                             optimize=T,
                             method="Nelder-Mead",
                             control=list(maxit=1000))

  Limits.2<-cbind(Stage2.Limits.Opt$par[1],Stage2.Limits.Opt$par[2])
  myresults$Stage2.Limits <- as.vector(Limits.2)[1]
  names(myresults$Stage2.Limits) <- c('Upper Limit')

  # Computation of estimated stage1 information under th1=(1,1) and th2=(1,1)
  D1.Hat <- optim(c(1,1,1,1),
                  Stage1.Inf.Optimization,
                  a=Limits.1[1],b=Limits.1[2],
                  th1=c(1,1),th2=c(1,1),
                  optimize=T,method="Nelder-Mead",control=list(maxit=1000))

  D2.Hat<-optim(c(1,1,1,1),
                Stage2.Inf.Optimization,
                lims2=c(Limits.2[1],Limits.2[2]),
                lims1=c(Limits.1[1],Limits.1[2]),
                th1=c(1,1),th2=c(1,1),
                D=c(D1.Hat$par[1],D1.Hat$par[3],D1.Hat$par[2],D1.Hat$par[4]),
                optimize=T,
                method="Nelder-Mead",
                control=list(maxit=1000))

  # Computation of total estimated information
  D.Hat<-sum(D1.Hat$par)+sum(D2.Hat$par)

  # Stage1 subgroup specific information at information time t
  # Total informationxinformation timexsubgroup prevalence

  Stage1.S1.D.Hat<-D.Hat*t*f1
  Stage1.S2.D.Hat<-D.Hat*t*(1-f1)

  # New estimated information for stage 1
  New.D1.Hat<-cbind(Stage1.S1.D.Hat/2,Stage1.S1.D.Hat/2,Stage1.S2.D.Hat/2,Stage1.S2.D.Hat/2)

  # Stage 1 power under th1=(1,1),th2=c(1,1)
  # Changing the values of th1 and th2 below we can get stage 1 power for different efficacy setting
  Stage1.Power <- Stage1.Boundary.Values(lims1=c(Limits.1[1],Limits.1[2]),
                                         D=c(New.D1.Hat[1],New.D1.Hat[2],New.D1.Hat[3],New.D1.Hat[4]),
                                         th1=th1,th2=th2,
                                         au=au1.Star,al=al1.Star,optimize=F)

  # Stage 2 power under th1=(1,1),th2=c(1,1)
  # Changing the values of th1 and th2 below we can get stage 1 power for different efficacy setting

  Stage2.Power <- Stage2.Boundary.Values(lims2=c(Limits.2[1],Limits.2[2]),
                                         lims1=c(Limits.1[1],Limits.1[2]),
                                         th1=th1,th2=th2,
                                         D=c(New.D1.Hat[1],New.D1.Hat[2],New.D1.Hat[3],New.D1.Hat[4]),
                                         Ds2=c(New.D1.Hat[1],New.D1.Hat[2],New.D1.Hat[3],New.D1.Hat[4]),
                                         au2=au2.Star,al2=al2.Star,optimize=F)

  # For same values of th1 and th2, if we add Stage1 and stage 2 power, we will get the
  # final table in the manuscript which provides rejection probabilities.

  myresults$Stage1.OC <- c(Stage1.Power$OR,
                           Stage1.Power$S1,
                           Stage1.Power$S2,
                           Stage1.Power$TR)

  myresults$Stage2.OC <- c(Stage2.Power$OR,
                           Stage2.Power$S1,
                           Stage2.Power$S2,
                           Stage2.Power$TR)

  myresults$Overall.OC <- c((Stage1.Power$OR + Stage2.Power$OR),
                            (Stage1.Power$S1 + Stage2.Power$S1),
                            (Stage1.Power$S2 + Stage2.Power$S2),
                            (Stage1.Power$TR + Stage2.Power$TR))

  names(myresults$Stage1.OC) <- c('Rejection probability overall',
                                  'Rejection probability subgroup 1',
                                  'Rejection probability subgroup 2',
                                  'Total rejection probability')

  names(myresults$Stage2.OC) <- c('Rejection probability overall',
                                  'Rejection probability subgroup 1',
                                  'Rejection probability subgroup 2',
                                  'Total rejection probability')

  names(myresults$Overall.OC) <- c('Rejection probability overall',
                                   'Rejection probability subgroup 1',
                                   'Rejection probability subgroup 2',
                                   'Total rejection probability')

  return(myresults)
}
