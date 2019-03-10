#' @importFrom msm ptnorm

######################################################################
# Probability of being above/below the futility boundary in stage I
#
######################################################################
library(msm) # needed for truncated normal

######################################
# Function 1
# Stage 1 Subgroup 1 Outcome 1 Greater
######################################

Stage1.S1.O1.Gt<-function(a,th1,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  pnorm(a*sqrt(D1[1,1]),th1[1]*D1[1,1],sqrt(D1[1,1]),lower.tail=F)

}



####################################
# Function 2
# Stage 1 Subgroup 1 Outcome 1 Less
####################################

Stage1.S1.O1.Lt<-function(a,th1,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  pnorm(a*sqrt(D1[1,1]),th1[1]*D1[1,1],sqrt(D1[1,1]),lower.tail=T)

}


######################################
# Function 3
# Stage 1 Subgroup 1 Outcome 2 Greater
######################################

Stage1.S1.O2.Gt<-function(a,th1,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  pnorm(a*sqrt(D1[1,2]),th1[2]*D1[1,2],sqrt(D1[1,2]),lower.tail=F)

}


###################################
# Function 4
# Stage 1 Subgroup 1 Outcome 2 Less
###################################

Stage1.S1.O2.Lt<-function(a,th1,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  pnorm(a*sqrt(D1[1,2]),th1[2]*D1[1,2],sqrt(D1[1,2]),lower.tail=T)

}


######################################
# Function 5
# Stage 1 Subgroup 2 Outcome 1 Greater
######################################

Stage1.S2.O1.Gt<-function(a,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  pnorm(a*sqrt(D1[2,1]),th2[1]*D1[2,1],sqrt(D1[2,1]),lower.tail=F)

}


####################################
# Function 6
# Stage 1 Subgroup 2 Outcome 1 Less
####################################

Stage1.S2.O1.Lt<-function(a,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  pnorm(a*sqrt(D1[2,1]),th2[1]*D1[2,1],sqrt(D1[2,1]),lower.tail=T)

}


######################################
# Function 7
# Stage 1 Subgroup 2 Outcome 2 Greater
######################################

Stage1.S2.O2.Gt<-function(a,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  pnorm(a*sqrt(D1[2,2]),th2[2]*D1[2,2],sqrt(D1[2,2]),lower.tail=F)

}


#####################################
# Function 8
# Stage 1 Subgroup 2 Outcome 2 Less
#####################################

Stage1.S2.O2.Lt<-function(a,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  pnorm(a*sqrt(D1[2,2]),th2[2]*D1[2,2],sqrt(D1[2,2]),lower.tail=T)

}


#################################################################################
# Computation of 16 path probabilities (E0-E15) for decision rule
#
################################################################################


##########################################################
# Function 9. Computation of probability E-0
#
##########################################################

Prob.E0<-function(a,th1,th2,D){

  Stage1.S1.O1.Lt(a,th1,D)*Stage1.S1.O2.Lt(a,th1,D)*Stage1.S2.O1.Lt(a,th2,D)*Stage1.S2.O2.Lt(a,th2,D)


}




##########################################################
# Function 10. Computation of probability E-1
#
##########################################################

Prob.E1<-function(a,th1,th2,D){

  Stage1.S1.O1.Lt(a,th1,D)*Stage1.S1.O2.Lt(a,th1,D)*Stage1.S2.O1.Lt(a,th2,D)*Stage1.S2.O2.Gt(a,th2,D)


}






##########################################################
# Function 11. Computation of probability E-2
#
##########################################################

Prob.E2<-function(a,th1,th2,D){

  Stage1.S1.O1.Lt(a,th1,D)*Stage1.S1.O2.Lt(a,th1,D)*Stage1.S2.O1.Gt(a,th2,D)*Stage1.S2.O2.Lt(a,th2,D)


}





##########################################################
# Function 12. Computation of probability E-3
#
##########################################################


Prob.E3<-function(a,th1,th2,D){

  Stage1.S1.O1.Lt(a,th1,D)*Stage1.S1.O2.Lt(a,th1,D)*Stage1.S2.O1.Gt(a,th2,D)*Stage1.S2.O2.Gt(a,th2,D)


}





##########################################################
# Function 13. Computation of probability E-4
#
##########################################################


Prob.E4<-function(a,th1,th2,D){

  Stage1.S1.O1.Lt(a,th1,D)*Stage1.S1.O2.Gt(a,th1,D)*Stage1.S2.O1.Lt(a,th2,D)*Stage1.S2.O2.Lt(a,th2,D)


}





##########################################################
# Function 14. Computation of probability E-5
#
##########################################################


Prob.E5<-function(a,th1,th2,D){

  Stage1.S1.O1.Gt(a,th1,D)*Stage1.S1.O2.Lt(a,th1,D)*Stage1.S2.O1.Lt(a,th2,D)*Stage1.S2.O2.Lt(a,th2,D)


}






##########################################################
# Function 15. Computation of probability E-6
#
##########################################################


Prob.E6<-function(a,th1,th2,D){

  Stage1.S1.O1.Gt(a,th1,D)*Stage1.S1.O2.Gt(a,th1,D)*Stage1.S2.O1.Lt(a,th2,D)*Stage1.S2.O2.Lt(a,th2,D)


}



##########################################################
# Function 16. Computation of probability E-7
#
##########################################################


Prob.E7<-function(a,th1,th2,D){

  Stage1.S1.O1.Lt(a,th1,D)*Stage1.S1.O2.Gt(a,th1,D)*Stage1.S2.O1.Lt(a,th2,D)*Stage1.S2.O2.Gt(a,th2,D)


}



##########################################################
# Function 17. Computation of probability E-8
#
##########################################################


Prob.E8<-function(a,th1,th2,D){

  Stage1.S1.O1.Lt(a,th1,D)*Stage1.S1.O2.Gt(a,th1,D)*Stage1.S2.O1.Gt(a,th2,D)*Stage1.S2.O2.Lt(a,th2,D)


}




##########################################################
# Function 18. Computation of probability E-9
#
##########################################################


Prob.E9<-function(a,th1,th2,D){

  Stage1.S1.O1.Gt(a,th1,D)*Stage1.S1.O2.Lt(a,th1,D)*Stage1.S2.O1.Gt(a,th2,D)*Stage1.S2.O2.Lt(a,th2,D)


}





##########################################################
# Function 19. Computation of probability E-10
#
##########################################################


Prob.E10<-function(a,th1,th2,D){

  Stage1.S1.O1.Gt(a,th1,D)*Stage1.S1.O2.Lt(a,th1,D)*Stage1.S2.O1.Lt(a,th2,D)*Stage1.S2.O2.Gt(a,th2,D)


}




##########################################################
# Function 20. Computation of probability E-11
#
##########################################################

Prob.E11<-function(a,th1,th2,D){

  Stage1.S1.O1.Gt(a,th1,D)*Stage1.S1.O2.Gt(a,th1,D)*Stage1.S2.O1.Gt(a,th2,D)*Stage1.S2.O2.Lt(a,th2,D)


}



##########################################################
# Function 21. Computation of probability E-12
#
##########################################################

Prob.E12<-function(a,th1,th2,D){

  Stage1.S1.O1.Gt(a,th1,D)*Stage1.S1.O2.Gt(a,th1,D)*Stage1.S2.O1.Lt(a,th2,D)*Stage1.S2.O2.Gt(a,th2,D)


}




##########################################################
# Function 22. Computation of probability E-13
#
##########################################################

Prob.E13<-function(a,th1,th2,D){

  Stage1.S1.O1.Gt(a,th1,D)*Stage1.S1.O2.Lt(a,th1,D)*Stage1.S2.O1.Gt(a,th2,D)*Stage1.S2.O2.Gt(a,th2,D)


}



##########################################################
# Function 23. Computation of probability E-14
#
##########################################################

Prob.E14<-function(a,th1,th2,D){

  Stage1.S1.O1.Lt(a,th1,D)*Stage1.S1.O2.Gt(a,th1,D)*Stage1.S2.O1.Gt(a,th2,D)*Stage1.S2.O2.Gt(a,th2,D)


}


##########################################################
# Function 24. Computation of probability E-15
# Both Subgroup selected
##########################################################

Prob.E15<-function(a,th1,th2,D){

  Stage1.S1.O1.Gt(a,th1,D)*Stage1.S1.O2.Gt(a,th1,D)*Stage1.S2.O1.Gt(a,th2,D)*Stage1.S2.O2.Gt(a,th2,D)


}





######################################################################
# Probability of being above/below the efficacy boundary in stage I
#
######################################################################





##########################################################
# Function 25
# Stage 1 Subgroup 1 Outcome 1 Greater than upper boundary
##########################################################

Stage1.S1.O1.Tn.Gt<-function(a,b,th1,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  ptnorm(b*sqrt(D1[1,1]),th1[1]*D1[1,1],sqrt(D1[1,1]),lower=a*sqrt(D1[1,1]),lower.tail=F)

}



##########################################################
# Function 26
# Stage 1 Subgroup 1 Outcome 1 less than upper boundary
##########################################################

Stage1.S1.O1.Tn.Lt<-function(a,b,th1,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  ptnorm(b*sqrt(D1[1,1]),th1[1]*D1[1,1],sqrt(D1[1,1]),lower=a*sqrt(D1[1,1]),lower.tail=T)

}



##########################################################
# Function 27
# Stage 1 Subgroup 1 Outcome 2 Greater than upper boundary
##########################################################


Stage1.S1.O2.Tn.Gt<-function(a,b,th1,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  ptnorm(b*sqrt(D1[1,2]),th1[2]*D1[1,2],sqrt(D1[1,2]),lower=a*sqrt(D1[1,2]),lower.tail=F)

}


##########################################################
# Function 28
# Stage 1 Subgroup 1 Outcome 2 Less than upper boundary
##########################################################


Stage1.S1.O2.Tn.Lt<-function(a,b,th1,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  ptnorm(b*sqrt(D1[1,2]),th1[2]*D1[1,2],sqrt(D1[1,2]),lower=a*sqrt(D1[1,2]),lower.tail=T)

}




##########################################################
# Function 29
# Stage 1 Subgroup 2 Outcome 1 Greater than upper boundary
##########################################################


Stage1.S2.O1.Tn.Gt<-function(a,b,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  ptnorm(b*sqrt(D1[2,1]),th2[1]*D1[2,1],sqrt(D1[2,1]),lower=a*sqrt(D1[2,1]),lower.tail=F)

}



##########################################################
# Function 30
# Stage 1 Subgroup 2 Outcome 1 Less than upper boundary
##########################################################


Stage1.S2.O1.Tn.Lt<-function(a,b,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  ptnorm(b*sqrt(D1[2,1]),th2[1]*D1[2,1],sqrt(D1[2,1]),lower=a*sqrt(D1[2,1]),lower.tail=T)

}


##########################################################
# Function 31
# Stage 1 Subgroup 2 Outcome 2 Greater than upper boundary
##########################################################


Stage1.S2.O2.Tn.Gt<-function(a,b,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  ptnorm(b*sqrt(D1[2,2]),th2[2]*D1[2,2],sqrt(D1[2,2]),lower=a*sqrt(D1[2,2]),lower.tail=F)

}




##########################################################
# Function 32
# Stage 1 Subgroup 2 Outcome 2 Less than upper boundary
##########################################################


Stage1.S2.O2.Tn.Lt<-function(a,b,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  ptnorm(b*sqrt(D1[2,2]),th2[2]*D1[2,2],sqrt(D1[2,2]),lower=a*sqrt(D1[2,2]),lower.tail=T)

}


## Both crossing/not crossing the upper boundary
#
# #########################################################
# # Function 33
# # When both subgroups are selected we pull them together
# # Below is the distribution of the sum of two truncated normal variables
# # This is done for outcome outcome 1
# #########################################################
#
# Stage1.S12.O1.Tn.Dt <- function(y,a,th1,th2,D) {
#
#   D1=matrix(D,nrow=2,ncol=2,byrow=T)
#
#   mu11=th1[1]*D1[1,1]
#   sig11=sqrt(D1[1,1])
#   mu21=th2[1]*D1[2,1]
#   sig21=sqrt(D1[2,1])
#   Stage1.S1.O1.Select.lower=a*sqrt(D1[1,1])
#   Stage1.S2.O1.Select.lower=a*sqrt(D1[2,1])
#
#
#   integrand <- function(x) {
#
#     f1 <- dtnorm(x,mu11,sig11,lower=Stage1.S1.O1.Select.lower)
#     f2 <- dtnorm(y-x,mu21,sig21,lower=Stage1.S2.O1.Select.lower)
#     return(f1*f2)
#   }
#   int <- integrate(integrand,lower=Stage1.S1.O1.Select.lower,upper=y-Stage1.S2.O1.Select.lower)
#
#   return(int$value)
# }




#####################################################
# Function 33
# Outcome 1 for both subgroup is above upper bounday
####################################################


Stage1.S12.O1.Tn.Gt<-function(a,b,th1,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  mu11=th1[1]*D1[1,1]
  sig11=sqrt(D1[1,1])
  mu21=th2[1]*D1[2,1]
  sig21=sqrt(D1[2,1])
  Stage1.S1.O1.Select.lower=a*sqrt(D1[1,1])        # Subgroup 1, Outcome 1 lower bound for selection
  Stage1.S2.O1.Select.lower=a*sqrt(D1[2,1])        # Subgroup 2, Outcome 1 lower bound for selection

  Stage1.S12.O1.upper=b*sqrt(D1[1,1]+D1[2,1])      # Subgroup 1 & 2, Outcome 1 upper bound for rejection/acceptace



  Stage1.S12.O1.Tn.Dt <- function(y)
  {

    integrand <- function(x)
    {

      f1 <- dtnorm(x,mu11,sig11,lower=Stage1.S1.O1.Select.lower)
      f2 <- dtnorm(y-x,mu21,sig21,lower=Stage1.S2.O1.Select.lower)
      return(f1*f2)
    }

    int.x <- integrate(integrand,lower=Stage1.S1.O1.Select.lower,upper=y-Stage1.S2.O1.Select.lower)

    return(int.x$value)
  }


  int.y<-integrate(Vectorize(Stage1.S12.O1.Tn.Dt),lower=Stage1.S12.O1.upper,upper=Inf)


  return(int.y$value)
}




#####################################################
# Function 34
# Outcome 1 for both subgroup is less than upper bounday
####################################################







#####################################################
# Function 35
# Outcome 2 for both subgroup is above upper bounday
####################################################


Stage1.S12.O2.Tn.Gt<-function(a,b,th1,th2,D){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)

  mu12=th1[2]*D1[1,2]
  sig12=sqrt(D1[1,2])


  mu22=th2[2]*D1[2,2]
  sig22=sqrt(D1[2,2])


  Stage1.S1.O2.Select.lower=a*sqrt(D1[1,2])        # Subgroup 1, Outcome 2 lower bound for selection
  Stage1.S2.O2.Select.lower=a*sqrt(D1[2,2])        # Subgroup 2, Outcome 2 lower bound for selection

  Stage1.S12.O2.upper=b*sqrt(D1[1,2]+D1[2,2])      # Subgroup 1 & 2, Outcome 2 upper bound for rejection/acceptace



  Stage1.S12.O2.Tn.Dt <- function(y)
  {

    integrand <- function(x)
    {

      f1 <- dtnorm(x,mu12,sig12,lower=Stage1.S1.O2.Select.lower)
      f2 <- dtnorm(y-x,mu22,sig22,lower=Stage1.S2.O2.Select.lower)
      return(f1*f2)
    }

    int.x <- integrate(integrand,lower=Stage1.S1.O2.Select.lower,upper=y-Stage1.S2.O2.Select.lower)

    return(int.x$value)
  }


  int.y<-integrate(Vectorize(Stage1.S12.O2.Tn.Dt),lower=Stage1.S12.O2.upper,upper=Inf)


  return(int.y$value)
}




#####################################################
# Function 36
# Outcome 2 for both subgroup is below upper bounday
####################################################






##########################################################
# Function 37. Subgroup 2, Outcome 1 & Outcome 2 greater than upper boundary
# Subgroup 2 selected
##########################################################


Stage1.S2.Reject.Given.S2.Select<-function(a,b,th2,D)

{
  Stage1.S2.O1.Tn.Gt(a=a,b=b,th2=th2,D=D)*Stage1.S2.O2.Tn.Gt(a=a,b=b,th2=th2,D=D)

}



##########################################################
# Function 38. Subgroup 1, Outcome 1 & Outcome 2 greater than upper boundary
# Subgroup 1 select
##########################################################


Stage1.S1.Reject.Given.S1.Select<-function(a,b,th1,D)

{
  Stage1.S1.O1.Tn.Gt(a=a,b=b,th1=th1,D=D)*Stage1.S1.O2.Tn.Gt(a=a,b=b,th1=th1,D=D)

}


##########################################################
# Function 39. Subgroup 1 & 2, Outcome 1 & Outcome 2 greater than upper boundary
# Both Subgroups are selected
##########################################################


Stage1.S12.Reject.Given.S12.Select<-function(a,b,th1,th2,D)

{
  Stage1.S12.O1.Tn.Gt(a=a,b=b,th1=th1,th2=th2,D=D)*Stage1.S12.O2.Tn.Gt(a=a,b=b,th1=th1,th2=th2,D=D)

}



##########################################################
# Function 40. Only Subgroup 2 Selected
#
##########################################################

Stage1.S2.Select<-function(a,b,th1,th2,D)
{
  (Prob.E3(a=a,th1=th1,th2=th2,D=D)+Prob.E13(a=a,th1=th1,th2=th2,D=D)+Prob.E14(a=a,th1=th1,th2=th2,D=D))
}



##########################################################
# Function 40. Only Subgroup 2 Selected
#
##########################################################

Stage1.S1.Select<-function(a,b,th1,th2,D)
{
  (Prob.E6(a=a,th1=th1,th2=th2,D=D)+Prob.E11(a=a,th1=th1,th2=th2,D=D)+Prob.E12(a=a,th1=th1,th2=th2,D=D))
}



##########################################################
# Function 40. Only Subgroup 2 Selected
#
##########################################################

Stage1.S12.Select<-function(a,b,th1,th2,D)
{
  (Prob.E15(a=a,th1=th1,th2=th2,D=D))
}


##########################################################
# Function 40. Only efficacious in Subgroup 2
#
##########################################################


Stage1.S2.Ef<-function(a,b,th1,th2,D)

{

  Stage1.S2.Select(a=a,b=b,th1=th1,th2=th2,D=D)*Stage1.S2.Reject.Given.S2.Select(a=a,b=b,th2=th2,D=D)

}



##########################################################
# Function 41. Only efficacious in Subgroup 1
#
##########################################################


Stage1.S1.Ef<-function(a,b,th1,th2,D)

{

  Stage1.S1.Select(a=a,b=b,th1=th1,th2=th2,D=D)*Stage1.S1.Reject.Given.S1.Select(a=a,b=b,th1=th1,D=D)

}


##########################################################
# Function 42. Efficacacious in Subgroup 1 & Subgroup2
#
##########################################################


Stage1.S12.Ef<-function(a,b,th1,th2,D)

{

  Stage1.S12.Select(a=a,th1=th1,th2=th2,D=D)*Stage1.S12.Reject.Given.S12.Select(a=a,b=b,th1=th1,th2=th2,D=D)

}



##########################################################
# Function 43. Stage I total Acceptance probability for futility
#
##########################################################


Stage1.Total.Ft<-function(a,b,th1,th2,D)

{

  Prob.E0(a=a,th1=th1,th2=th2,D=D)+Prob.E1(a=a,th1=th1,th2=th2,D=D)+Prob.E2(a=a,th1=th1,th2=th2,D=D)+
    Prob.E4(a=a,th1=th1,th2=th2,D=D)+Prob.E5(a=a,th1=th1,th2=th2,D=D)+Prob.E7(a=a,th1=th1,th2=th2,D=D)+
    Prob.E8(a=a,th1=th1,th2=th2,D=D)+Prob.E9(a=a,th1=th1,th2=th2,D=D)+Prob.E10(a=a,th1=th1,th2=th2,D=D)


}




##########################################################
# Function 44. Stage I total rejection probability for efficacy
#
##########################################################


Stage1.Total.Ef<-function(a,b,th1,th2,D)

{
  Stage1.S1.Ef(a=a,b=b,th1=th1,th2=th2,D=D)+Stage1.S2.Ef(a=a,b=b,th1=th1,th2=th2,D=D)+Stage1.S12.Ef(a=a,b=b,th1=th1,th2=th2,D=D)
}





##########################################################
# Function 45. Computation of boundary values in Stage I
#
##########################################################


Stage1.Boundary.Values<-function(lims1,D,th1,th2,au,al,optimize=T){

  a=lims1[1]
  b=lims1[2]

  if (optimize==T) {

    retval<-(Stage1.Total.Ef(a=a,b=b,th1=th1,th2=th2,D=D)-au)^2+(Stage1.Total.Ft(a=a,b=b,th1=th1,th2=th2,D=D)-al)^2

  } else{

    retval<-list(TR=Stage1.Total.Ef(a=a,b=b,th1=th1,th2=th2,D=D),
                 OR=Stage1.S12.Ef(a=a,b=b,th1=th1,th2=th2,D=D),
                 S1=Stage1.S1.Ef(a=a,b=b,th1=th1,th2=th2,D=D),
                 S2=Stage1.S2.Ef(a=a,b=b,th1=th1,th2=th2,D=D))

  }
  return(retval)

}






##########################################################
# Function 46. Computation of Optimum information in Stage I
#
##########################################################


Stage1.Inf.Optimization<-function(D,a,b,th1,th2,optimize=T)

{

  if (optimize==T) {

    retval<-(Stage1.Total.Ef(a=a,b=b,th1=th1,th2=th2,D=D)-0.405)^2


  } else{

    retval<-list(TR=Stage1.Total.Ef(a=a,b=b,th1=th1,th2=th2,D=D),
                 OR=Stage1.S12.Ef(a=a,b=b,th1=th1,th2=th2,D=D),
                 S1=Stage1.S1.Ef(a=a,b=b,th1=th1,th2=th2,D=D),
                 S2=Stage1.S2.Ef(a=a,b=b,th1=th1,th2=th2,D=D))

  }

  return(retval)

}












##########################################################
# Function 47. Stage 2 subgroup 1 outcome 1 distribution
#lt=F will generate the proability that the outcome 1 in stage 2 is above efficacy boundary
#lt=T will generate the proability that the outcome 1 in stage 2 is below efficacy boundary
# l.lim and u.lim will regulate if the ourcome 1 at stage 1 was above/below efficacy boundary
##########################################################


Stage2.S1.O1.Dt<-function(a,b,c1,th1,th2,D,Ds2,l.lim,u.lim,lt)

{

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  Stage1.S1.O1.Tn.Mean<-th1[1]*D1[1,1]       # Truncated normal mean,sd,lower and upper limits
  Stage1.S1.O1.Tn.Sd<-sqrt(D1[1,1])
  Stage1.S1.O1.Tn.Lower<-a*sqrt(D1[1,1])
  Stage1.S1.O1.Tn.Upper<-b*sqrt(D1[1,1])     # upper limit to see if the outcome cross the upper boundary for efficacy


  Stage2.S1.O1.Nor.Mean<-th1[1]*(D2[1,1])     # When the trial continues in the second stage, this information is needed
  Stage2.S1.O1.Nor.Sd<-sqrt((D2[1,1]))        # for the second stage distribition of outcome 1
  Stage2.S1.O1.Nor.Lower<-c1*sqrt(D1[1,1]+D2[1,1])



  Stage2.S1.O1.Int<-function(u)

  {

    return(dtnorm(u,Stage1.S1.O1.Tn.Mean,Stage1.S1.O1.Tn.Sd,lower=Stage1.S1.O1.Tn.Lower)*
             pnorm(Stage2.S1.O1.Nor.Lower,Stage2.S1.O1.Nor.Mean+u,Stage2.S1.O1.Nor.Sd,lower.tail=lt))

  }


  int.u<-integrate(Vectorize(Stage2.S1.O1.Int),lower=l.lim,upper=u.lim)


  return(int.u$value)

}



##
##Stage2.S2.O1.Dt(a=-0.1040146,b=2.2773003,c1=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1),l.lim=1,u.lim=Inf,lt=F)
##


##########################################################
# Function 48. Stage 2 subgroup 1 outcome 2 distribution
# lt=F will generate the proability that the outcome 2 in stage 2 is above efficacy boundary
#lt=T will generate the proability that the outcome 2 in stage 2 is below efficacy boundary
# l.lim and u.lim will regulate if the outcome 2 at stage 1 was above/below efficacy boundary
##########################################################
##########################################################


Stage2.S1.O2.Dt<-function(a,b,c2,th1,th2,D,Ds2,l.lim,u.lim,lt)

{

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  Stage1.S1.O2.Tn.Mean<-th1[2]*D1[1,2]       # Truncated normal mean,sd,lower and upper limits
  Stage1.S1.O2.Tn.Sd<-sqrt(D1[1,2])
  Stage1.S1.O2.Tn.Lower<-a*sqrt(D1[1,2])
  Stage1.S1.O2.Tn.Upper<-b*sqrt(D1[1,2])     # upper limit to see if the outcome cross the upper boundary for efficacy


  Stage2.S1.O2.Nor.Mean<-th1[2]*(D2[1,2])     # When the trial continues in the second stage, this information is needed
  Stage2.S1.O2.Nor.Sd<-sqrt((D2[1,2]))        # for the second stage distribition of outcome 1
  Stage2.S1.O2.Nor.Lower<-c2*sqrt(D1[1,2]+D2[1,2])



  Stage2.S1.O2.Int<-function(v)

  {

    return(dtnorm(v,Stage1.S1.O2.Tn.Mean,Stage1.S1.O2.Tn.Sd,lower=Stage1.S1.O2.Tn.Lower)*
             pnorm(Stage2.S1.O2.Nor.Lower,Stage2.S1.O2.Nor.Mean+v,Stage2.S1.O2.Nor.Sd,lower.tail=lt))

  }


  int.v<-integrate(Vectorize(Stage2.S1.O2.Int),lower=l.lim,upper=u.lim)


  return(int.v$value)

}


#Stage2.S2.O2.Dt(a=-0.1040146,b=2.2773003,c2=1,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1),l.lim=1,u.lim=Inf,lt=F)


##########################################################
# Function 49. Stage 2 subgroup 1 distribution
# lt=F will give the rejection probability for subgroup 1 in stage 2
##########################################################



Stage2.S1.Reject.Given.S1.Select<-function(a,b,c1,c2,th1,th2,D,Ds2){
  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)

  Stage1.S1.O1.Tn.Lower<-a*sqrt(D1[1,1])
  Stage1.S1.O1.Tn.Upper<-b*sqrt(D1[1,1])
  Stage1.S1.O2.Tn.Lower<-a*sqrt(D1[1,2])
  Stage1.S1.O2.Tn.Upper<-b*sqrt(D1[1,2])


  ((Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Upper,u.lim=Inf,lt=F)*
      Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Lower,u.lim=Stage1.S1.O2.Tn.Upper,lt=F))+

      (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Lower,u.lim=Stage1.S1.O1.Tn.Upper,lt=F)*
         Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Upper,u.lim=Inf,lt=F))+

      (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Lower,u.lim=Stage1.S1.O1.Tn.Upper,lt=F)*
         Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Lower,u.lim=Stage1.S1.O2.Tn.Upper,lt=F)))

}




#Stage2.S1.Reject.Given.S1.Select(a=-0.1040146,b=2.2773003,c1=1,c2=1,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))



###########################################################
# Subgroup 1 efficacy in stage 2
#
###########################################################

Stage2.S1.Ef<-function(a,b,c1,c2,th1,th2,D,Ds2)

{
  Stage1.S1.Select(a,b,th1,th2,D)*Stage2.S1.Reject.Given.S1.Select(a,b,c1,c2,th1,th2,D,Ds2)

}


##
## Stage2.S1.Ef(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##






###########################################################
# Stage 2 stop for futility in Subgroup 1, given subgroup 1 selected
#
###########################################################



Stage2.S1.Accept.Given.S1.Select<-function(a,b,c1,c2,th1,th2,D,Ds2){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  Stage1.S1.O1.Tn.Lower<-a*sqrt(D1[1,1])
  Stage1.S1.O1.Tn.Upper<-b*sqrt(D1[1,1])
  Stage1.S1.O2.Tn.Lower<-a*sqrt(D1[1,2])
  Stage1.S1.O2.Tn.Upper<-b*sqrt(D1[1,2])


  (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Upper,u.lim=Inf,lt=F)*
      Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Lower,u.lim=Stage1.S1.O2.Tn.Upper,lt=T))+
    (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Upper,u.lim=Inf,lt=T)*
       Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Lower,u.lim=Stage1.S1.O2.Tn.Upper,lt=F))+
    (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Upper,u.lim=Inf,lt=T)*
       Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Lower,u.lim=Stage1.S1.O2.Tn.Upper,lt=T))+


    (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Lower,u.lim=Stage1.S1.O1.Tn.Upper,lt=F)*
       Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Upper,u.lim=Inf,lt=T))+
    (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Lower,u.lim=Stage1.S1.O1.Tn.Upper,lt=T)*
       Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Upper,u.lim=Inf,lt=F))+
    (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Lower,u.lim=Stage1.S1.O1.Tn.Upper,lt=T)*
       Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Upper,u.lim=Inf,lt=T))+

    (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Lower,u.lim=Stage1.S1.O1.Tn.Upper,lt=F)*
       Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Lower,u.lim=Stage1.S1.O2.Tn.Upper,lt=T))+
    (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Lower,u.lim=Stage1.S1.O1.Tn.Upper,lt=T)*
       Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Lower,u.lim=Stage1.S1.O2.Tn.Upper,lt=F))+
    (Stage2.S1.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O1.Tn.Lower,u.lim=Stage1.S1.O1.Tn.Upper,lt=T)*
       Stage2.S1.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S1.O2.Tn.Lower,u.lim=Stage1.S1.O2.Tn.Upper,lt=T))
}



##
##Stage2.S1.Accept.Given.S1.Select(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##



###########################################################
# Subgroup 1 futility in stage 2
#
###########################################################

Stage2.S1.Ft<-function(a,b,c1,c2,th1,th2,D,Ds2)

{
  Stage1.S1.Select(a,b,th1,th2,D)*Stage2.S1.Accept.Given.S1.Select(a,b,c1,c2,th1,th2,D,Ds2)

}


#Stage2.S1.Ft(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))





##########################################################
# Function 50. Stage 2 subgroup 2 outcome 1 distribution
#lt=F will generate the proability that the outcome 1 in stage 2 is above efficacy boundary
#lt=T will generate the proability that the outcome 1 in stage 2 is below efficacy boundary
# l.lim and u.lim will regulate if the outcome 1 at stage 1 was above/below efficacy boundary

##########################################################


Stage2.S2.O1.Dt<-function(a,b,c1,th1,th2,D,Ds2,l.lim,u.lim,lt)

{

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  Stage1.S2.O1.Tn.Mean<-th2[1]*D1[2,1]       # Truncated normal mean,sd,lower and upper limits
  Stage1.S2.O1.Tn.Sd<-sqrt(D1[2,1])
  Stage1.S2.O1.Tn.Lower<-a*sqrt(D1[2,1])
  Stage1.S2.O1.Tn.Upper<-b*sqrt(D1[2,1])     # upper limit to see if the outcome cross the upper boundary for efficacy


  Stage2.S2.O1.Nor.Mean<-th2[1]*(D2[2,1])     # When the trial continues in the second stage, this information is needed
  Stage2.S2.O1.Nor.Sd<-sqrt((D2[2,1]))        # for the second stage distribition of outcome 1
  Stage2.S2.O1.Nor.Lower<-c1*sqrt(D1[2,1]+D2[2,1])



  Stage2.S2.O1.Int<-function(u)

  {

    return(dtnorm(u,Stage1.S2.O1.Tn.Mean,Stage1.S2.O1.Tn.Sd,lower=Stage1.S2.O1.Tn.Lower)*
             pnorm(Stage2.S2.O1.Nor.Lower,Stage2.S2.O1.Nor.Mean+u,Stage2.S2.O1.Nor.Sd,lower.tail=lt))

  }


  int.u<-integrate(Vectorize(Stage2.S2.O1.Int),lower=l.lim,upper=u.lim)


  return(int.u$value)

}







##########################################################
# Function 51. Stage 2 subgroup 2 outcome 2 distribution
# lt=F will generate the proability that the outcome 2 in stage 2 is above efficacy boundary
# lt=T will generate the proability that the outcome 2 in stage 2 is below efficacy boundary
# l.lim and u.lim will regulate if the outcome 2 at stage 1 was above/below efficacy boundary

##########################################################


Stage2.S2.O2.Dt<-function(a,b,c2,th1,th2,D,Ds2,l.lim,u.lim,lt)

{

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  Stage1.S2.O2.Tn.Mean<-th2[2]*D1[2,2]       # Truncated normal mean,sd,lower and upper limits
  Stage1.S2.O2.Tn.Sd<-sqrt(D1[2,2])
  Stage1.S2.O2.Tn.Lower<-a*sqrt(D1[2,2])
  Stage1.S2.O2.Tn.Upper<-b*sqrt(D1[2,2])     # upper limit to see if the outcome cross the upper boundary for efficacy


  Stage2.S2.O2.Nor.Mean<-th2[2]*(D2[2,2])     # When the trial continues in the second stage, this information is needed
  Stage2.S2.O2.Nor.Sd<-sqrt((D2[2,2]))        # for the second stage distribition of outcome 1
  Stage2.S2.O2.Nor.Lower<-c2*sqrt(D1[2,2]+D2[2,2])



  Stage2.S2.O2.Int<-function(v)

  {

    return(dtnorm(v,Stage1.S2.O2.Tn.Mean,Stage1.S2.O2.Tn.Sd,lower=Stage1.S2.O2.Tn.Lower)*
             pnorm(Stage2.S2.O2.Nor.Lower,Stage2.S2.O2.Nor.Mean+v,Stage2.S2.O2.Nor.Sd,lower.tail=lt))

  }


  int.v<-integrate(Vectorize(Stage2.S2.O2.Int),lower=l.lim,upper=u.lim)


  return(int.v$value)

}



##########################################################
# Function 52. Stage 2 subgroup 2 distribution
# lt=F will give the rejection probability for subgroup 2 in stage 2
# lt=T will give the probability that the outcome 2 at stage 2 is below the efficacy boundary
##########################################################



Stage2.S2.Reject.Given.S2.Select<-function(a,b,c1,c2,th1,th2,D,Ds2){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  Stage1.S2.O1.Tn.Lower<-a*sqrt(D1[2,1])
  Stage1.S2.O1.Tn.Upper<-b*sqrt(D1[2,1])
  Stage1.S2.O2.Tn.Lower<-a*sqrt(D1[2,2])
  Stage1.S2.O2.Tn.Upper<-b*sqrt(D1[2,2])


  ((Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Upper,u.lim=Inf,lt=F)*
      Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Lower,u.lim=Stage1.S2.O2.Tn.Upper,lt=F))+

      (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Lower,u.lim=Stage1.S2.O1.Tn.Upper,lt=F)*
         Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Upper,u.lim=Inf,lt=F))+

      (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Lower,u.lim=Stage1.S2.O1.Tn.Upper,lt=F)*
         Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Lower,u.lim=Stage1.S2.O2.Tn.Upper,lt=F)))


}



#Stage2.S2.Reject.Given.S2.Select(a=-0.1040146,b=2.2773003,c1=1,c2=1,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))


###########################################################
# Subgroup 2 efficacy in stage 2
#
###########################################################

Stage2.S2.Ef<-function(a,b,c1,c2,th1,th2,D,Ds2)

{
  Stage1.S2.Select(a,b,th1,th2,D)*Stage2.S2.Reject.Given.S2.Select(a,b,c1,c2,th1,th2,D,Ds2)

}


##
##Stage2.S2.Ef(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##

###########################################################
# Subgroup 2 stop for futility in stage 2 given subgroup 2 selected
#
###########################################################



Stage2.S2.Accept.Given.S2.Select<-function(a,b,c1,c2,th1,th2,D,Ds2)

{

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)

  Stage1.S2.O1.Tn.Lower<-a*sqrt(D1[2,1])
  Stage1.S2.O1.Tn.Upper<-b*sqrt(D1[2,1])
  Stage1.S2.O2.Tn.Lower<-a*sqrt(D1[2,2])
  Stage1.S2.O2.Tn.Upper<-b*sqrt(D1[2,2])

  (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Upper,u.lim=Inf,lt=F)*
      Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Lower,u.lim=Stage1.S2.O2.Tn.Upper,lt=T))+
    (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Upper,u.lim=Inf,lt=T)*
       Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Lower,u.lim=Stage1.S2.O2.Tn.Upper,lt=F))+
    (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Upper,u.lim=Inf,lt=T)*
       Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Lower,u.lim=Stage1.S2.O2.Tn.Upper,lt=T))+
    (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Lower,u.lim=Stage1.S2.O1.Tn.Upper,lt=F)*
       Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Upper,u.lim=Inf,lt=T))+
    (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Lower,u.lim=Stage1.S2.O1.Tn.Upper,lt=T)*
       Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Upper,u.lim=Inf,lt=F))+
    (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Lower,u.lim=Stage1.S2.O1.Tn.Upper,lt=T)*
       Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Upper,u.lim=Inf,lt=T))+

    (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Lower,u.lim=Stage1.S2.O1.Tn.Upper,lt=F)*
       Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Lower,u.lim=Stage1.S2.O2.Tn.Upper,lt=T))+
    (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Lower,u.lim=Stage1.S2.O1.Tn.Upper,lt=T)*
       Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Lower,u.lim=Stage1.S2.O2.Tn.Upper,lt=F))+
    (Stage2.S2.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O1.Tn.Lower,u.lim=Stage1.S2.O1.Tn.Upper,lt=T)*
       Stage2.S2.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S2.O2.Tn.Lower,u.lim=Stage1.S2.O2.Tn.Upper,lt=T))


}

#Stage2.S2.Accept.Given.S2.Select(a=-0.1040146,b=2.2773003,c1=1,c2=1,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))




###########################################################
# Subgroup 2 efficacy in stage 2
#
###########################################################

Stage2.S2.Ft<-function(a,b,c1,c2,th1,th2,D,Ds2)

{
  Stage1.S2.Select(a,b,th1,th2,D)*Stage2.S2.Accept.Given.S2.Select(a,b,c1,c2,th1,th2,D,Ds2)

}

##
##Stage2.S2.Ft(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##





##########################################################
# Function 53. Stage 2 subgroup 1 & 2, Outcome 1 distribution
# lt=F will give the rejection probability for subgroup 2
##########################################################


Stage2.S12.O1.Dt<-function(a,b,c1,th1,th2,D,Ds2,l.lim,u.lim,lt){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  mu11=th1[1]*D1[1,1]
  sig11=sqrt(D1[1,1])
  mu21=th2[1]*D1[2,1]
  sig21=sqrt(D1[2,1])

  Stage1.S1.O1.Select.lower=a*sqrt(D1[1,1])
  Stage1.S2.O1.Select.lower=a*sqrt(D1[2,1])

  Stage1.S12.O1.Tn.upper=b*sqrt(D1[1,1]+D1[2,1])


  muS1=((th1[1]*D2[1,1]+th2[1]*D2[2,1])/(D2[1,1]+D2[2,1]))*(D2[1,1]+D2[2,1]) #need to discuss this computation
  sigS1=sqrt(D2[1,1]+D2[2,1])
  Stage2.S12.O1.lower=c1*sqrt(D1[1,1]+D1[2,1]+D2[1,1]+D2[2,1])


  Stage2.S12.O1.Int<-function(y)
  {


    Stage1.S12.O1.Tn.Dt <- function(y)
    {

      integrand <- function(x)
      {

        f1 <- dtnorm(x,mu11,sig11,lower=Stage1.S1.O1.Select.lower)
        f2 <- dtnorm(y-x,mu21,sig21,lower=Stage1.S2.O1.Select.lower)
        return(f1*f2)
      }

      int.x <- integrate(integrand,lower=Stage1.S1.O1.Select.lower,upper=y-Stage1.S2.O1.Select.lower)

      return(int.x$value)
    }


    return(Stage1.S12.O1.Tn.Dt(y)*pnorm(Stage2.S12.O1.lower,muS1+y,sigS1,lower.tail=lt))

    #lt=F will return the probability that the outcome 1 is above the efficacy boundary
    #lt=T will return the probability that the outcome 1 is below the efficacy boundary

  }

  int.w<-integrate(Vectorize(Stage2.S12.O1.Int),lower=l.lim,upper=u.lim)
  # l.lim and u.lim will regulate if the outcome is above/below the efficacy boundary in stage 1

  return(int.w$value)
}



#Stage2.S12.O1.Dt(a=-0.1040146,b=2.2773003,c1=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1),l.lim=-Inf,u.lim=b*sqrt(D1[1,1]+D1[2,1]),lt=F)



##########################################################
# Function 54. Stage 2 subgroup 1 & 2, Outcome 2 distribution
# lt=F will give the rejection probability for subgroup 2
##########################################################



Stage2.S12.O2.Dt<-function(a,b,c2,th1,th2,D,Ds2,l.lim,u.lim,lt){

  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  mu12=th1[2]*D1[1,2]
  sig12=sqrt(D1[1,2])
  mu22=th2[2]*D1[2,2]
  sig22=sqrt(D1[2,2])


  Stage1.S1.O2.Select.lower=a*sqrt(D1[1,2])
  Stage1.S2.O2.Select.lower=a*sqrt(D1[2,2])

  Stage1.S12.O2.Tn.Upper=b*sqrt(D1[1,2]+D1[2,2])

  muS2=((th1[2]*D2[1,2]+th2[2]*D2[2,2])/(D2[1,2]+D2[2,2]))*(D2[1,2]+D2[2,2])
  sigS2=sqrt(D2[1,2]+D2[2,2])
  Stage2.S12.O2.lower=c2*sqrt(D1[1,2]+D1[2,2]+D2[1,2]+D2[2,2])


  Stage2.S12.O2.Int<-function(y)
  {


    Stage1.S12.O2.Tn.Dt <- function(y)
    {

      integrand <- function(x)
      {

        f1 <- dtnorm(x,mu12,sig12,lower=Stage1.S1.O2.Select.lower)
        f2 <- dtnorm(y-x,mu22,sig22,lower=Stage1.S2.O2.Select.lower)
        return(f1*f2)
      }

      int.x <- integrate(integrand,lower=Stage1.S1.O2.Select.lower,upper=y-Stage1.S2.O2.Select.lower)

      return(int.x$value)
    }


    return(Stage1.S12.O2.Tn.Dt(y)*pnorm(Stage2.S12.O2.lower,muS2+y,sigS2,lower.tail=lt))

    #lt=F will return the probability that the outcome 2 is above the efficacy boundary
    #lt=T will return the probability that the outcome 2 is below the efficacy boundary


  }

  int.w<-integrate(Vectorize(Stage2.S12.O2.Int),lower=l.lim,upper=u.lim)

  # l.lim and u.lim will regulate if the outcome is above/below the efficacy boundary in stage 1


  return(int.w$value)
}


##
##Stage2.S12.O2.Dt(a=-0.1040146,b=2.2773003,c=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1),l.lim=-Inf,u.lim=b*sqrt(D1[1,1]+D1[2,1]),lt=F)
##




##########################################################
# Function 55. Stage 2 subgroup 1 & 2 distribution
# lt=F will give the rejection probability for subgroup 1 and 2
##########################################################



Stage2.S12.Reject.Given.S12.Select<-function(a,b,c1,c2,th1,th2,D,Ds2){
  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  Stage1.S12.O1.Tn.Upper=b*sqrt(D1[1,1]+D1[2,1])
  Stage1.S12.O2.Tn.Upper=b*sqrt(D1[1,2]+D1[2,2])


  ((Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O1.Tn.Upper,lt=F)*
      Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S12.O2.Tn.Upper,u.lim=Inf,lt=F))+

      (Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S12.O1.Tn.Upper,u.lim=Inf,lt=F)*
         Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O2.Tn.Upper,lt=F))+

      (Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O1.Tn.Upper,lt=F)*
         Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O2.Tn.Upper,lt=F)))


}

##
##Stage2.S12.Reject.Given.S12.Select(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##










Stage2.S12.Ef<-function(a,b,c1,c2,th1,th2,D,Ds2)

{

  Prob.E15(a,th1,th2,D)*Stage2.S12.Reject.Given.S12.Select(a,b,c1,c2,th1,th2,D,Ds2)
}

##
##Stage2.S12.Ef(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##


# Will work from here. C1 in c1=c2 needed to be changed to c2=c2.
# lower limit should be changed to -Inf
# upper limits should be added in the function

Stage2.S12.Accept.Given.S12.Select<-function(a,b,c1,c2,th1,th2,D,Ds2){
  D1=matrix(D,nrow=2,ncol=2,byrow=T)
  D2=matrix(Ds2,nrow=2,ncol=2,byrow=T)


  Stage1.S12.O1.Tn.Upper=b*sqrt(D1[1,1]+D1[2,1])
  Stage1.S12.O2.Tn.Upper=b*sqrt(D1[1,2]+D1[2,2])



  ((Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S12.O1.Tn.Upper,u.lim=Inf,lt=F)*
      Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O2.Tn.Upper,lt=T))+

      (Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S12.O1.Tn.Upper,u.lim=Inf,lt=T)*
         Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O2.Tn.Upper,lt=F))+

      (Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S12.O1.Tn.Upper,u.lim=Inf,lt=T)*
         Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O2.Tn.Upper,lt=T)))+

    ((Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O1.Tn.Upper,lt=F)*
        Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S12.O2.Tn.Upper,u.lim=Inf,lt=T))+

       (Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O1.Tn.Upper,lt=T)*
          Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S12.O2.Tn.Upper,u.lim=Inf,lt=F))+

       (Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O1.Tn.Upper,lt=T)*
          Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=Stage1.S12.O2.Tn.Upper,u.lim=Inf,lt=T)))+


    ((Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O1.Tn.Upper,lt=F)*
        Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O2.Tn.Upper,lt=T))+

       (Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O1.Tn.Upper,lt=T)*
          Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O2.Tn.Upper,lt=F))+

       (Stage2.S12.O1.Dt(a=a,b=b,c1=c1,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O1.Tn.Upper,lt=T)*
          Stage2.S12.O2.Dt(a=a,b=b,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2,l.lim=-Inf,u.lim=Stage1.S12.O2.Tn.Upper,lt=T)))


}

##
##Stage2.S12.Accept.Given.S12.Select(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##







Stage2.S12.Ft<-function(a,b,c1,c2,th1,th2,D,Ds2){

  Prob.E15(a,th1,th2,D)*Stage2.S12.Accept.Given.S12.Select(a,b,c1,c2,th1,th2,D,Ds2)

}


##
##Stage2.S12.Ft(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##



##########################################################
# Function 44. Stage 2 total rejection probability for efficacy
#
##########################################################


Stage2.Total.Ef<-function(a,b,c1,c2,th1,th2,D,Ds2)

{
  Stage2.S1.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2)+
    Stage2.S2.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2)+
    Stage2.S12.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2)
}

##
##Stage2.Total.Ef(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##

##########################################################
# Function 44. Stage 2 total rejection probability for futility
#
##########################################################


Stage2.Total.Ft<-function(a,b,c1,c2,th1,th2,D,Ds2)

{
  Stage2.S1.Ft(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2)+
    Stage2.S2.Ft(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2)+
    Stage2.S12.Ft(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2)
}


##
##Stage2.Total.Ft(a=-0.1040146,b=2.2773003,c1=2,c2=2,th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1))
##



##########################################################
# Function 45. Computation of boundary values in Stage II
#
##########################################################


Stage2.Boundary.Values<-function(lims2,lims1,th1,th2,D,Ds2,au2,al2,optimize=T){

  a=lims1[1]
  b=lims1[2]
  c1=lims2[1]
  c2=lims2[2]

  if (optimize==T) {

    retval<-(Stage2.Total.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2)-au2)^2+
      (Stage2.Total.Ft(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2)-al2)^2+
      (lims2[2]-lims2[1])^2

  } else{

    retval<-list(TR=Stage2.Total.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2),
                 OR=Stage2.S12.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2),
                 S1=Stage2.S1.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2),
                 S2=Stage2.S2.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2))

  }
  return(retval)

}



##
##optim(c(2,2),Stage2.Boundary.Values,lims1=c(-0.1040146,2.2773003),th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1),au2=0.0003125,al2=0.4996875,optimize=T,method="Nelder-Mead",control=list(maxit=1000))
##


#Stage2.Boundary.Values(lims2=c(1.7,1.7),lims1=c(-0.1040146,2.2773003),th1=c(0,0),th2=c(0,0),D=c(1,1,1,1),Ds2=c(1,1,1,1),au2=0.0003125,al2=0.4996875,optimize=F)


##########################################################
# Function 46. Computation of Optimum information in Stage I
#
##########################################################


Stage2.Inf.Optimization<-function(Ds2,lims2,lims1,th1,th2,D,optimize=T)

{
  a=lims1[1]
  b=lims1[2]
  c1=lims2[1]
  c2=lims2[2]

  if (optimize==T) {

    retval<-(Stage2.Total.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2)-0.405)^2


  } else{

    retval<-list(TR=Stage2.Total.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2),
                 OR=Stage2.S12.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2),
                 S1=Stage2.S1.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2),
                 S2=Stage2.S2.Ef(a=a,b=b,c1=c1,c2=c2,th1=th1,th2=th2,D=D,Ds2=Ds2))

  }

  return(retval)

}
