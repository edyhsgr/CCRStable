##########
#R CODE FOR COHORT CHANGE RATIO-BASED STABLE POPULATION REVIEW - JUST NASCENT WORK
#
#EDDIE HUNSINGER, AUGUST 2019
#https://edyhsgr.github.io/eddieh/
#
#IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, BE SURE TO CITE THE SOURCE
#
#EXAMPLE DATA IS LINKED, SO YOU SHOULD BE ABLE TO SIMPLY COPY ALL AND PASTE INTO R
#
#THERE IS NO WARRANTY FOR THIS CODE
#THIS CODE HAS NOT BEEN TESTED AT ALL-- PLEASE LET ME KNOW IF YOU FIND ANY PROBLEMS (edyhsgr@gmail.com)
##########

##########
#####
#DIMENSIONS
SIZE<-18
STEP<-0
#####

#####
#IMPOSED TFR OPTION
ImposedTFR<-1.5
ffab<-.4886
UseImposedTFR<-"NO"
#####

#####
#SELECTION OF US CENSUS BUREAU 2000, 2005, 2010 INTERCENSAL POPULATION ESTIMATES BY AGE (RELEASED IN 2012) (https://www.census.gov/data/tables/time-series/demo/popest/intercensal-2000-2010-state.html)
K<-read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/AgeSex200020052010_CA_USCBIntercensal_2012.csv",header=TRUE,sep=",")
TMinusOneAge<-K$CA_F_2000
TMinusZeroAge<-K$CA_F_2005
#####

#####
#CALCULATIONS
Ratios<-array(0,dim=length(TMinusOneAge))
for (i in 2:length(TMinusOneAge)) {Ratios[i]<-TMinusZeroAge[i]/TMinusOneAge[i-1]}
Ratios[1]<-TMinusZeroAge[1]/sum(TMinusOneAge[4:9])

S<-array(0,c(SIZE,SIZE))
S<-rbind(0,cbind(diag(Ratios[2:SIZE]),0))

B<-0*S
B[1,4:9]<-Ratios[1]
ImpliedTFR<-sum(B)*(1/.4886) #(Need to check - just going off intuition in this version)
if(UseImposedTFR=="YES") {B[1,4:10]<-(ImposedTFR/ImpliedTFR)*B[1,4:10]} #(Need to check - just going off intuition in this version)

A<-B+S
#####

#####
#CALCULATORS
CCRProject<-function(A,TMinusZeroAge,STEP){
TMinusZeroAge<-A%*%TMinusZeroAge
TMinusZeroAge<-data.frame(TMinusZeroAge)
return(c(TMinusZeroAge,STEP=STEP+1))
}

CCRNew<-CCRProject(A,TMinusZeroAge,STEP)

while(CCRNew$STEP<51) {CCRNew<-CCRProject(A,CCRNew$TMinusZeroAge,CCRNew$STEP)}
#####

#####
#GRAPHS
plot(K$CA_F_2000/sum(K$CA_F_2000),type="l",col=1,ylim=c(0,.1),xlab="Age group number",ylab="Population (proportional)")
lines(K$CA_F_2005/sum(K$CA_F_2005),col=2)
lines(CCRNew$TMinusZeroAge[]/sum(CCRNew$TMinusZeroAge),col=3)
#####
##########

