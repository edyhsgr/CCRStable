##########
#R CODE FOR COHORT CHANGE RATIO-BASED STABLE POPULATION REVIEW SHINY APP - JUST NASCENT WORK, STILL FIGURING/LEARNING AND CHECKING
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
##INPUTS

#DIMENSIONS
SIZE<-18
STEPS<-5
STEPSSTABLE<-100
CURRENTSTEP<-0
CURRENTSTEPSTABLE<-0
PROJECTIONYEAR<-STEPS*5+2015
FERTWIDTH<-34

#IMPOSED TFR OPTION
ImposedTFR<-2.1
ffab<-.4886
UseImposedTFR<-"NO"

#SELECTION OF US CENSUS BUREAU VINTAGE 2018 POPULATION ESTIMATES BY AGE (https://www.census.gov/programs-surveys/popest/data/tables.html)
K<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/PEP_2018_PEPAGE_FemaleByCounty.csv",header=TRUE,sep=","))
TMinusOneAgeInit<-subset(K,Year==2010)
TMinusOneAgeInit<-TMinusOneAgeInit$Alameda
TMinusOneAge<-TMinusOneAgeInit

TMinusZeroAgeInit<-subset(K,Year==2015)
TMinusZeroAgeInit<-TMinusZeroAgeInit$Alameda
TMinusZeroAge<-TMinusZeroAgeInit

#####
##CALCULATIONS
Ratios<-array(0,dim=length(TMinusOneAge))
for (i in 2:length(TMinusOneAge)) {Ratios[i]<-TMinusZeroAge[i]/TMinusOneAge[i-1]}
Ratios[1]<-TMinusZeroAge[1]/sum(TMinusOneAge[4:10])

S<-array(0,c(SIZE,SIZE))
S<-rbind(0,cbind(diag(Ratios[2:SIZE]),0))

B<-0*S
B[1,4:10]<-Ratios[1]
ImpliedTFR<-sum(B)/ffab #(Need to work with/check)

ImpliedTFR2010<-(TMinusOneAgeInit[1]/ffab/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH
ImpliedTFR2015<-(TMinusZeroAgeInit[1]/ffab/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH

A<-B+S

CCRProject<-function(A,TMinusZeroAge,CURRENTSTEP){
TMinusZeroAge<-A%*%TMinusZeroAge
if(UseImposedTFR=="YES") {TMinusZeroAge[1]<-ImposedTFR*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*ffab}
TMinusZeroAge<-data.frame(TMinusZeroAge)
return(c(TMinusZeroAge,CURRENTSTEP=CURRENTSTEP+1))
}

CCRNew<-CCRProject(A,TMinusZeroAge,CURRENTSTEP)
while(CCRNew$CURRENTSTEP<STEPS+1) {CCRNew<-CCRProject(A,CCRNew$TMinusZeroAge,CCRNew$CURRENTSTEP)}
ImpliedTFRNew<-(CCRNew$TMinusZeroAge[1]/ffab/5)/sum(CCRNew$TMinusZeroAge[4:10])*FERTWIDTH

TMinusZeroAge<-TMinusZeroAgeInit
CCRStable<-CCRProject(A,TMinusZeroAge,CURRENTSTEPSTABLE)
while(CCRStable$CURRENTSTEP<STEPSSTABLE+1) {CCRStable<-CCRProject(A,CCRStable$TMinusZeroAge,CCRStable$CURRENTSTEP)}
ImpliedTFRStable<-(CCRStable$TMinusZeroAge[1]/ffab/5)/sum(CCRStable$TMinusZeroAge[4:10])*FERTWIDTH

TotalNew<-round(sum(CCRNew$TMinusZeroAge))
TotalTMinusOneInit<-sum(TMinusOneAgeInit)
TotalTMinusZeroInit<-sum(TMinusZeroAgeInit)

#####
##GRAPHS (SOME ~HACKY LABELING SO MAY NOT RENDER WELL)
agegroups<-c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85+")
plot(TMinusOneAgeInit/sum(TMinusOneAgeInit),type="l",col="orange",main="Female Population",ylim=c(0,.1),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)
lines(TMinusZeroAgeInit/sum(TMinusZeroAgeInit),col="blue",lwd=4)
lines(CCRNew$TMinusZeroAge[]/sum(CCRNew$TMinusZeroAge),col="dark green",lty=1,lwd=4)
lines(CCRStable$TMinusZeroAge[]/sum(CCRStable$TMinusZeroAge),col="dark green",lty=2,lwd=2)
mtext(side=1,"Age groups",line=4)
axis(side=1,at=1:SIZE,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2)
legend(11.5, .10, legend=c("2010 (estimate)","2015 (estimate)",c(PROJECTIONYEAR),"Stable"),
       col=c("orange","blue","dark green","dark green"), lty=c(1,1,1,2),lwd=c(4,4,4,2),cex=1.2)
mtext(side=1,c("(projection)"),line=-28.25,adj=.905,cex=1.2)

mtext(side=1,c("Total 2010:"),line=-12,adj=.125,col="orange")
mtext(side=1,c(TotalTMinusOneInit),line=-12,adj=.3,col="orange")

mtext(side=1,c("Total 2015:"),line=-11,adj=.125,col="blue")
mtext(side=1,c(TotalTMinusZeroInit),line=-11,adj=.3,col="blue")

mtext(side=1,c("Total "),line=-10,adj=.117,col="dark green")
mtext(side=1,c(PROJECTIONYEAR),line=-10,adj=.18,col="dark green")
mtext(side=1,c(":"),line=-10,adj=.225,col="dark green")
mtext(side=1,c(TotalNew),line=-10,adj=.3,col="dark green")

mtext(side=1,c("iTFR 2010:"),line=-8,adj=.13,col="orange")
mtext(side=1,c(round(ImpliedTFR2010,2)),line=-8,adj=.29,col="orange")

mtext(side=1,c("iTFR 2015:"),line=-7,adj=.13,col="blue")
mtext(side=1,c(round(ImpliedTFR2015,2)),line=-7,adj=.29,col="blue")

mtext(side=1,c("iTFR"),line=-6,adj=.12,col="dark green")
mtext(side=1,c(PROJECTIONYEAR),line=-6,adj=.185,col="dark green")
mtext(side=1,c(":"),line=-6,adj=.23,col="dark green")
mtext(side=1,c(round(ImpliedTFRNew,2)),line=-6,adj=.29,col="dark green")
##########
