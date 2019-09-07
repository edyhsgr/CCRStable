##########
#R CODE FOR COHORT CHANGE RATIO-BASED STABLE POPULATION REVIEW SHINY APP - JUST NASCENT WORK, STILL FIGURING/LEARNING AND CHECKING
#
#EDDIE HUNSINGER, AUGUST 2019 (UPDATED SEPTEMBER 2019)
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
SIZE<-36
HALFSIZE<-SIZE/2
STEPS<-1
STEPSSTABLE<-1000
CURRENTSTEP<-0
CURRENTSTEPSTABLE<-0
PROJECTIONYEAR<-STEPS*5+2015
FERTWIDTH<-35

#SELECTING RATIOS BASIS
FirstYear<-3
SecondYear<-3+5

#IMPOSED TFR OPTION
ImposedTFR<-2.1
ffab<-.4886
UseImposedTFR<-"NO"

##ADJUST BY MIGRATION OPTION
NetMigrationAdjustLevel<-1/100
NetMigrationAdjust<-"YES"

#SELECT BY SEX
SelectBySex<-"Total"

#DATA
#SELECTION OF US CENSUS BUREAU VINTAGE 2018 POPULATION ESTIMATES BY AGE
#https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/counties/asrh/cc-est2018-alldata-06.csv 
#https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2018/
K<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/cc-est2018-alldata-06_Extract.csv",header=TRUE,sep=","))

Name<-paste("Alameda County")

TMinusOneAgeInit_F<-subset(K,CTYNAME=="Alameda County" & YEAR==3 & AGEGRP>0)
TMinusOneAgeInit_F<-TMinusOneAgeInit_F$TOT_FEMALE
TMinusOneAge_F<-TMinusOneAgeInit_F

TMinusOneAgeInit_M<-subset(K,CTYNAME=="Alameda County" & YEAR==3 & AGEGRP>0)
TMinusOneAgeInit_M<-TMinusOneAgeInit_M$TOT_MALE
TMinusOneAge_M<-TMinusOneAgeInit_M

TMinusOneAge<-TMinusOneAgeInit<-c(TMinusOneAge_F,TMinusOneAge_M)

TMinusOneAgeInitRatios_F<-subset(K,CTYNAME=="Alameda County" & YEAR==FirstYear & AGEGRP>0)
TMinusOneAgeInitRatios_F<-TMinusOneAgeInitRatios_F$TOT_FEMALE
TMinusOneAgeRatios_F<-TMinusOneAgeInitRatios_F

TMinusOneAgeInitRatios_M<-subset(K,CTYNAME=="Alameda County" & YEAR==FirstYear & AGEGRP>0)
TMinusOneAgeInitRatios_M<-TMinusOneAgeInitRatios_M$TOT_MALE
TMinusOneAgeRatios_M<-TMinusOneAgeInitRatios_M

TMinusOneAgeRatios<-TMinusOneAgeInitRatios<-c(TMinusOneAgeRatios_F,TMinusOneAgeRatios_M)

TMinusZeroAgeInit_F<-subset(K,CTYNAME=="Alameda County" & YEAR==8 & AGEGRP>0)
TMinusZeroAgeInit_F<-TMinusZeroAgeInit_F$TOT_FEMALE
TMinusZeroAge_F<-TMinusZeroAgeInit_F

TMinusZeroAgeInit_M<-subset(K,CTYNAME=="Alameda County" & YEAR==8 & AGEGRP>0)
TMinusZeroAgeInit_M<-TMinusZeroAgeInit_M$TOT_MALE
TMinusZeroAge_M<-TMinusZeroAgeInit_M

TMinusZeroAge<-TMinusZeroAgeInit<-c(TMinusZeroAge_F,TMinusZeroAge_M)

TMinusZeroAgeInitRatios_F<-subset(K,CTYNAME=="Alameda County" & YEAR==SecondYear & AGEGRP>0)
TMinusZeroAgeInitRatios_F<-TMinusZeroAgeInitRatios_F$TOT_FEMALE
TMinusZeroAgeRatios_F<-TMinusZeroAgeInitRatios_F

TMinusZeroAgeInitRatios_M<-subset(K,CTYNAME=="Alameda County" & YEAR==SecondYear & AGEGRP>0)
TMinusZeroAgeInitRatios_M<-TMinusZeroAgeInitRatios_M$TOT_MALE
TMinusZeroAgeRatios_M<-TMinusZeroAgeInitRatios_M

TMinusZeroAgeRatios<-TMinusZeroAgeInitRatios<-c(TMinusZeroAgeRatios_F,TMinusZeroAgeRatios_M)

#SOME GENERIC CA MIGRATION DATA (PLACEHOLDER)
Migration<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/AGenericMigrationProfile_CA_2013to2017ACS.csv",header=TRUE,sep=","))
Migration<-c(Migration$CA_F,Migration$CA_M)

#####
##CALCULATIONS
Ratios<-array(0,dim=length(TMinusOneAge))
for (i in 2:length(TMinusOneAge)) 
	{Ratios[i]<-TMinusZeroAge[i]/TMinusOneAge[i-1]}
Ratios[1]<-(TMinusZeroAge[1]+TMinusZeroAge[HALFSIZE+1])/sum(TMinusOneAge[4:10])

S_F<-array(0,c(HALFSIZE,HALFSIZE))
S_F<-rbind(0,cbind(diag(Ratios[2:(HALFSIZE)]),0))
B_F<-0*S_F
B_F[1,4:10]<-Ratios[1]*ffab
A_F<-B_F+S_F

S_M<-array(0,c(HALFSIZE,HALFSIZE))
S_M<-rbind(0,cbind(diag(Ratios[20:SIZE]),0))
B_M<-0*S_M
B_M[1,4:10]<-Ratios[1]*(1-ffab)

A_Zero<-array(0,c(HALFSIZE,HALFSIZE))

Acolone<-cbind(A_F,A_Zero)
Acoltwo<-cbind(B_M,S_M)
A<-rbind(Acolone,Acoltwo)

SumFirstRows<-(sum(B_F)+sum(B_M)) #(May work with)

ImpliedTFR2010<-((TMinusOneAgeInit[1]+TMinusOneAgeInit[HALFSIZE+1])/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH
ImpliedTFR2015<-((TMinusZeroAgeInit[1]+TMinusZeroAgeInit[HALFSIZE+1])/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH

CCRProject<-function(A,TMinusZeroAge,CURRENTSTEP)
	{TMinusOneAgeNew<-data.frame(TMinusZeroAge) 
		if(CURRENTSTEP>0){
			TMinusZeroAge<-A%*%TMinusZeroAge
				if(NetMigrationAdjust=="YES")
				{TMinusZeroAge<-NetMigrationAdjustLevel*5*sum(TMinusOneAgeNew)*Migration+TMinusZeroAge}
				}
		if(CURRENTSTEP>0){
			if(UseImposedTFR=="YES") 
				{TMinusZeroAge[1]<-ImposedTFR*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*ffab}
				}
		if(CURRENTSTEP>0){
			if(UseImposedTFR=="YES") 
				{TMinusZeroAge[HALFSIZE+1]<-ImposedTFR*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*(1-ffab)}
				}
	TMinusZeroAge<-data.frame(TMinusZeroAge)
	return(c(TMinusZeroAge,TMinusOneAge=TMinusOneAgeNew,CURRENTSTEP=CURRENTSTEP+1))}

CCRNew<-CCRProject(A,TMinusZeroAge,CURRENTSTEP)
while(CCRNew$CURRENTSTEP<STEPS+1) {CCRNew<-CCRProject(A,CCRNew$TMinusZeroAge,CCRNew$CURRENTSTEP)}
ImpliedTFRNew<-((CCRNew$TMinusZeroAge[1]+CCRNew$TMinusZeroAge[HALFSIZE+1])/5)/sum(CCRNew$TMinusZeroAge[4:10])*FERTWIDTH

CCRatios<-array(0,length(TMinusOneAge)+1)
for (i in 2:length(CCRatios)) {CCRatios[i]<-CCRNew$TMinusZeroAge[i]/CCRNew$TMinusOneAge[i-1]}
CCRatiosF<-CCRatios[2:18]
CCRatiosM<-CCRatios[20:36]

TMinusZeroAge<-TMinusZeroAgeInit
CCRStable<-CCRProject(A,TMinusZeroAge,0)
while(CCRStable$CURRENTSTEP<STEPSSTABLE+1) {CCRStable<-CCRProject(A,CCRStable$TMinusZeroAge,CCRStable$CURRENTSTEP)}
ImpliedTFRStable<-((CCRStable$TMinusZeroAge[1]+CCRStable$TMinusZeroAge[HALFSIZE+1])/5)/sum(CCRStable$TMinusZeroAge[4:10])*FERTWIDTH

#####
##TABLING
NewAge_F<-CCRNew$TMinusZeroAge[1:HALFSIZE]
StableAge_F<-CCRStable$TMinusZeroAge[1:HALFSIZE]
TMinusOneAgeInit_F<-TMinusOneAgeInit[1:HALFSIZE]
TMinusZeroAgeInit_F<-TMinusZeroAgeInit[1:HALFSIZE]

NewAge_M<-CCRNew$TMinusZeroAge[(HALFSIZE+1):SIZE]
StableAge_M<-CCRStable$TMinusZeroAge[(HALFSIZE+1):SIZE]
TMinusOneAgeInit_M<-TMinusOneAgeInit[(HALFSIZE+1):SIZE]
TMinusZeroAgeInit_M<-TMinusZeroAgeInit[(HALFSIZE+1):SIZE]

NewAge_T<-NewAge_F+NewAge_M
StableAge_T<-StableAge_F+StableAge_M
TMinusOneAgeInit_T<-TMinusOneAgeInit_F+TMinusOneAgeInit_M
TMinusZeroAgeInit_T<-TMinusZeroAgeInit_F+TMinusZeroAgeInit_M

NewAge<-array(c(NewAge_T,NewAge_F,NewAge_M),c(HALFSIZE,3))
StableAge<-array(c(StableAge_T,StableAge_F,StableAge_M),c(HALFSIZE,3))
TMinusOneAgeInit<-array(c(TMinusOneAgeInit_T,TMinusOneAgeInit_F,TMinusOneAgeInit_M),c(HALFSIZE,3))
TMinusZeroAgeInit<-array(c(TMinusZeroAgeInit_T,TMinusZeroAgeInit_F,TMinusZeroAgeInit_M),c(HALFSIZE,3))


#####
##GRAPHS (SOME ~HACKY LABELING SO MAY [LIKELY] NOT RENDER WELL)
#FIRST GRAPH
agegroups<-c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85+")
if(SelectBySex=="Total") 
	{plot(TMinusOneAgeInit[,1]/sum(TMinusOneAgeInit[,1]),type="l",col="orange",main=paste(text=c("Alameda, ",SelectBySex),collapse=""),ylim=c(0,.1),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
if(SelectBySex=="Female") 
	{plot(TMinusOneAgeInit[,2]/sum(TMinusOneAgeInit[,2]),type="l",col="orange",main=paste(text=c("Alameda, ",SelectBySex),collapse=""),ylim=c(0,.1),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
if(SelectBySex=="Male") 
	{plot(TMinusOneAgeInit[,3]/sum(TMinusOneAgeInit[,3]),type="l",col="orange",main=paste(text=c("Alameda, ",SelectBySex),collapse=""),ylim=c(0,.1),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}

if(SelectBySex=="Total") 
	{lines(TMinusZeroAgeInit[,1]/sum(TMinusZeroAgeInit[,1]),col="blue",lwd=4)}
if(SelectBySex=="Female") 
	{lines(TMinusZeroAgeInit[,2]/sum(TMinusZeroAgeInit[,2]),col="blue",lwd=4)}
if(SelectBySex=="Male") 
	{lines(TMinusZeroAgeInit[,3]/sum(TMinusZeroAgeInit[,3]),col="blue",lwd=4)}

if(SelectBySex=="Total") 
	{lines(NewAge[,1]/sum(NewAge[,1]),col="dark green",lty=1,lwd=4)}
if(SelectBySex=="Female") 
	{lines(NewAge[,2]/sum(NewAge[,2]),col="dark green",lty=1,lwd=4)}
if(SelectBySex=="Male") 
	{lines(NewAge[,3]/sum(NewAge[,3]),col="dark green",lty=1,lwd=4)}

if(SelectBySex=="Total") 
	{lines(StableAge[,1]/sum(StableAge[,1]),col="dark green",lty=3,lwd=2)}
if(SelectBySex=="Female") 
	{lines(StableAge[,2]/sum(StableAge[,2]),col="dark green",lty=3,lwd=2)}
if(SelectBySex=="Male") 
	{lines(StableAge[,3]/sum(StableAge[,3]),col="dark green",lty=3,lwd=2)}

mtext(side=1,"Age groups",line=4,cex=.75)
axis(side=1,at=1:HALFSIZE,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2)
legend(11.5, .12, legend=c("2010 (estimate)","2015 (estimate)",paste(c(PROJECTIONYEAR),"(projection)"),"Stable"),
       col=c("orange","blue","dark green","black"), lty=c(1,1,1,3),lwd=c(4,4,4,1.5),cex=1.2)

mtext(side=1,c("Sum 2010:"),line=-12,adj=.125,col="orange")
if(SelectBySex=="Total") 
	{mtext(side=1,c(sum(TMinusOneAgeInit[,1])),line=-12,adj=.3,col="orange")}
if(SelectBySex=="Female") 
	{mtext(side=1,c(sum(TMinusOneAgeInit[,2])),line=-12,adj=.3,col="orange")}
if(SelectBySex=="Male") 
	{mtext(side=1,c(sum(TMinusOneAgeInit[,3])),line=-12,adj=.3,col="orange")}

mtext(side=1,c("Sum 2015:"),line=-11,adj=.125,col="blue")
if(SelectBySex=="Total") 
	{mtext(side=1,c(sum(TMinusZeroAgeInit[,1])),line=-11,adj=.3,col="blue")}
if(SelectBySex=="Female") 
	{mtext(side=1,c(sum(TMinusZeroAgeInit[,2])),line=-11,adj=.3,col="blue")}
if(SelectBySex=="Male") 
	{mtext(side=1,c(sum(TMinusZeroAgeInit[,3])),line=-11,adj=.3,col="blue")}

mtext(side=1,c("Sum "),line=-10,adj=.117,col="dark green")
mtext(side=1,paste(c(PROJECTIONYEAR,": "),collapse=""),line=-10,adj=.18,col="dark green")
mtext(side=1,c(":"),line=-10,adj=.225,col="dark green")
if(SelectBySex=="Total") 
	{mtext(side=1,c(round(sum(NewAge[,1]))),line=-10,adj=.3,col="dark green")}
if(SelectBySex=="Female") 
	{mtext(side=1,c(round(sum(NewAge[,2]))),line=-10,adj=.3,col="dark green")}
if(SelectBySex=="Male") 
	{mtext(side=1,c(round(sum(NewAge[,3]))),line=-10,adj=.3,col="dark green")}

mtext(side=1,c("iTFR 2010:"),line=-8,adj=.13,col="orange")
mtext(side=1,c(round(ImpliedTFR2010,2)),line=-8,adj=.29,col="orange")

mtext(side=1,c("iTFR 2015:"),line=-7,adj=.13,col="blue")
mtext(side=1,c(round(ImpliedTFR2015,2)),line=-7,adj=.29,col="blue")

mtext(side=1,c("iTFR"),line=-6,adj=.12,col="dark green")
mtext(side=1,c(PROJECTIONYEAR),line=-6,adj=.185,col="dark green")
mtext(side=1,c(":"),line=-6,adj=.23,col="dark green")
mtext(side=1,c(round(ImpliedTFRNew,2)),line=-6,adj=.29,col="dark green")

if(SelectBySex=="Total") {LASTGROWTHRATE<-paste(text=c("R (2010 to 2015):  ", round(log(sum(TMinusZeroAgeInit[,1])/sum(TMinusOneAgeInit[,1]))/5*100,2)),collapse="")}
if(SelectBySex=="Male") {LASTGROWTHRATE<-paste(text=c("R (2010 to 2015):  ", round(log(sum(TMinusZeroAgeInit[,1])/sum(TMinusOneAgeInit[,1]))/5*100,2)),collapse="")}
if(SelectBySex=="Female") {LASTGROWTHRATE<-paste(text=c("R (2010 to 2015):  ", round(log(sum(TMinusZeroAgeInit[,1])/sum(TMinusOneAgeInit[,1]))/5*100,2)),collapse="")}
mtext(side=1,c(LASTGROWTHRATE),line=-4,adj=.15,col="blue")

if(SelectBySex=="Total") {GROWTHRATE<-paste(text=c("R (2015 to ",PROJECTIONYEAR,"):  ", round(log(sum(NewAge[,1])/sum(TMinusZeroAgeInit[,1]))/(STEPS*5)*100,2)),collapse="")}
if(SelectBySex=="Female") {GROWTHRATE<-paste(text=c("R (2015 to ",PROJECTIONYEAR,"):  ", round(log(sum(NewAge[,2])/sum(TMinusZeroAgeInit[,2]))/(STEPS*5)*100,2)),collapse="")}
if(SelectBySex=="Male") {GROWTHRATE<-paste(text=c("R (2015 to ",PROJECTIONYEAR,"):  ", round(log(sum(NewAge[,3])/sum(TMinusZeroAgeInit[,3]))/(STEPS*5)*100,2)),collapse="")}
mtext(side=1,c(GROWTHRATE),line=-3,adj=.15,col="dark green")

if(SelectBySex=="Total") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ", round(log(sum(StableAge[,1])/sum(TMinusZeroAgeInit[,1]))/(STEPSSTABLE*5)*100,2)),collapse="")}
if(SelectBySex=="Female") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ", round(log(sum(StableAge[,2])/sum(TMinusZeroAgeInit[,2]))/(STEPSSTABLE*5)*100,2)),collapse="")}
if(SelectBySex=="Male") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ", round(log(sum(StableAge[,3])/sum(TMinusZeroAgeInit[,3]))/(STEPSSTABLE*5)*100,2)),collapse="")}
mtext(side=1,c(STABLEGROWTHRATE),line=-2,adj=.15,col="black")

##SECOND GRAPH
#agegroups2<-c("5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
#plot(Ratios[2:18],type="l",col="dodger blue",main=paste(text=c("Applied Cohort Change Ratios, ",PROJECTIONYEAR-5," to ",PROJECTIONYEAR),collapse=""),ylim=c(.5,1.75),axes=FALSE,xlab="",ylab="Ratio",lwd=4)
#lines(Ratios[20:36],type="l",col="gold",lwd=4)
#lines(CCRatiosF,type="l",col="dodger blue",lty=2,lwd=2)
#lines(CCRatiosM,type="l",col="gold",lty=2,lwd=2)
#mtext(side=1,"Age groups",line=4,cex=.75)
#axis(side=1,at=1:(HALFSIZE-1),labels=agegroups2,las=2,cex.axis=0.9)
#axis(side=2)
#legend(8,1.75, legend=c("Female","Male", "Female, with migration adjustment","Male, with migration adjustment"),
#       col=c("dodger blue","gold","dodger blue","gold"), lty=c(1,1,2,2),lwd=c(4,4,2,2),cex=1.2)

#THIRD GRAPH
#barplot(NewAge_F,horiz=T,names=agegroups,space=0,xlim=c(max(TMinusZeroAgeInit[,2])*2,0),col="dodgerblue",las=1,main=paste(text=c("Female, ",PROJECTIONYEAR),collapse=""))

#FOURTH GRAPH
#barplot(NewAge_M,horiz=T,names=FALSE,space=0,xlim=c(0,max(TMinusZeroAgeInit[,2])*2),col="gold",main=paste(text=c("Male, ",PROJECTIONYEAR),collapse=""))
##########

