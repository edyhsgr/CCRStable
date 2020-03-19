##########
#R CODE FOR COHORT CHANGE RATIO-BASED STABLE POPULATION REVIEW SHINY APP
#
#EDDIE HUNSINGER, AUGUST 2019 (UPDATED MARCH 2020)
#https://edyhsgr.github.io/eddieh/
#
#IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, BE SURE TO CITE THE SOURCE
#This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 International License (more information: https://creativecommons.org/licenses/by-sa/3.0/igo/).
#
#THERE IS NO WARRANTY FOR THIS CODE
#THIS CODE HAS NOT BEEN TESTED AT ALL-- PLEASE LET ME KNOW IF YOU FIND ANY PROBLEMS (edyhsgr@gmail.com)
##########

library(shiny)
ui<-fluidPage(

	tags$h3("Cohort Change Ratio-Based Stable Population Review Shiny App"),
	p("Related information and ",
	tags$a(href="https://www.r-project.org/", "R"),
	"code available at: ",
	tags$a(href="https://github.com/edyhsgr/CCRStable", 
	"CCRStable GitHub Repository")
),
  
hr(),

sidebarLayout(
sidebarPanel(

 selectInput("County", "County",
c(
"Alameda"="Alameda County",
"Alpine"="Alpine County",
"Amador"="Amador County",
"Butte"="Butte County",
"Calaveras"="Calaveras County",
"Colusa"="Colusa County",
"Contra Costa"="Contra Costa County",
"Del Norte"="Del Norte County",
"El Dorado"="El Dorado County",
"Fresno"="Fresno County",
"Glenn"="Glenn County",
"Humboldt"="Humboldt County",
"Imperial"="Imperial County",
"Inyo"="Inyo County",
"Kern"="Kern County",
"Kings"="Kings County",
"Lake"="Lake County",
"Lassen"="Lassen County",
"Los Angeles"="Los Angeles County",
"Madera"="Madera County",
"Marin"="Marin County",
"Mariposa"="Mariposa County",
"Mendocino"="Mendocino County",
"Merced"="Merced County",
"Modoc"="Modoc County",
"Mono"="Mono County",
"Monterey"="Monterey County",
"Napa"="Napa County",
"Nevada"="Nevada County",
"Orange"="Orange County",
"Placer"="Placer County",
"Plumas"="Plumas County",
"Riverside"="Riverside County",
"Sacramento"="Sacramento County",
"San Benito"="San Benito County",
"San Bernardino"="San Bernardino County",
"San Diego"="San Diego County",
"San Francisco"="San Francisco County",
"San Joaquin"="San Joaquin County",
"San Luis Obispo"="San Luis Obispo County",
"San Mateo"="San Mateo County",
"Santa Barbara"="Santa Barbara County",
"Santa Clara"="Santa Clara County",
"Santa Cruz"="Santa Cruz County",
"Shasta"="Shasta County",
"Sierra"="Sierra County",
"Siskiyou"="Siskiyou County",
"Solano"="Solano County",
"Sonoma"="Sonoma County",
"Stanislaus"="Stanislaus County",
"Sutter"="Sutter County",
"Tehama"="Tehama County",
"Trinity"="Trinity County",
"Tulare"="Tulare County",
"Tuolumne"="Tuolumne County",
"Ventura"="Ventura County",
"Yolo"="Yolo County",
"Yuba"="Yuba County"
),
),

selectInput("Sex", "Sex",
c(
"Total"="Total",
"Female"="Female",
"Male"="Male"
),
),

numericInput("STEP","Project to (year)",2030,2020,3000,step=5),

selectInput("RatiosFrom", "Using ratios from",
c(
"2010 to 2015"="3",
"2011 to 2016"="4",
"2012 to 2017"="5",
"2013 to 2018"="6"
),
),

hr(),

selectInput("ImposeTFR", "Impose iTFR?",
c(
"No"="NO",
"Yes"="YES"
),
),

numericInput("ImposedTFR","If Yes, iTFR level",2.1,0,10,step=.1),

hr(),

numericInput("NetMigrationAdjustLevel","Net migration adjustment (annual, percent of population)",0,-25,25,step=.1),

hr(),

selectInput("ImputeMort", "Impute mortality?",
c(
"Yes"="YES",
"No"="NO"
),
),

numericInput("BAStart","If yes, Brass' model alpha for First projection step...",.03,-2,2,step=.03),
numericInput("BAEnd","...and Brass' model alpha for Last projection step",.12,-2,2,step=.03),

hr(),

p("This interface was made with ",
tags$a(href="https://shiny.rstudio.com/", 
	"Shiny for R."),
       
tags$a(href="https://edyhsgr.github.io/eddieh/", 
	"Eddie Hunsinger,"), 

"August 2019 (updated October 2019)."),

p("Population estimates inputs from ",
tags$a(href="https://www.census.gov/programs-surveys/popest.html", 
	"US Census Bureau Vintage 2018 Population Estimates.")),

p(" More information on cohort change ratios, including a chapter on stable population: ",
tags$a(href="https://www.worldcat.org/title/cohort-change-ratios-and-their-applications/oclc/988385033", 
	"Baker, Swanson, Tayman, and Tedrow (2017)."),

p("More information on iTFR: ",
tags$a(href="https://osf.io/adu98/", 
	"Hauer and Schmertmann (2019)"),
	" and ",
tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0067226", 
	"Hauer, Baker, and Brown (2013).")),

p("Slides with background thoughts on adjusting net migration: ",
tags$a(href="https://edyhsgr.github.io/eddieh/documents/ProjPresentation.pdf", 
	"Hunsinger (2007)."),

"Migration by age over time comparisons from Alaska data: ",
tags$a(href="http://shiny.demog.berkeley.edu/eddieh/AKPFDMigrationReview/", 
	"Hunsinger (2018)."),

"Interface with net migration adjustment examples and comparisons: ",
tags$a(href="http://shiny.demog.berkeley.edu/eddieh/NMAdjustCompare/", 
	"Hunsinger (2019)."),

"Migration adjustment profile was made from the US Census Bureau's 2013 to 2017 
American Community Survey Public Use Microdata Sample, accessed via the ", 
tags$a(href="https://usa.ipums.org/usa/", 
	"IPUMS USA, University of Minnesota.")),

tags$a(href="https://twitter.com/ApplDemogToolbx/status/1079286699941752832", 
	"Graph of e0 and Brass' relational life table alpha by US state."),

"Model life table (0.0 alpha) is the 5x5 2010 to 2014 life table for California from the ",
tags$a(href="https://usa.mortality.org/index.php", 
	"United States Mortality Database.")),

width=3
),

mainPanel(
	
	plotOutput("plots")
))
)

##READING EXTERNAL DATA IN
#DATA (CENSUS BUREAU VINTAGE 2018 POPULATION ESTIMATES BY DEMOGRAPHIC CHARACTERISTICS)
	#https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/counties/asrh/cc-est2018-alldata-06.csv 
	#https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2018/
K<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/cc-est2018-alldata-06_Extract.csv",header=TRUE,sep=","))

#CENSUS ACS (via IPUMS) CA MIGRATION DATA (GENERIC)
Migration<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/AGenericMigrationProfile_CA_2013to2017ACS.csv",header=TRUE,sep=","))
Migration<-c(Migration$CA_F,Migration$CA_M)

#USMD CA SURVIVAL DATA (GENERIC)
lt<-read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/lt_CA_USMD2010to2014.csv",header=TRUE,sep=",")
lxF<-lt$lx_Female/100000
lxM<-lt$lx_Male/100000
lxT<-lt$lx_Both/100000
lxF<-c(lxF[1],lxF[3:24])
lxM<-c(lxM[1],lxM[3:24])
lxT<-c(lxT[1],lxT[3:24])

server<-function(input, output) {	
	output$plots<-renderPlot({
par(mfrow=c(2,2))

##########
#####
##SCRIPT INPUTS

options(scipen=999)

#DIMENSIONS
SIZE<-36
HALFSIZE<-SIZE/2
STEPS<-(input$STEP-2015)/5
STEPSSTABLE<-STEPS+1000
CURRENTSTEP<-0
CURRENTSTEPSTABLE<-0
PROJECTIONYEAR<-STEPS*5+2015
FERTWIDTH<-35

#SELECTING RATIOS BASIS
FirstYear<-strtoi(input$RatiosFrom)
SecondYear<-strtoi(input$RatiosFrom)+5

#IMPOSED TFR OPTION
ImposedTFR<-input$ImposedTFR
ffab<-.4886
UseImposedTFR<-input$ImposeTFR

##ADJUST BY MIGRATION OPTION
NetMigrationAdjustLevel<-input$NetMigrationAdjustLevel/100

#SELECT BY SEX
SelectBySex<-input$Sex

Name<-paste(input$County)

TMinusOneAgeInit_F<-subset(K,CTYNAME==input$County & YEAR==3 & AGEGRP>0)
TMinusOneAgeInit_F<-TMinusOneAgeInit_F$TOT_FEMALE
TMinusOneAge_F<-TMinusOneAgeInit_F

TMinusOneAgeInit_M<-subset(K,CTYNAME==input$County & YEAR==3 & AGEGRP>0)
TMinusOneAgeInit_M<-TMinusOneAgeInit_M$TOT_MALE
TMinusOneAge_M<-TMinusOneAgeInit_M

TMinusOneAge<-TMinusOneAgeInit<-c(TMinusOneAge_F,TMinusOneAge_M)

TMinusOneAgeInitRatios_F<-subset(K,CTYNAME==input$County & YEAR==FirstYear & AGEGRP>0)
TMinusOneAgeInitRatios_F<-TMinusOneAgeInitRatios_F$TOT_FEMALE
TMinusOneAgeRatios_F<-TMinusOneAgeInitRatios_F

TMinusOneAgeInitRatios_M<-subset(K,CTYNAME==input$County & YEAR==FirstYear & AGEGRP>0)
TMinusOneAgeInitRatios_M<-TMinusOneAgeInitRatios_M$TOT_MALE
TMinusOneAgeRatios_M<-TMinusOneAgeInitRatios_M

TMinusOneAgeRatios<-TMinusOneAgeInitRatios<-c(TMinusOneAgeRatios_F,TMinusOneAgeRatios_M)

TMinusZeroAgeInit_F<-subset(K,CTYNAME==input$County & YEAR==8 & AGEGRP>0)
TMinusZeroAgeInit_F<-TMinusZeroAgeInit_F$TOT_FEMALE
TMinusZeroAge_F<-TMinusZeroAgeInit_F

TMinusZeroAgeInit_M<-subset(K,CTYNAME==input$County & YEAR==8 & AGEGRP>0)
TMinusZeroAgeInit_M<-TMinusZeroAgeInit_M$TOT_MALE
TMinusZeroAge_M<-TMinusZeroAgeInit_M

TMinusZeroAge<-TMinusZeroAgeInit<-c(TMinusZeroAge_F,TMinusZeroAge_M)

TMinusZeroAgeInitRatios_F<-subset(K,CTYNAME==input$County & YEAR==SecondYear & AGEGRP>0)
TMinusZeroAgeInitRatios_F<-TMinusZeroAgeInitRatios_F$TOT_FEMALE
TMinusZeroAgeRatios_F<-TMinusZeroAgeInitRatios_F

TMinusZeroAgeInitRatios_M<-subset(K,CTYNAME==input$County & YEAR==SecondYear & AGEGRP>0)
TMinusZeroAgeInitRatios_M<-TMinusZeroAgeInitRatios_M$TOT_MALE
TMinusZeroAgeRatios_M<-TMinusZeroAgeInitRatios_M

TMinusZeroAgeRatios<-TMinusZeroAgeInitRatios<-c(TMinusZeroAgeRatios_F,TMinusZeroAgeRatios_M)

##"BA" IS THE BRASS RELATIONAL LOGIT MODEL ALPHA
if(input$ImputeMort=="YES") {
BA_start<-input$BAStart
BA_end<-input$BAEnd
BB<-1
}

if(input$ImputeMort=="NO") {
BA_start<-0
BA_end<-0
BB<-1
}

#####
##CALCULATIONS
Ratios<-array(0,dim=length(TMinusOneAgeRatios))
for (i in 2:length(TMinusOneAgeRatios)) {Ratios[i]<-TMinusZeroAgeRatios[i]/TMinusOneAgeRatios[i-1]}
Ratios[1]<-(TMinusZeroAgeRatios[1]+TMinusZeroAgeRatios[HALFSIZE+1])/sum(TMinusOneAgeRatios[4:10])

S_F<-array(0,c(HALFSIZE,HALFSIZE))
S_F<-rbind(0,cbind(diag(Ratios[2:(HALFSIZE)]),0))
B_F<-0*S_F
B_F[1,4:10]<-Ratios[1]*ffab
A_F<-B_F+S_F

S_M<-array(0,c(HALFSIZE,HALFSIZE))
S_M<-rbind(0,cbind(diag(Ratios[20:SIZE]),0))
B_M<-0*S_M
B_M[1,4:10]<-Ratios[1]*(1-ffab)

AEnd_Zero<-A_Zero<-array(0,c(HALFSIZE,HALFSIZE))

Acolone<-cbind(A_F,A_Zero)
Acoltwo<-cbind(B_M,S_M)
A<-rbind(Acolone,Acoltwo)

SumFirstRows<-(sum(B_F)+sum(B_M)) #(May work with)

ImpliedTFR2010<-((TMinusOneAgeInit[1]+TMinusOneAgeInit[HALFSIZE+1])/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH
ImpliedTFR2015<-((TMinusZeroAgeInit[1]+TMinusZeroAgeInit[HALFSIZE+1])/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH

if(STEPS<198){
##PROJECTION FUNCTION
CCRProject<-function(TMinusZeroAge,BA_start,BA_end,CURRENTSTEP)
	{
	##CALCULATE THE Yx FOR THE lx'S
	YxF<-YxM<-NULL
	for (i in 1:length(lxF)){YxF[i]<-.5*log(lxF[i]/(1-lxF[i]))}
	for (i in 1:length(lxM)){YxM[i]<-.5*log(lxM[i]/(1-lxM[i]))}
	
	lxFStart<-lxFEnd<-array(0,length(lxF))
	lxMStart<-lxMEnd<-array(0,length(lxM))
	for (i in 1:length(lxFStart)){lxFStart[i]<-1/(1+exp(-2*BA_start-2*BB*YxF[i]))}
	for (i in 1:length(lxMStart)){lxMStart[i]<-1/(1+exp(-2*BA_start-2*BB*YxM[i]))}
	
	LxFStart<-LxFEnd<-array(0,length(lxF))
	LxMStart<-LxMEnd<-array(0,length(lxM))
	##**THIS IS A LITTLE OFF FOR THE FIRST AGE GROUP**
	for (i in 1:length(LxFStart)){LxFStart[i]<-.5*(lxFStart[i]+lxFStart[i+1])}
	for (i in 1:length(LxMStart)){LxMStart[i]<-.5*(lxMStart[i]+lxMStart[i+1])}
	
	SxFStart<-SxFEnd<-array(0,length(lxF)-1)
	SxMStart<-SxMEnd<-array(0,length(lxM)-1)
	for (i in 1:length(SxFStart)-1){SxFStart[i]<-(LxFStart[i+1]/LxFStart[i])}
	for (i in 1:length(SxMStart)-1){SxMStart[i]<-(LxMStart[i+1]/LxMStart[i])}	

	e0FStart<-sum(LxFStart[1:22]*5)
	e0MStart<-sum(LxMStart[1:22]*5)

	lxFAdj<-array(0,length(lxF))
	lxMAdj<-array(0,length(lxM))

	if(CURRENTSTEP<=STEPS){
	for (i in 1:length(lxFAdj)){lxFAdj[i]<-1/(1+exp(-2*(BA_start*(1-CURRENTSTEP/STEPS)+BA_end*(CURRENTSTEP/STEPS))-2*BB*YxF[i]))}
	for (i in 1:length(lxMAdj)){lxMAdj[i]<-1/(1+exp(-2*(BA_start*(1-CURRENTSTEP/STEPS)+BA_end*(CURRENTSTEP/STEPS))-2*BB*YxM[i]))}
	}

	if(CURRENTSTEP>=STEPS){
	for (i in 1:length(lxFAdj)){lxFAdj[i]<-1/(1+exp(-2*BA_end-2*BB*YxF[i]))}
	for (i in 1:length(lxMAdj)){lxMAdj[i]<-1/(1+exp(-2*BA_end-2*BB*YxM[i]))}
	}

	LxFAdj<-array(0,length(lxF))
	LxMAdj<-array(0,length(lxM))
	##**THIS IS A LITTLE OFF FOR THE FIRST AGE GROUP**
	for (i in 1:length(LxFAdj)){LxFAdj[i]<-.5*(lxFAdj[i]+lxFAdj[i+1])}
	for (i in 1:length(LxMAdj)){LxMAdj[i]<-.5*(lxMAdj[i]+lxMAdj[i+1])}

	SxFAdj<-array(0,length(lxF)-1)
	SxMAdj<-array(0,length(lxM)-1)
	for (i in 1:length(SxFAdj)-1){SxFAdj[i]<-(LxFAdj[i+1]/LxFAdj[i])}
	for (i in 1:length(SxMAdj)-1){SxMAdj[i]<-(LxMAdj[i+1]/LxMAdj[i])}

	e0FAdj<-sum(LxFAdj[1:22]*5)
	e0MAdj<-sum(LxMAdj[1:22]*5)

	SAdj_F<-array(0,c(HALFSIZE,HALFSIZE))
	SAdj_F<-rbind(0,cbind(diag(SxFAdj[2:(HALFSIZE)]-SxFStart[2:(HALFSIZE)]),0))
	SAdj_F<-SAdj_F+S_F
	AAdj_F<-B_F+SAdj_F

	SAdj_M<-array(0,c(HALFSIZE,HALFSIZE))
	SAdj_M<-rbind(0,cbind(diag(SxMAdj[2:(HALFSIZE)]-SxMStart[2:(HALFSIZE)]),0))
	SAdj_M<-SAdj_M+S_M

	AAdj_Zero<-A_Zero<-array(0,c(HALFSIZE,HALFSIZE))

	Acolone<-cbind(A_F,A_Zero)
	Acoltwo<-cbind(B_M,S_M)
	A<-rbind(Acolone,Acoltwo)

	AAdjcolone<-cbind(AAdj_F,AAdj_Zero)
	AAdjcoltwo<-cbind(B_M,SAdj_M)
	AAdj<-rbind(AAdjcolone,AAdjcoltwo)

	TMinusOneAgeNew<-data.frame(TMinusZeroAge) 
		if(CURRENTSTEP>0){
				TMinusZeroAge<-AAdj%*%TMinusZeroAge
		if(NetMigrationAdjustLevel!=0)
				{TMinusZeroAge<-NetMigrationAdjustLevel*5*sum(TMinusOneAgeNew)*Migration+TMinusZeroAge}
		if(UseImposedTFR=="YES") 
				{TMinusZeroAge[1]<-ImposedTFR*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*ffab
				TMinusZeroAge[HALFSIZE+1]<-ImposedTFR*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*(1-ffab)}
				}
	TMinusZeroAge<-data.frame(TMinusZeroAge)
	return(c(TMinusZeroAge,TMinusOneAge=TMinusOneAgeNew,e0FStart=e0FStart,e0MStart=e0MStart,e0FAdj=e0FAdj,e0MAdj=e0MAdj,CURRENTSTEP=CURRENTSTEP+1))
	}
}

CCRNew<-CCRProject(TMinusZeroAge,BA_start,BA_end,CURRENTSTEP)
while(CCRNew$CURRENTSTEP<STEPS+1) {CCRNew<-CCRProject(CCRNew$TMinusZeroAge,BA_start,BA_end,CCRNew$CURRENTSTEP)}
ImpliedTFRNew<-((CCRNew$TMinusZeroAge[1]+CCRNew$TMinusZeroAge[HALFSIZE+1])/5)/sum(CCRNew$TMinusZeroAge[4:10])*FERTWIDTH

CCRatios<-array(0,length(TMinusOneAge)+1)
for (i in 2:length(CCRatios)) {CCRatios[i]<-CCRNew$TMinusZeroAge[i]/CCRNew$TMinusOneAge[i-1]}
CCRatiosF<-CCRatios[2:18]
CCRatiosM<-CCRatios[20:36]

TMinusZeroAge<-TMinusZeroAgeInit
CCRStable<-CCRProject(TMinusZeroAge,BA_start,BA_end,0)
while(CCRStable$CURRENTSTEP<STEPSSTABLE+1) {CCRStable<-CCRProject(CCRStable$TMinusZeroAge,BA_start,BA_end,CCRStable$CURRENTSTEP)}
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
##FIRST GRAPH
agegroups<-c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
if(SelectBySex=="Total") {plot(TMinusOneAgeInit[,1]/sum(TMinusOneAgeInit[,1]),type="l",col="orange",main=paste(text=c(input$County,", ",input$Sex),collapse=""),ylim=c(0,.12),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
if(SelectBySex=="Female") {plot(TMinusOneAgeInit[,2]/sum(TMinusOneAgeInit[,2]),type="l",col="orange",main=paste(text=c(input$County,", ",input$Sex),collapse=""),ylim=c(0,.12),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
if(SelectBySex=="Male") {plot(TMinusOneAgeInit[,3]/sum(TMinusOneAgeInit[,3]),type="l",col="orange",main=paste(text=c(input$County,", ",input$Sex),collapse=""),ylim=c(0,.12),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}

if(SelectBySex=="Total") {lines(TMinusZeroAgeInit[,1]/sum(TMinusZeroAgeInit[,1]),col="blue",lwd=4)}
if(SelectBySex=="Female") {lines(TMinusZeroAgeInit[,2]/sum(TMinusZeroAgeInit[,2]),col="blue",lwd=4)}
if(SelectBySex=="Male") {lines(TMinusZeroAgeInit[,3]/sum(TMinusZeroAgeInit[,3]),col="blue",lwd=4)}

if(SelectBySex=="Total") {lines(NewAge[,1]/sum(NewAge[,1]),col="dark green",lty=1,lwd=4)}
if(SelectBySex=="Female") {lines(NewAge[,2]/sum(NewAge[,2]),col="dark green",lty=1,lwd=4)}
if(SelectBySex=="Male") {lines(NewAge[,3]/sum(NewAge[,3]),col="dark green",lty=1,lwd=4)}

if (min(StableAge)>=0) {
mtext(side=1,"Age groups",line=4,cex=.75)
axis(side=1,at=1:HALFSIZE,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2)
legend(11.5, .12, legend=c("2010 (estimate)","2015 (estimate)",paste(c(PROJECTIONYEAR),"(projection)"),"Stable"),
       col=c("orange","blue","dark green","black"), lty=c(1,1,1,3),lwd=c(4,4,4,1.5),cex=1.2)
}

if (min(StableAge)<0) {
mtext(side=1,"Age groups",line=4,cex=.75)
axis(side=1,at=1:HALFSIZE,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2)
legend(11.5, .12, legend=c("2010 (estimate)","2015 (estimate)",paste(c(PROJECTIONYEAR),"(projection)")),
       col=c("orange","blue","dark green"), lty=c(1,1,1),lwd=c(4,4,4),cex=1.2)
}

mtext(side=1,c("Sum 2010:"),line=-15,adj=.125,col="orange")
if(SelectBySex=="Total") {mtext(side=1,c(sum(TMinusOneAgeInit[,1])),line=-15,adj=.3,col="orange")}
if(SelectBySex=="Female") {mtext(side=1,c(sum(TMinusOneAgeInit[,2])),line=-15,adj=.3,col="orange")}
if(SelectBySex=="Male") {mtext(side=1,c(sum(TMinusOneAgeInit[,3])),line=-15,adj=.3,col="orange")}

mtext(side=1,c("Sum 2015:"),line=-14,adj=.125,col="blue")
if(SelectBySex=="Total") {mtext(side=1,c(sum(TMinusZeroAgeInit[,1])),line=-14,adj=.3,col="blue")}
if(SelectBySex=="Female") {mtext(side=1,c(sum(TMinusZeroAgeInit[,2])),line=-14,adj=.3,col="blue")}
if(SelectBySex=="Male") {mtext(side=1,c(sum(TMinusZeroAgeInit[,3])),line=-14,adj=.3,col="blue")}

mtext(side=1,c("Sum "),line=-13,adj=.117,col="dark green")
mtext(side=1,c(PROJECTIONYEAR),line=-13,adj=.18,col="dark green")
mtext(side=1,c(":"),line=-13,adj=.225,col="dark green")
if(SelectBySex=="Total") {mtext(side=1,c(round(sum(NewAge[,1]))),line=-13,adj=.3,col="dark green")}
if(SelectBySex=="Female") {mtext(side=1,c(round(sum(NewAge[,2]))),line=-13,adj=.3,col="dark green")}
if(SelectBySex=="Male") {mtext(side=1,c(round(sum(NewAge[,3]))),line=-13,adj=.3,col="dark green")}

mtext(side=1,c("iTFR 2010:"),line=-7,adj=.13,col="orange")
mtext(side=1,c(round(ImpliedTFR2010,2)),line=-7,adj=.29,col="orange")

mtext(side=1,c("iTFR 2015:"),line=-6,adj=.13,col="blue")
mtext(side=1,c(round(ImpliedTFR2015,2)),line=-6,adj=.29,col="blue")

mtext(side=1,c("iTFR"),line=-5,adj=.12,col="dark green")
mtext(side=1,c(PROJECTIONYEAR),line=-5,adj=.185,col="dark green")
mtext(side=1,c(":"),line=-5,adj=.23,col="dark green")
mtext(side=1,c(round(ImpliedTFRNew,2)),line=-5,adj=.29,col="dark green")

if(SelectBySex=="Total") {LASTGROWTHRATE<-paste(text=c("R (2010 to 2015):  ", round(log(sum(TMinusZeroAgeInit[,1])/sum(TMinusOneAgeInit[,1]))/5*100,2)),collapse="")}
if(SelectBySex=="Male") {LASTGROWTHRATE<-paste(text=c("R (2010 to 2015):  ", round(log(sum(TMinusZeroAgeInit[,1])/sum(TMinusOneAgeInit[,1]))/5*100,2)),collapse="")}
if(SelectBySex=="Female") {LASTGROWTHRATE<-paste(text=c("R (2010 to 2015):  ", round(log(sum(TMinusZeroAgeInit[,1])/sum(TMinusOneAgeInit[,1]))/5*100,2)),collapse="")}
mtext(side=1,c(LASTGROWTHRATE),line=-11,adj=.15,col="blue")

if(SelectBySex=="Total") {GROWTHRATE<-paste(text=c("R (2015 to ",PROJECTIONYEAR,"):  ", round(log(sum(NewAge[,1])/sum(TMinusZeroAgeInit[,1]))/(STEPS*5)*100,2)),collapse="")}
if(SelectBySex=="Female") {GROWTHRATE<-paste(text=c("R (2015 to ",PROJECTIONYEAR,"):  ", round(log(sum(NewAge[,2])/sum(TMinusZeroAgeInit[,2]))/(STEPS*5)*100,2)),collapse="")}
if(SelectBySex=="Male") {GROWTHRATE<-paste(text=c("R (2015 to ",PROJECTIONYEAR,"):  ", round(log(sum(NewAge[,3])/sum(TMinusZeroAgeInit[,3]))/(STEPS*5)*100,2)),collapse="")}
mtext(side=1,c(GROWTHRATE),line=-10,adj=.15,col="dark green")

if (min(StableAge)>=0) {
if(SelectBySex=="Total") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ", round(log(sum(StableAge[,1])/sum(TMinusZeroAgeInit[,1]))/(STEPSSTABLE*5)*100,2)),collapse="")}
if(SelectBySex=="Female") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ", round(log(sum(StableAge[,2])/sum(TMinusZeroAgeInit[,2]))/(STEPSSTABLE*5)*100,2)),collapse="")}
if(SelectBySex=="Male") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ", round(log(sum(StableAge[,3])/sum(TMinusZeroAgeInit[,3]))/(STEPSSTABLE*5)*100,2)),collapse="")}
mtext(side=1,c(STABLEGROWTHRATE),line=-9,adj=.15,col="black")

if(SelectBySex=="Total") {lines(StableAge[,1]/sum(StableAge[,1]),col="black",lty=3,lwd=1.5)}
if(SelectBySex=="Female") {lines(StableAge[,2]/sum(StableAge[,2]),col="black",lty=3,lwd=1.5)}
if(SelectBySex=="Male") {lines(StableAge[,3]/sum(StableAge[,3]),col="black",lty=3,lwd=1.5)}
}

if (min(StableAge)<0) {
if(SelectBySex=="Total") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ..."),collapse="")}
if(SelectBySex=="Female") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ..."),collapse="")}
if(SelectBySex=="Male") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ..."),collapse="")}
mtext(side=1,c(STABLEGROWTHRATE),line=-9,adj=.15,col="black")
}

if (input$ImputeMort=="YES" & SelectBySex=="Total") {
mtext(side=1,c("Imputed starting e0, female: "),line=-3,adj=.157,col="black")
mtext(side=1,c(round(CCRNew$e0FStart,1)),line=-3,adj=.455,col="black")
mtext(side=1,c("Imputed starting e0, male: "),line=-2,adj=.155,col="black")
mtext(side=1,c(round(CCRNew$e0MStart,1)),line=-2,adj=.4565,col="black")
}

if (input$ImputeMort=="YES" & SelectBySex=="Female") {
mtext(side=1,c("Imputed starting e0, female: "),line=-3,adj=.157,col="black")
mtext(side=1,c(round(CCRNew$e0FStart,1)),line=-3,adj=.455,col="black")
}

if (input$ImputeMort=="YES" & SelectBySex=="Male") {
mtext(side=1,c("Imputed starting e0, male: "),line=-3,adj=.155,col="black")
mtext(side=1,c(round(CCRNew$e0MStart,1)),line=-3,adj=.4565,col="black")
}

##SECOND GRAPH
agegroups2<-c("5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

plot(Ratios[2:18],type="l",col="dodger blue",main=paste(text=c("Effective Cohort Change Ratios, ",PROJECTIONYEAR-5," to ",PROJECTIONYEAR),collapse=""),ylim=c(.5,1.75),axes=FALSE,xlab="",ylab="Ratio",lwd=4)
lines(Ratios[20:36],type="l",col="gold",lwd=4)
lines(CCRatiosF,type="l",col="dodger blue",lty=2,lwd=2)
lines(CCRatiosM,type="l",col="gold",lty=2,lwd=2)
mtext(side=1,"Age groups",line=4,cex=.75)
axis(side=1,at=1:(HALFSIZE-1),labels=agegroups2,las=2,cex.axis=0.9)
axis(side=2)
legend(7,1.75, legend=c("Female","Male", "Female, with migration and mortality adjustments","Male, with migration and mortality adjustments"),
       col=c("dodger blue","gold","dodger blue","gold"), lty=c(1,1,2,2),lwd=c(4,4,2,2),cex=1.2)

if (input$ImputeMort=="YES") {
mtext(side=1,c("Imputed e0, female:"),line=-10,adj=.125,col="black")
mtext(side=1,c(round(CCRNew$e0FAdj,1)),line=-10,adj=.35,col="black")
mtext(side=1,c("Imputed e0, male:"),line=-9,adj=.122,col="black")
mtext(side=1,c(round(CCRNew$e0MAdj,1)),line=-9,adj=.35,col="black")
}

##THIRD GRAPH
barplot(NewAge_F,horiz=T,names=agegroups,space=0,xlim=c(max(NewAge_M)*2,0),col="dodger blue",las=1,main=paste(text=c("Female, ",PROJECTIONYEAR),collapse=""))

##FOURTH GRAPH
barplot(NewAge_M,horiz=T,names=FALSE,space=0,xlim=c(0,max(NewAge_M)*2),col="gold",main=paste(text=c("Male, ",PROJECTIONYEAR),collapse=""))
##########

},height=1200,width=1200)
		
}

shinyApp(ui = ui, server = server)

