##########
##HAMILTON-PERRY WITH STOCHASTIC COMPONENTS POPULATION PROJECTION CODE
##
##EDDIE HUNSINGER, NOVEMBER 2020 (UPDATED OCTOBER 2021)
##https://edyhsgr.github.io/eddieh/
##
##IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, PLEASE CITE THE SOURCE
##This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 International License (more information: https://creativecommons.org/licenses/by-sa/3.0/igo/).
##
##EXAMPLE DATA IS LINKED, SO YOU SHOULD BE ABLE TO SIMPLY COPY ALL AND PASTE INTO R TO SEE IT WORK
##
##THERE IS NO WARRANTY FOR THIS CODE
##THIS CODE HAS NOT BEEN PEER-REVIEWED OR CAREFULLY TESTED - QUESTIONS AND COMMENTS ARE WELCOME, OF COURSE (edyhsgr@gmail.com)
##########

#install.packages("shiny")
#install.packages("gplots")
library(shiny)
library(gplots)

ui<-fluidPage(
  
  tags$h3("Draft Cohort Change Ratio-Based Stochastic Population Projection Review Shiny App - Based on Cohort Change Ratio-Based Stable Population Review Shiny App"),
  p("Related information and ",
    tags$a(href="https://www.r-project.org/", "R"),
    "code available at: ",
    tags$a(href="https://github.com/edyhsgr/CCRStable", 
           "CCRStable GitHub Repository")
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      
	selectizeInput(inputId = "County", label = "County", 
	choices = c("Alameda"="Alameda County",
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
#options = list(placeholder = "Type in a county to see graphs", multiple = TRUE, maxOptions = 5000, onInitialize = I('function() { this.setValue(""); }'))
	          ),
            
      numericInput("STEP","Project to (year)",2030,2020,2100,step=5),
      
      selectInput("RatiosFrom", "Using ratios from",
                  c(
                    "2014 to 2019"="7",
                    "2013 to 2018"="6",
                    "2012 to 2017"="5",
                    "2011 to 2016"="4",
                    "2010 to 2015"="3"
                  ),
      ),

      numericInput("ITER","Number of projection iterations (sample size)",100,100,1000,step=100),
      
      hr(),
      
      selectInput("ImposeTFR", "Impose iTFR?",
                  c(
                    "Yes"="YES",
                    "No"="NO"
                  ),
      ),
      
      sliderInput("ImposedTFR_ar","If Yes, iTFR AR(1) term (range inputs give uniform range option, for uncertain autocorrelation, etc.)",min=0,max=1,value=c(.5,1),step=0.05),
      sliderInput("ImposedTFR","...and iTFR level term",min=0,max=10,value=c(1.8,1.8),step=0.1),
      sliderInput("ImposedTFR_se","...and iTFR standard error term",min=0,max=.5,value=c(.25,.25),step=0.05),
      
      hr(),
      
      selectInput("AdjustMigr", "Adjust net migration? (Annual, percent of total population)",
                  c(
                    "Yes"="YES",
                    "No"="NO"
                  ),
      ),

      sliderInput("NetMigrationAdjustLevel_ar","If yes, net migration adjustment AR(1) term (range inputs give uniform range option, for uncertain autocorrelation, etc.)",min=-1,max=1,value=c(-.5,1),step=0.05),
      sliderInput("NetMigrationAdjustLevel","...and net migration adjustment level term",min=-2,max=2,value=c(0,0),step=0.1),
      sliderInput("NetMigrationAdjustLevel_se","...and net migration adjustment standard error term",min=0,max=.5,value=c(.25,.25),step=0.05),
      
      hr(),
      
      selectInput("ImputeMort", "Impute mortality?",
                  c(
                    "Yes"="YES",
                    "No"="NO"
                  ),
      ),
      
      sliderInput("BAStart","If yes, Brass' model alpha for First projection step (range inputs give uniform range option, for uncertain drift, etc.)",min=-.5,max=.5,value=c(.03,.03),step=0.01),
      sliderInput("BAEnd","...and Brass' model alpha drift term (increase per step)",min=-.5,max=.5,value=c(.03,.03),step=0.01),
      sliderInput("BA_se","...and Brass' model alpha standard error term",min=0,max=.25,value=c(.02,.02),step=0.01),
      
      hr(),
      
      p("This interface was made with ",
        tags$a(href="https://shiny.rstudio.com/", 
               "Shiny for R."),
        
        tags$a(href="https://edyhsgr.github.io/eddieh/", 
               "Eddie Hunsinger,"), 
        
        "November 2020 (updated October 2021)."),
      
      p("Population estimates inputs from ",
        tags$a(href="https://www.census.gov/programs-surveys/popest.html", 
               "US Census Bureau Vintage 2019 Population Estimates.")),

      p("Related Cohort Change Ratio-Based Stable Population Projection interface with more information on the Cohort Change Ratio Projection: ",
        tags$a(href="https://shiny.demog.berkeley.edu/eddieh/CCRStable/", 
               "Hunsinger (2019).")),
      
      p("Supporting work and thinking on stochastic population projection: ",
        tags$a(href="https://applieddemogtoolbox.github.io/#StochasticForecast", 
               "Hunsinger (2011)."),
        
        p("More information on iTFR: ",
          tags$a(href="https://osf.io/adu98/", 
                 "Hauer and Schmertmann (2019)"),
          " and ",
          tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0067226", 
                 "Hauer, Baker, and Brown (2013).")),
        
        p("Slides with background thoughts on adjusting net migration: ",
          tags$a(href="https://edyhsgr.github.io/documents/ProjPresentation.pdf", 
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
      
        tags$a(href="https://applieddemogtoolbox.github.io/#CCRStable", 
             "Applied Demography Toolbox listing for the Cohort Change Ratio-Based Stable Population work."),
      
      width=3
    ),
    
    mainPanel(
      
      plotOutput("plots")
    ))
  )

##########
##READING EXTERNAL DATA IN
##########

##DATA (CENSUS BUREAU VINTAGE 2019 POPULATION ESTIMATES BY DEMOGRAPHIC CHARACTERISTICS)
##https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-alldata-06.csv 
##https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/
K<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/PopEstimates/cc-est2019-alldata-06_Extract.csv",header=TRUE,sep=","))

##CENSUS ACS (via IPUMS) CA MIGRATION DATA (GENERIC)
Migration<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Migration/AGenericMigrationProfile_CA_2013to2017ACS.csv",header=TRUE,sep=","))
Migration<-c(Migration$CA_F,Migration$CA_M)

##USMD CA SURVIVAL DATA (GENERIC)
lt<-read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Mortality/lt_CA_USMD2010to2014.csv",header=TRUE,sep=",")
lxF<-lt$lx_Female/100000
lxM<-lt$lx_Male/100000
lxT<-lt$lx_Both/100000
lxF<-c(lxF[1],lxF[3:24])
lxM<-c(lxM[1],lxM[3:24])
lxT<-c(lxT[1],lxT[3:24])

server<-function(input, output) {	
  output$plots<-renderPlot({
    par(mfrow=c(3,2))
    
    ##RUN ONLY IF COUNTY INPUTS ARE PROVIDED
    #if(input$County=="") {
    #  plot.new()
    #  legend("topleft",legend=c("Select a county with the panel to the left"),cex=2,bty="n")
    #}
    
    ##NUMBER FORMATTING
    options(scipen=999)

##########
##SCRIPT INPUTS
##########

##DIMENSIONS
ITER<-input$ITER
SIZE<-36
HALFSIZE<-SIZE/2
STEPS<-(input$STEP-2015)/5
CURRENTSTEP<-(2020-2015)/5
PROJECTIONYEAR<-STEPS*5+2015
FERTWIDTH<-35

##SELECTING RATIOS BASIS
##(Year "6" is 2013, 7 is 2014, 8 is 2015...)
FirstYear<-strtoi(input$RatiosFrom)
SecondYear<-strtoi(input$RatiosFrom)+5

##IMPOSED TFR OPTION (WITH AUTOCORRELATION OPTION)
ImposedTFR<-array(runif(ITER,input$ImposedTFR[1],input$ImposedTFR[2]))
ImposedTFR_ar<-array(runif(ITER,input$ImposedTFR_ar[1],input$ImposedTFR_ar[2]))
ImposedTFR_se<-array(runif(ITER,input$ImposedTFR_se[1],input$ImposedTFR_se[2]))
ffab<-.4886
UseImposedTFR<-input$ImposeTFR

##ADJUST BY MIGRATION OPTION
NetMigrationAdjustLevel<-array(runif(ITER,input$NetMigrationAdjustLevel[1]/100,input$NetMigrationAdjustLevel[2]/100))
NetMigration_ar<-array(runif(ITER,input$NetMigrationAdjustLevel_ar[1],input$NetMigrationAdjustLevel_ar[2]))
NetMigration_se<-array(runif(ITER,input$NetMigrationAdjustLevel_se[1]/100,input$NetMigrationAdjustLevel_se[2]/100))

##IMPUTE MORTALITY OPTION
##"BA" IS THE BRASS RELATIONAL LOGIT MODEL ALPHA
BA_start<-array(runif(ITER,input$BAStart[1],input$BAStart[2]))
BA_start_init<-BA_start
BA_end<-array(runif(ITER,input$BAEnd[1],input$BAEnd[2]))
BA_se<-array(runif(ITER,input$BA_se[1],input$BA_se[2]))
BB<-1
ImputeMort<-input$ImputeMort

if(ImputeMort=="NO") {for (i in 1:ITER) {BA_start[i]<-BA_end[i]<-1}}
if(ImputeMort=="NO") {for (i in 1:ITER) {BA_se[i]<-0}}

##SELECT BY SEX
SelectBySex<-"Total"

##SELECT AREA
Name<-paste(input$County)

##NUMBER FORMATTING
options(scipen=999)

if(input$County!="") {

##SELECTING FROM THE INPUT POPULATION TABLE (K) BASED ON INPUTS (DATA FOR JUMP-OFF, AND DATA FOR RATIOS)
TMinusOneAgeInit_F<-subset(K,CTYNAME==Name & YEAR==3 & AGEGRP>0)
TMinusOneAgeInit_F<-TMinusOneAgeInit_F$TOT_FEMALE
TMinusOneAge_F<-TMinusOneAgeInit_F

TMinusOneAgeInit_M<-subset(K,CTYNAME==Name & YEAR==3 & AGEGRP>0)
TMinusOneAgeInit_M<-TMinusOneAgeInit_M$TOT_MALE
TMinusOneAge_M<-TMinusOneAgeInit_M

TMinusOneAge<-TMinusOneAgeInit<-array(c(TMinusOneAge_F,TMinusOneAge_M),c(SIZE,1,ITER))

TMinusOneAgeInitRatios_F<-subset(K,CTYNAME==Name & YEAR==FirstYear & AGEGRP>0)
TMinusOneAgeInitRatios_F<-TMinusOneAgeInitRatios_F$TOT_FEMALE
TMinusOneAgeRatios_F<-TMinusOneAgeInitRatios_F

TMinusOneAgeInitRatios_M<-subset(K,CTYNAME==Name & YEAR==FirstYear & AGEGRP>0)
TMinusOneAgeInitRatios_M<-TMinusOneAgeInitRatios_M$TOT_MALE
TMinusOneAgeRatios_M<-TMinusOneAgeInitRatios_M

TMinusOneAgeRatios<-TMinusOneAgeInitRatios<-array(c(TMinusOneAgeRatios_F,TMinusOneAgeRatios_M),c(SIZE,1,ITER))

TMinusZeroAgeInit_F<-subset(K,CTYNAME==Name & YEAR==8 & AGEGRP>0)
TMinusZeroAgeInit_F<-TMinusZeroAgeInit_F$TOT_FEMALE
TMinusZeroAge_F<-TMinusZeroAgeInit_F

TMinusZeroAgeInit_M<-subset(K,CTYNAME==Name & YEAR==8 & AGEGRP>0)
TMinusZeroAgeInit_M<-TMinusZeroAgeInit_M$TOT_MALE
TMinusZeroAge_M<-TMinusZeroAgeInit_M

TMinusZeroAge<-TMinusZeroAgeInit<-array(c(TMinusZeroAge_F,TMinusZeroAge_M),c(SIZE,1,ITER))

TMinusZeroAgeInitRatios_F<-subset(K,CTYNAME==Name & YEAR==SecondYear & AGEGRP>0)
TMinusZeroAgeInitRatios_F<-TMinusZeroAgeInitRatios_F$TOT_FEMALE
TMinusZeroAgeRatios_F<-TMinusZeroAgeInitRatios_F

TMinusZeroAgeInitRatios_M<-subset(K,CTYNAME==Name & YEAR==SecondYear & AGEGRP>0)
TMinusZeroAgeInitRatios_M<-TMinusZeroAgeInitRatios_M$TOT_MALE
TMinusZeroAgeRatios_M<-TMinusZeroAgeInitRatios_M

TMinusZeroAgeRatios<-TMinusZeroAgeInitRatios<-array(c(TMinusZeroAgeRatios_F,TMinusZeroAgeRatios_M),c(SIZE,1,ITER))

##########
##CALCULATIONS
##########

##COHORT CHANGE RATIOS
Ratios<-array(0,c(SIZE,ITER))
for (i in 2:SIZE) {Ratios[i,]<-TMinusZeroAgeRatios[i,,]/TMinusOneAgeRatios[i-1,,]}
for (i in 1:ITER) {Ratios[1,i]<-(TMinusZeroAgeRatios[1,,i]+TMinusZeroAgeRatios[HALFSIZE+1,,i])/sum(TMinusOneAgeRatios[4:10,,i])}

##PLACING COHORT CHANGE RATIOS (FEMALE)
S_F<-array(0,c(HALFSIZE,HALFSIZE,ITER))
for (i in 1:ITER) {S_F[,,i]<-rbind(0,cbind(diag(Ratios[2:(HALFSIZE),i]),0))}

##OPEN-ENDED AGE GROUP OPTION (FEMALE)
for (i in 1:ITER) {S_F[HALFSIZE,HALFSIZE-1,i]<-TMinusZeroAgeRatios[HALFSIZE,,i]/(TMinusOneAgeRatios[HALFSIZE-1,,i]+TMinusOneAgeRatios[HALFSIZE,,i])}
for (i in 1:ITER) {Ratios[HALFSIZE,i]<-S_F[HALFSIZE,HALFSIZE,i]<-S_F[HALFSIZE,HALFSIZE-1,i]}

##BIRTHS AND MATRIX PORTION CONSTRUCTION (FEMALE)
B_F<-0*S_F
for (i in 1:ITER) {B_F[1,4:10,i]<-Ratios[1,i]*ffab}
A_F<-B_F+S_F

##PLACING COHORT CHANGE RATIOS (MALE)
S_M<-array(0,c(HALFSIZE,HALFSIZE,ITER))
for (i in 1:ITER) {S_M[,,i]<-rbind(0,cbind(diag(Ratios[20:SIZE,i]),0))}

##OPEN-ENDED AGE GROUP OPTION (MALE)
for (i in 1:ITER) {S_M[HALFSIZE,HALFSIZE-1,i]<-TMinusZeroAgeRatios[SIZE,,i]/(TMinusOneAgeRatios[SIZE-1,,i]+TMinusOneAgeRatios[SIZE,,i])}
for (i in 1:ITER) {Ratios[SIZE,i]<-S_M[HALFSIZE,HALFSIZE,i]<-S_M[HALFSIZE,HALFSIZE-1,i]}

##BIRTHS AND MATRIX PORTION CONSTRUCTION (MALE)
B_M<-0*S_M
for (i in 1:ITER) {B_M[1,4:10,i]<-Ratios[1,i]*(1-ffab)}

##STRUCTURAL ZEROES
AEnd_Zero<-A_Zero<-array(0,c(HALFSIZE,HALFSIZE,ITER))

##MAKING FULL PROJECTION MATRIX (TWO-SEX)
Acoltwo<-Acolone<-array(0,c(HALFSIZE,SIZE,ITER))
for (i in 1:ITER) {Acolone[,,i]<-cbind(A_F[,,i],A_Zero[,,i])}
for (i in 1:ITER) {Acoltwo[,,i]<-cbind(B_M[,,i],S_M[,,i])}
A<-array(0,c(SIZE,SIZE,ITER))
for (i in 1:ITER) {A[,,i]<-rbind(Acolone[,,i],Acoltwo[,,i])}

##IMPLIED TFR CALCULATION
ImpliedTFR2010<-((TMinusOneAgeInit[1]+TMinusOneAgeInit[HALFSIZE+1])/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH
ImpliedTFR2015<-((TMinusZeroAgeInit[1]+TMinusZeroAgeInit[HALFSIZE+1])/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH
ImpliedTFR<-array(ImpliedTFR2015,ITER)

##RUN THE PROJECTION WITH SURV ADJUSTMENT (BY SOURCE() OF PROJECTION FILE)
repeat{
SurvChange<-array(0,ITER)
SurvChange_e<-array(0,ITER)
		##ADDING TO BRASS ALPHA WITH EACH STEP
		for (i in 1:ITER) {SurvChange_e[i]<-rnorm(1,0,BA_se[i])}
		if(ImputeMort=="YES") {for (i in 1:ITER) {SurvChange[i]<-BA_start[i]+BA_end[i]+SurvChange_e[i]}}
		for (i in 1:ITER) {BA_start[i]<-SurvChange[i]}
	source("https://raw.githubusercontent.com/edyhsgr/CCRStable/master/CCR_Unc_CA_Supporting_Project.R",local=TRUE)
		
		##MAKING TIME SERIES OBJECTS
		assign(paste(text=c("ImpliedTFR_",CURRENTSTEP),collapse=""),ImpliedTFRNew[])
		assign(paste(text=c("NetMigrAdj_",CURRENTSTEP),collapse=""),NetMigrAdjust[])
		assign(paste(text=c("e0F_",CURRENTSTEP),collapse=""),e0FAdj[])
		assign(paste(text=c("e0M_",CURRENTSTEP),collapse=""),e0MAdj[])

		ImpliedTFR_0<-ImpliedTFR2015
		NetMigrAdj_0<-0
		e0F_0<-e0FStart
		e0M_0<-e0MStart

		ImpliedTFR_Project<-paste0('ImpliedTFR_',0:CURRENTSTEP)
		NetMigrAdj_Project<-paste0('NetMigrAdj_',0:CURRENTSTEP)
		e0F_Project<-paste0('e0F_',0:CURRENTSTEP)
		e0M_Project<-paste0('e0M_',0:CURRENTSTEP)
		
		ImpliedTFR_Project<-do.call(cbind,mget(ImpliedTFR_Project))
		NetMigrAdj_Project<-do.call(cbind,mget(NetMigrAdj_Project))
		e0F_Project<-do.call(cbind,mget(e0F_Project))
		e0M_Project<-do.call(cbind,mget(e0M_Project))

	CURRENTSTEP <- CURRENTSTEP+1

	if(CURRENTSTEP > STEPS) {break}}

    ##########
    ##TABLING DATA
    ##########
    
    #JUST ALL POPULATIONS USED IN GRAPHS
    NewAge_F<-array(0,c(HALFSIZE,1,ITER))
    NewAge_F_Median<-NewAge_F_Low<-NewAge_F_High<-array(0,c(HALFSIZE))
    NewAge_M<-array(0,c(HALFSIZE,1,ITER))
    NewAge_M_Median<-NewAge_M_Low<-NewAge_M_High<-array(0,c(HALFSIZE))

    for (i in 1:ITER) {NewAge_F[1:HALFSIZE,,i]<-TMinusZeroAge[1:HALFSIZE,,i]}
    for (i in 1:HALFSIZE) {NewAge_F_Median[i]<-median(NewAge_F[i,,])}
    for (i in 1:HALFSIZE) {NewAge_F_Low[i]<-quantile(NewAge_F[i,1,],.05,na.rm=TRUE)}
    for (i in 1:HALFSIZE) {NewAge_F_High[i]<-quantile(NewAge_F[i,1,],.95,na.rm=TRUE)}
NewAge_F<-NewAge_F_Median
    TMinusOneAgeInit_F<-TMinusOneAgeInit[1:HALFSIZE]
    TMinusZeroAgeInit_F<-TMinusZeroAgeInit[1:HALFSIZE]
    
    for (i in 1:ITER) {NewAge_M[1:HALFSIZE,,i]<-TMinusZeroAge[(HALFSIZE+1):SIZE,,i]}
    for (i in 1:HALFSIZE) {NewAge_M_Median[i]<-median(NewAge_M[i,,])}
    for (i in 1:HALFSIZE) {NewAge_M_Low[i]<-quantile(NewAge_M[i,1,],.05,na.rm=TRUE)}
    for (i in 1:HALFSIZE) {NewAge_M_High[i]<-quantile(NewAge_M[i,1,],.95,na.rm=TRUE)}
NewAge_M<-NewAge_M_Median
    TMinusOneAgeInit_M<-TMinusOneAgeInit[(HALFSIZE+1):SIZE]
    TMinusZeroAgeInit_M<-TMinusZeroAgeInit[(HALFSIZE+1):SIZE]
    
    NewAge_T<-NewAge_F+NewAge_M
    NewAge_T_Low<-NewAge_F_Low+NewAge_M_Low
    NewAge_T_High<-NewAge_F_High+NewAge_M_High
    TMinusOneAgeInit_T<-TMinusOneAgeInit_F+TMinusOneAgeInit_M
    TMinusZeroAgeInit_T<-TMinusZeroAgeInit_F+TMinusZeroAgeInit_M
    
    NewAge<-array(c(NewAge_T,NewAge_F,NewAge_M),c(HALFSIZE,3))
    TMinusOneAgeInit<-array(c(TMinusOneAgeInit_T,TMinusOneAgeInit_F,TMinusOneAgeInit_M),c(HALFSIZE,3))
    TMinusZeroAgeInit<-array(c(TMinusZeroAgeInit_T,TMinusZeroAgeInit_F,TMinusZeroAgeInit_M),c(HALFSIZE,3))
    
    ##########
    ##GRAPHING DATA (SOME ~HACKY LABELING SO MAY [LIKELY] NOT RENDER WELL)
    ##########

agegroups<-c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
       
    ##THIRD GRAPH - PYRAMID (FEMALE PORTION)
    barplot2(NewAge_F,plot.ci=TRUE,ci.l=NewAge_F_Low,ci.u=NewAge_F_High,horiz=T,names=agegroups,cex.main=2,cex.names=1.2,cex.axis=1.5,space=0,xlim=c(max(NewAge_M)*2,0),col="dodger blue",las=1,main=paste(text=c("Female, ",PROJECTIONYEAR),collapse=""))
#mtext(side=1,c((e0FAdj[1])),line=0,adj=.29,col="dark green")

    ##FOURTH GRAPH - PYRAMID (MALE PORTION)
    barplot2(NewAge_M,plot.ci=TRUE,ci.l=NewAge_M_Low,ci.u=NewAge_M_High,horiz=T,names=FALSE,cex.main=2,cex.names=1.25,cex.axis=1.5,space=0,xlim=c(0,max(NewAge_M)*2),col="gold",main=paste(text=c("Male, ",PROJECTIONYEAR),collapse=""))

plot(ImpliedTFR_Project[1,],type="l",ylim=c(0,5),xlab="Time Step",ylab="",main="Implied TFR by Time Step End Year",cex.lab=2,cex.main=2,axes=F)
	for (i in 1:ITER) {lines(ImpliedTFR_Project[i,],col=sample(6))}
		axis(side=1,at=0:CURRENTSTEP,labels=paste(seq(2010,CURRENTSTEP*5+2010,5)),cex.axis=1.5)
		axis(side=2,cex.axis=1.5)

plot(NetMigrAdj_Project[1,],type="l",ylim=c(-.02,.02),xlab="Time Step",ylab="",main="Net Migration Adjustment by Time Step End Year",cex.lab=2,cex.main=2,axes=F)
	for (i in 1:ITER) {lines(NetMigrAdj_Project[i,],col=sample(6))}
		axis(side=1,at=0:CURRENTSTEP,labels=paste(seq(2010,CURRENTSTEP*5+2010,5)),cex.axis=1.5)
		axis(side=2,cex.axis=1.5)

plot(e0F_Project[1,],type="l",ylim=c(60,110),xlab="Time Step",ylab="",main="e0 (Female and Male) by Time Step End Year",cex.lab=2,cex.main=2,axes=F)
	for (i in 1:ITER) {lines(e0F_Project[i,],col=sample(6))}
	for (i in 1:ITER) {lines(e0M_Project[i,],col=sample(6))}
		axis(side=1,at=0:CURRENTSTEP,labels=paste(seq(2010,CURRENTSTEP*5+2010,5)),cex.axis=1.5)
		axis(side=2,cex.axis=1.5)

    }   
    },height=1800,width=1200)

}

shinyApp(ui = ui, server = server) 


