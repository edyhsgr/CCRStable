##########
##R CODE FOR COHORT CHANGE RATIO-BASED (HAMILTON-PERRY) WITH COMPONENTS AND STABLE POPULATION REVIEW SHINY APP - APPLIED TO FLORIDA, SINGLE-YEAR OF AGE
##
##EDDIE HUNSINGER, OCTOBER 2020 (UPDATED JANUARY 2022)
##https://edyhsgr.github.io/eddieh/
##
##APPLIED DEMOGRAPHY TOOLBOX LISTING: https://applieddemogtoolbox.github.io/Toolbox/#CCRStable
##
##MORE INFORMATION IS AVAILABLE AT: https://github.com/edyhsgr/CCRStable/tree/master/Oct2020Presentation
##
##IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, PLEASE CITE THE SOURCE
##This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 International License (more information: https://creativecommons.org/licenses/by-sa/3.0/igo/).
##
##THERE IS NO WARRANTY FOR THIS CODE
##THIS CODE HAS NOT BEEN PEER-REVIEWED OR CAREFULLY TESTED - QUESTIONS AND COMMENTS ARE WELCOME, OF COURSE (edyhsgr@gmail.com)
##########

#install.packages("shiny")
library(shiny)
ui<-fluidPage(
  
  tags$h3("Cohort Change Ratio-Based Stable Population Review Shiny App - Florida"),
  p("Related information and ",
    tags$a(href="https://www.r-project.org/", "R"),
    "code available at: ",
    tags$a(href="https://github.com/edyhsgr/CCRStable", 
           "CCRStable GitHub Repository")
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      
  radioButtons("radio","",c("Include stable population information" = 1, "Don't include stable population information" = 2),selected = 2),

  hr(),
      
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
                    "2018 to 2019"="21",
                    "2017 to 2018"="20",
                    "2016 to 2017"="19",
                    "2015 to 2016"="18",
                    "2014 to 2015"="17",
                    "2013 to 2014"="16",
                    "2012 to 2013"="15",
                    "2011 to 2012"="14",
                    "2010 to 2011"="13"
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
      numericInput("ImposedTFR_ar","If Yes, iTFR AR(1)",.00,0,.99,step=.01),

      hr(),

      numericInput("SRB","Projected sex ratio at birth",round((1-.4886)/.4886,3),0,2,step=.005),
      
      hr(),
      
      numericInput("NetMigrationAdjustLevel","Net migration adjustment (annual, percent of population)",0,-25,25,step=.1),

      numericInput("GrossMigrationAdjustLevel","Gross migration adjustment (percent of net migration ratios)",100,0,200,step=10),
            
      selectInput("GrossMigrationProfile", "Gross migration age profile to use", selected="Florida",
                  c(
                    "California"="California",
                    "Florida"="Florida",
                    "Sarasota County, Florida"="SarasotaFlorida",
                    "Kentucky"="Kentucky"
                  ),
      ),

      selectInput("ImputeMort", "Impute mortality?",
                  c(
                    "Yes"="YES",
                    "No"="NO"
                  ),
      ),
      
      numericInput("BAStart","If yes, Brass' model alpha for First projection step...",.018,-2,2,step=.03),
      numericInput("BAEnd","...and Brass' model alpha for Last projection step",.108,-2,2,step=.03),

      selectInput("LifeTable", "Life table standard to use", selected="Florida",
                  c(
                    "California"="California",
                    "Florida"="Florida",
                    "Kentucky"="Kentucky"
                  ),
      ),
      
      hr(),
      
      p("This interface was made with ",
        tags$a(href="https://shiny.rstudio.com/", 
               "Shiny for R."),
        
        tags$a(href="https://edyhsgr.github.io/", 
               "Eddie Hunsinger,"), 
        
        "August 2019 (updated January 2022)."),

      p("Information including ", 
	tags$a(href="https://github.com/edyhsgr/CCRStable/tree/master/Oct2020Presentation",
		"formulas, a spreadsheet demonstration, and slides for a related talk, "),
	"as well as ",
	tags$a(href="https://www.r-project.org/",
		"R"),
	"code with input files for several examples, including a ",
	tags$a(href="https://shiny.demog.berkeley.edu/eddieh/CCRUnc/",
		"stochastic version "), 
	"and an ",	
	tags$a(href="https://edyhsgr.shinyapps.io/CCRStable_ValView_Florida/",
		"errors review version"), 
	"is all available in the ",
	tags$a(href="https://github.com/edyhsgr/CCRStable", 
		"related GitHub repository.")),
      
      p("Population estimates inputs from ",
        tags$a(href="https://www.census.gov/programs-surveys/popest.html", 
               "US Census Bureau Vintage 2019 Population Estimates.")),
      
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
      
      p(tags$a(href="https://applieddemogtoolbox.github.io/#CCRStable", 
             "Applied Demography Toolbox listing.")),

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
K_Single<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/PopEstimates/sc-est2019-alldata6_Extract_Florida.csv",header=TRUE,sep=","))
K_Single<-subset(K_Single,K_Single$ORIGIN==0)
K<-K_Single

server<-function(input, output) {	
  output$plots<-renderPlot({
    par(mfrow=c(2,2))

    ##NUMBER FORMATTING
    options(scipen=999)
    
##USMD CA SURVIVAL DATA (GENERIC)
if(input$LifeTable=="California") {lt<-read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Mortality/lt_CA_USMD2010to2014Single.csv",header=TRUE,sep=",")
}
if(input$LifeTable=="Florida") {lt<-read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Mortality/lt_FL_USMD2010to2014Single.csv",header=TRUE,sep=",")
}
if(input$LifeTable=="Kentucky") {lt<-read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Mortality/lt_KY_USMD2010to2014Single.csv",header=TRUE,sep=",")
}
lxF<-lt$lx_Female/100000
lxM<-lt$lx_Male/100000
lxT<-lt$lx_Both/100000

##SELECT CENSUS ACS (via IPUMS) MIGRATION DATA
if(input$GrossMigrationProfile=="California") {
Migration<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Migration/AGenericMigrationProfileSingleYrOfAge_CA_2013to2017ACS.csv",header=TRUE,sep=","))
Migration<-c(Migration$CA_F[1:86],Migration$CA_M[1:86])
}
if(input$GrossMigrationProfile=="Florida") {
Migration<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Migration/AGenericMigrationProfileSingleYrOfAge_FL_2013to2017ACS.csv",header=TRUE,sep=","))
Migration<-c(Migration$FL_F[1:86],Migration$FL_M[1:86])
}
if(input$GrossMigrationProfile=="SarasotaFlorida") {
Migration<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Migration/AGenericMigrationProfileSingleYrOfAge_SarasotaFL_2013to2017ACS.csv",header=TRUE,sep=","))
Migration<-c(Migration$SarasotaFL_F[1:86],Migration$SarasotaFL_M[1:86])
}
if(input$GrossMigrationProfile=="Kentucky") {
Migration<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Migration/AGenericMigrationProfileSingleYrOfAge_KY_2013to2017ACS.csv",header=TRUE,sep=","))
Migration<-c(Migration$KY_F[1:86],Migration$KY_M[1:86])
}
    
    ##########
    ##SCRIPT INPUTS
    ##########
    
    ##DIMENSIONS
    SIZE<-172
    HALFSIZE<-SIZE/2
    STEPS<-(input$STEP-2015)
    STEPSSTABLE<-STEPS+1000
    CURRENTSTEP<-0
    CURRENTSTEPSTABLE<-0
    PROJECTIONYEAR<-STEPS+2015
    FERTWIDTH<-35
    
    ##SELECTING RATIOS BASIS
    FirstYear<-strtoi(input$RatiosFrom)-1
    SecondYear<-strtoi(input$RatiosFrom)
    
    ##IMPOSED TFR OPTION
    ImposedTFR<-input$ImposedTFR
    ffab<-1/(input$SRB+1)
    UseImposedTFR<-input$ImposeTFR
    
    ##ADJUST BY MIGRATION OPTION
    GrossMigrationAdjustLevel<-input$GrossMigrationAdjustLevel/100
    NetMigrationAdjustLevel<-input$NetMigrationAdjustLevel/100
    
    ##IMPUTE MORTALITY OPTION
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
    
    ##SELECT BY SEX
    SelectBySex<-input$Sex
    
    ##SELECTING FROM THE INPUT POPULATION TABLE (K) BASED ON INPUTS (2010 AND 2015 INFO FOR GRAPHS, AND 2015 INFO FOR JUMP OFF, AND 2010 THROUGH 2019 AS INPUT OPTIONS RATIOS)
    K_MinusOneInit<-aggregate(K_Single[,12], by = list(K_Single$AGE,K_Single$SEX),FUN = sum)
    K_MinusOneInit_F<-subset(K_MinusOneInit,K_MinusOneInit$Group.2==2)
    TMinusOneAge_F<-TMinusOneAgeInit_F<-K_MinusOneInit_F$x

    K_MinusOneInit<-aggregate(K_Single[,12], by = list(K_Single$AGE,K_Single$SEX),FUN = sum)
    K_MinusOneInit_M<-subset(K_MinusOneInit,K_MinusOneInit$Group.2==1)
    TMinusOneAge_M<-TMinusOneAgeInit_M<-K_MinusOneInit_M$x
    
    TMinusOneAge<-TMinusOneAgeInit<-c(TMinusOneAge_F,TMinusOneAge_M)
    
    K_MinusOneInitRatios<-aggregate(K_Single[,strtoi(input$RatiosFrom)-1], by = list(K_Single$AGE,K_Single$SEX),FUN = sum)
    K_MinusOneInitRatios_F<-subset(K_MinusOneInitRatios,K_MinusOneInitRatios$Group.2==2)
    TMinusOneAgeRatios_F<-TMinusOneInitRatios_F<-K_MinusOneInitRatios_F$x

    K_MinusOneInitRatios<-aggregate(K_Single[,strtoi(input$RatiosFrom)-1], by = list(K_Single$AGE,K_Single$SEX),FUN = sum)
    K_MinusOneInitRatios_M<-subset(K_MinusOneInitRatios,K_MinusOneInitRatios$Group.2==1)
    TMinusOneAgeRatios_M<-TMinusOneInitRatios_M<-K_MinusOneInitRatios_M$x
    
    TMinusOneAgeRatios<-TMinusOneAgeInitRatios<-c(TMinusOneAgeRatios_F,TMinusOneAgeRatios_M)

    K_MinusZeroInit<-aggregate(K_Single[,17], by = list(K_Single$AGE,K_Single$SEX),FUN = sum)
    K_MinusZeroInit_F<-subset(K_MinusZeroInit,K_MinusZeroInit$Group.2==2)
    TMinusZeroAge_F<-TMinusZeroAgeInit_F<-K_MinusZeroInit_F$x

    K_MinusZeroInit<-aggregate(K_Single[,17], by = list(K_Single$AGE,K_Single$SEX),FUN = sum)
    K_MinusZeroInit_M<-subset(K_MinusZeroInit,K_MinusZeroInit$Group.2==1)
    TMinusZeroAge_M<-TMinusZeroAgeInit_M<-K_MinusZeroInit_M$x
    
    TMinusZeroAge<-TMinusZeroAgeInit<-c(TMinusZeroAge_F,TMinusZeroAge_M)
    
    K_MinusZeroInitRatios<-aggregate(K_Single[,strtoi(input$RatiosFrom)], by = list(K_Single$AGE,K_Single$SEX),FUN = sum)
    K_MinusZeroInitRatios_F<-subset(K_MinusZeroInitRatios,K_MinusZeroInitRatios$Group.2==2)
    TMinusZeroAgeRatios_F<-TMinusZeroInitRatios_F<-K_MinusZeroInitRatios_F$x

    K_MinusZeroInitRatios<-aggregate(K_Single[,strtoi(input$RatiosFrom)], by = list(K_Single$AGE,K_Single$SEX),FUN = sum)
    K_MinusZeroInitRatios_M<-subset(K_MinusZeroInitRatios,K_MinusZeroInitRatios$Group.2==1)
    TMinusZeroAgeRatios_M<-TMinusZeroInitRatios_M<-K_MinusZeroInitRatios_M$x
    
    TMinusZeroAgeRatios<-TMinusZeroAgeInitRatios<-c(TMinusZeroAgeRatios_F,TMinusZeroAgeRatios_M)
    
    ##########
    ##CALCULATIONS
    ##########
    
    ##COHORT CHANGE RATIOS
    Ratios<-array(,length(TMinusOneAgeRatios))
    for (i in 2:(HALFSIZE-1)) {Ratios[i]<-TMinusZeroAgeRatios[i]/TMinusOneAgeRatios[i-1]}
    for (i in (HALFSIZE+2):(SIZE-1)) {Ratios[i]<-TMinusZeroAgeRatios[i]/TMinusOneAgeRatios[i-1]}
    
    ##PLACING COHORT CHANGE RATIOS (FEMALE)
    S_F<-array(0,c(HALFSIZE,HALFSIZE))
    S_F<-rbind(0,cbind(diag(Ratios[2:(HALFSIZE)]),0))
    
    ##OPEN-ENDED AGE GROUP (FEMALE)
    S_F[HALFSIZE,HALFSIZE-1]<-TMinusZeroAgeRatios[HALFSIZE]/(TMinusOneAgeRatios[HALFSIZE-1]+TMinusOneAgeRatios[HALFSIZE])
    Ratios[HALFSIZE]<-S_F[HALFSIZE,HALFSIZE]<-S_F[HALFSIZE,HALFSIZE-1]
    
    ##BIRTHS AND MATRIX PORTION CONSTRUCTION (FEMALE)
    B_F<-0*S_F
    B_F[1,16:50]<-Ratios[1]*ffab
    A_F<-B_F+S_F

    ##PLACING COHORT CHANGE RATIOS (MALE)
    S_M<-array(0,c(HALFSIZE,HALFSIZE))
    S_M<-rbind(0,cbind(diag(Ratios[(HALFSIZE+2):SIZE]),0)) ##########################FLAG##########################
    
    ##OPEN-ENDED AGE GROUP (MALE)
    S_M[HALFSIZE,HALFSIZE-1]<-TMinusZeroAgeRatios[SIZE]/(TMinusOneAgeRatios[SIZE-1]+TMinusOneAgeRatios[SIZE])
    Ratios[SIZE]<-S_M[HALFSIZE,HALFSIZE]<-S_M[HALFSIZE,HALFSIZE-1]
    
    ##BIRTHS AND MATRIX PORTION CONSTRUCTION (MALE)
    B_M<-0*S_M
    B_M[1,16:50]<-Ratios[1]*(1-ffab)
      
    ##STRUCTURAL ZEROES
    AEnd_Zero<-A_Zero<-array(0,c(HALFSIZE,HALFSIZE))
    
    ##MAKING FULL PROJECTION MATRIX (TWO-SEX)
    Acolone<-cbind(A_F,A_Zero)
    Acoltwo<-cbind(B_M,S_M)
    A<-rbind(Acolone,Acoltwo)
    
    ##IMPLIED TFR CALCUATION
    ImpliedTFR2010<-((TMinusOneAgeInit[1]+TMinusOneAgeInit[HALFSIZE+1]))/sum(TMinusZeroAgeInit[16:50])*FERTWIDTH
    ImpliedTFR2015<-((TMinusZeroAgeInit[1]+TMinusZeroAgeInit[HALFSIZE+1]))/sum(TMinusZeroAgeInit[16:50])*FERTWIDTH
          
      ##########
      ##PROJECTION FUNCTION
      ##########
  
      ##MAX STEPS IN CASE USER (ESP ME) GETS CARRIED AWAY
      if(STEPS<500){     
  
      ##FUNCTION INPUTTING
      CCRProject<-function(TMinusZeroAge,ImpliedTFR,BA_start,BA_end,CURRENTSTEP){

	##CALCULATE SURVIVAL ADJUSTMENT (Yx, lx, Lx, Sx)
	YxF<-YxM<-NULL
	for (i in 1:length(lxF)){YxF[i]<-.5*log(lxF[i]/(1-lxF[i]))}
	for (i in 1:length(lxM)){YxM[i]<-.5*log(lxM[i]/(1-lxM[i]))}
	
	lxFStart<-array(0,length(lxF))
	lxMStart<-array(0,length(lxM))
	for (i in 1:length(lxFStart)){lxFStart[i]<-1/(1+exp(-2*BA_start-2*BB*YxF[i]))}
	for (i in 1:length(lxMStart)){lxMStart[i]<-1/(1+exp(-2*BA_start-2*BB*YxM[i]))}
	
	LxFStart<-array(,length(lxF))
	LxMStart<-array(,length(lxM))
	##**THIS IS A LITTLE OFF FOR THE FIRST AGE GROUP**
	for (i in 1:length(LxFStart)){LxFStart[i]<-.5*(lxFStart[i]+lxFStart[i+1])}
	for (i in 1:length(LxMStart)){LxMStart[i]<-.5*(lxMStart[i]+lxMStart[i+1])}
	
	SxFStart<-array(,HALFSIZE)
	SxMStart<-array(,HALFSIZE)
	for (i in 2:HALFSIZE){SxFStart[i]<-(LxFStart[i]/LxFStart[i-1])}
	for (i in 2:HALFSIZE){SxMStart[i]<-(LxMStart[i]/LxMStart[i-1])}	

	##(OPEN-ENDED AGE GROUP (FEMALE))
	SxFStart[HALFSIZE]<-rev(cumsum(rev(LxFStart[HALFSIZE:(length(LxFStart)-1)])))[1]/rev(cumsum(rev(LxFStart[(HALFSIZE-1):(length(LxFStart)-1)])))[1]
	
	##(OPEN-ENDED AGE GROUP (MALE))
	SxMStart[HALFSIZE]<-rev(cumsum(rev(LxMStart[HALFSIZE:(length(LxMStart)-1)])))[1]/rev(cumsum(rev(LxMStart[(HALFSIZE-1):(length(LxMStart)-1)])))[1]

	##INITIAL e0
	e0FStart<-sum(LxFStart[1:(length(LxFStart)-1)]) ###################FLAG########################
	e0MStart<-sum(LxMStart[1:(length(LxFStart)-1)])

	lxFAdj<-array(0,length(lxF))
	lxMAdj<-array(0,length(lxM))

	##INTERPOLATING BRASS ALPHA BETWEEN FIRST AND LAST STEP
	if(CURRENTSTEP<=STEPS){
	for (i in 1:length(lxFAdj)){lxFAdj[i]<-1/(1+exp(-2*(BA_start*(1-CURRENTSTEP/STEPS)+BA_end*(CURRENTSTEP/STEPS))-2*BB*YxF[i]))}
	for (i in 1:length(lxMAdj)){lxMAdj[i]<-1/(1+exp(-2*(BA_start*(1-CURRENTSTEP/STEPS)+BA_end*(CURRENTSTEP/STEPS))-2*BB*YxM[i]))}
	}

	##ALLOWING FOR LONG-TERM (STABLE POPULATION) SIMULATION
	if(CURRENTSTEP>=STEPS){
	for (i in 1:length(lxFAdj)){lxFAdj[i]<-1/(1+exp(-2*BA_end-2*BB*YxF[i]))}
	for (i in 1:length(lxMAdj)){lxMAdj[i]<-1/(1+exp(-2*BA_end-2*BB*YxM[i]))}
	}

	##SURVIVAL ADJUSTMENTS (Lx, SX)
	LxFAdj<-array(,length(lxF))
	LxMAdj<-array(,length(lxM))
	##**THIS IS A LITTLE OFF FOR THE FIRST AGE GROUP**
	for (i in 1:length(LxFAdj)){LxFAdj[i]<-.5*(lxFAdj[i]+lxFAdj[i+1])}
	for (i in 1:length(LxMAdj)){LxMAdj[i]<-.5*(lxMAdj[i]+lxMAdj[i+1])}

	SxFAdj<-array(,HALFSIZE)
	SxMAdj<-array(,HALFSIZE)
	for (i in 2:length(SxFAdj)){SxFAdj[i]<-(LxFAdj[i]/LxFAdj[i-1])}
	for (i in 2:length(SxMAdj)){SxMAdj[i]<-(LxMAdj[i]/LxMAdj[i-1])}

	##(OPEN-ENDED AGE GROUP (FEMALE))
	SxFAdj[HALFSIZE]<-rev(cumsum(rev(LxFAdj[HALFSIZE:(length(LxFAdj)-1)])))[1]/rev(cumsum(rev(LxFAdj[(HALFSIZE-1):(length(LxFAdj)-1)])))[1]

	##(OPEN-ENDED AGE GROUP (MALE))
	SxMAdj[HALFSIZE]<-rev(cumsum(rev(LxMAdj[HALFSIZE:(length(LxMAdj)-1)])))[1]/rev(cumsum(rev(LxMAdj[(HALFSIZE-1):(length(LxMAdj)-1)])))[1]

	##ADJUSTED e0
	e0FAdj<-sum(LxFAdj[1:(length(LxFStart)-1)])
	e0MAdj<-sum(LxMAdj[1:(length(LxFStart)-1)])

        ##ADJUST GROSS MIGRATION OPTION
        if(GrossMigrationAdjustLevel!=1){
            RatiosGrossMigAdj<-Ratios
            for (i in 2:HALFSIZE) {RatiosGrossMigAdj[i]<-(Ratios[i]-SxFStart[i])*GrossMigrationAdjustLevel+SxFStart[i]}
            SGrossMigAdj_F<-array(0,c(HALFSIZE,HALFSIZE))
            SGrossMigAdj_F<-rbind(0,cbind(diag(RatiosGrossMigAdj[2:HALFSIZE]),0))
            ##OPEN-ENDED AGE GROUP (FEMALE)
            SGrossMigAdj_F[HALFSIZE,HALFSIZE]<-SGrossMigAdj_F[HALFSIZE,HALFSIZE-1]
            S_F<-SGrossMigAdj_F
            A_F<-B_F+S_F
            
            for (i in (HALFSIZE+2):SIZE) {RatiosGrossMigAdj[i]<-(Ratios[i]-SxMStart[i-HALFSIZE])*GrossMigrationAdjustLevel+SxMStart[i-HALFSIZE]}
            SGrossMigAdj_M<-array(0,c(HALFSIZE,HALFSIZE))
            SGrossMigAdj_M<-rbind(0,cbind(diag(RatiosGrossMigAdj[(HALFSIZE+2):SIZE]),0))
            ##OPEN-ENDED AGE GROUP (MALE)
            SGrossMigAdj_M[HALFSIZE,HALFSIZE]<-SGrossMigAdj_M[HALFSIZE,HALFSIZE-1]
            S_M<-SGrossMigAdj_M
            }

	##CONSTRUCT PROJECTION MATRICES WITH SURVIVAL ADJUSTMENT
	SAdj_F<-array(0,c(HALFSIZE,HALFSIZE))
	SAdj_F<-rbind(0,cbind(diag(SxFAdj[2:HALFSIZE]-SxFStart[2:HALFSIZE]),0))
	SAdj_F[HALFSIZE,HALFSIZE]<-SAdj_F[HALFSIZE,HALFSIZE-1]
	SAdj_F<-SAdj_F+S_F
	AAdj_F<-B_F+SAdj_F

	SAdj_M<-array(0,c(HALFSIZE,HALFSIZE))
	SAdj_M<-rbind(0,cbind(diag(SxMAdj[2:HALFSIZE]-SxMStart[2:HALFSIZE]),0))
	SAdj_M[HALFSIZE,HALFSIZE]<-SAdj_M[HALFSIZE,HALFSIZE-1]
	SAdj_M<-SAdj_M+S_M

	AAdj_Zero<-A_Zero<-array(0,c(HALFSIZE,HALFSIZE))

	Acolone<-cbind(A_F,A_Zero)
	Acoltwo<-cbind(B_M,S_M)
	A<-rbind(Acolone,Acoltwo)

	AAdjcolone<-cbind(AAdj_F,AAdj_Zero)
	AAdjcoltwo<-cbind(B_M,SAdj_M)
	AAdj<-rbind(AAdjcolone,AAdjcoltwo)
              
	##PROJECTION IMPLEMENTATION (WITH FERTILITY AND MIGRATION ADJUSTMENTS)
	TMinusOneAgeNew<-data.frame(TMinusZeroAge) 
		if(CURRENTSTEP>0){
				TMinusZeroAge<-AAdj%*%TMinusZeroAge
		if(NetMigrationAdjustLevel!=0)
				{TMinusZeroAge<-NetMigrationAdjustLevel*1*sum(TMinusOneAgeNew)*Migration+TMinusZeroAge} ##########FLAG##########
		if(UseImposedTFR=="YES") 
				{TMinusZeroAge[1]<-(ImpliedTFR*input$ImposedTFR_ar+ImposedTFR*(1-input$ImposedTFR_ar))*(sum(TMinusZeroAge[16:50])/FERTWIDTH)*1*ffab
				TMinusZeroAge[HALFSIZE+1]<-(ImpliedTFR*input$ImposedTFR_ar+ImposedTFR*(1-input$ImposedTFR_ar))*(sum(TMinusZeroAge[16:50])/FERTWIDTH)*1*(1-ffab)}
		if(UseImposedTFR=="NO") 
				{TMinusZeroAge[1]<-ImpliedTFR*(sum(TMinusZeroAge[16:50])/FERTWIDTH)*1*ffab
				TMinusZeroAge[HALFSIZE+1]<-ImpliedTFR*(sum(TMinusZeroAge[16:50])/FERTWIDTH)*1*(1-ffab)}
				}
        TMinusZeroAge_NDF<-TMinusZeroAge
	TMinusZeroAge<-data.frame(TMinusZeroAge)

	##CALCULATE iTFR
	ImpliedTFRNew<-((TMinusZeroAge_NDF[1]+TMinusZeroAge_NDF[HALFSIZE+1])/1)/sum(TMinusZeroAge_NDF[16:50])*FERTWIDTH

	return(c(TMinusZeroAge=TMinusZeroAge,TMinusOneAge=TMinusOneAgeNew,ImpliedTFRNew=ImpliedTFRNew,e0FStart=e0FStart,e0MStart=e0MStart,e0FAdj=e0FAdj,e0MAdj=e0MAdj,CURRENTSTEP=CURRENTSTEP+1))
	}
      }
   
    ##APPLY PROJECTIONS
    CCRNew<-CCRProject(TMinusZeroAge,ImpliedTFR2015,BA_start,BA_end,CURRENTSTEP)
    while(CCRNew$CURRENTSTEP<STEPS+1) {CCRNew<-CCRProject(CCRNew$TMinusZeroAge,CCRNew$ImpliedTFRNew,BA_start,BA_end,CCRNew$CURRENTSTEP)}
    
    ##CALCULATE EFFECTIVE COHORT CHANGE RATIOS
    CCRatios<-Ratios
    for (i in 2:(HALFSIZE-1)) {CCRatios[i]<-CCRNew$TMinusZeroAge[i]/CCRNew$TMinusOneAge[i-1]}
    for (i in (HALFSIZE+2):(SIZE-1)) {CCRatios[i]<-CCRNew$TMinusZeroAge[i]/CCRNew$TMinusOneAge[i-1]}
    ##OPEN-ENDED AGE GROUPS
    CCRatios[HALFSIZE]<-CCRNew$TMinusZeroAge[HALFSIZE]/(CCRNew$TMinusOneAge[HALFSIZE-1]+CCRNew$TMinusOneAge[HALFSIZE])
    CCRatios[SIZE]<-CCRNew$TMinusZeroAge[SIZE]/(CCRNew$TMinusOneAge[SIZE-1]+CCRNew$TMinusOneAge[SIZE])
    ##BY SEX
    CCRatiosF<-CCRatios[2:HALFSIZE]
    CCRatiosM<-CCRatios[2+HALFSIZE:(SIZE-2)]
    
    ##iTFR
    ImpliedTFRNew<-CCRNew$ImpliedTFRNew
    
    ##ESTIMATE STABLE POPULATION BY SIMULATION
    TMinusZeroAge<-TMinusZeroAgeInit
    CCRStable<-CCRProject(TMinusZeroAge,ImpliedTFR2015,BA_start,BA_end,0)
    while(CCRStable$CURRENTSTEP<STEPSSTABLE+1) {CCRStable<-CCRProject(CCRStable$TMinusZeroAge,CCRStable$ImpliedTFRNew,BA_start,BA_end,CCRStable$CURRENTSTEP)}
    ImpliedTFRStable<-((CCRStable$TMinusZeroAge[1]+CCRStable$TMinusZeroAge[HALFSIZE+1])/5)/sum(CCRStable$TMinusZeroAge[4:10])*FERTWIDTH
    
    ##########
    ##TABLING DATA
    ##########
    
    #JUST ALL POPULATIONS USED IN GRAPHS
    NewAge_F<-CCRNew$TMinusZeroAge[1:HALFSIZE]
if (input$radio==1) {StableAge_F<-CCRStable$TMinusZeroAge[1:HALFSIZE]}
    TMinusOneAgeInit_F<-TMinusOneAgeInit[1:HALFSIZE]
    TMinusZeroAgeInit_F<-TMinusZeroAgeInit[1:HALFSIZE]
    
    NewAge_M<-CCRNew$TMinusZeroAge[(HALFSIZE+1):SIZE]
if (input$radio==1) {StableAge_M<-CCRStable$TMinusZeroAge[(HALFSIZE+1):SIZE]}
    TMinusOneAgeInit_M<-TMinusOneAgeInit[(HALFSIZE+1):SIZE]
    TMinusZeroAgeInit_M<-TMinusZeroAgeInit[(HALFSIZE+1):SIZE]
    
    NewAge_T<-NewAge_F+NewAge_M
if (input$radio==1) {StableAge_T<-StableAge_F+StableAge_M}
    TMinusOneAgeInit_T<-TMinusOneAgeInit_F+TMinusOneAgeInit_M
    TMinusZeroAgeInit_T<-TMinusZeroAgeInit_F+TMinusZeroAgeInit_M
    
    NewAge<-array(c(NewAge_T,NewAge_F,NewAge_M),c(HALFSIZE,3))
if (input$radio==1) {StableAge<-array(c(StableAge_T,StableAge_F,StableAge_M),c(HALFSIZE,3))}
    TMinusOneAgeInit<-array(c(TMinusOneAgeInit_T,TMinusOneAgeInit_F,TMinusOneAgeInit_M),c(HALFSIZE,3))
    TMinusZeroAgeInit<-array(c(TMinusZeroAgeInit_T,TMinusZeroAgeInit_F,TMinusZeroAgeInit_M),c(HALFSIZE,3))
    
    ##########
    ##GRAPHING DATA (SOME ~HACKY LABELING SO MAY [LIKELY] NOT RENDER WELL)
    ##########
    
    ##FIRST GRAPH - MAJOR SUMMARY
    agegroups<-as.character(c(0:85))
    agegroups[86]<-"85+"
    if(SelectBySex=="Total") {plot(TMinusOneAgeInit[,1]/sum(TMinusOneAgeInit[,1]),type="l",col="orange",main=paste(text=c("Florida",", ",input$Sex),collapse=""),ylim=c(0,.025),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
    if(SelectBySex=="Female") {plot(TMinusOneAgeInit[,2]/sum(TMinusOneAgeInit[,2]),type="l",col="orange",main=paste(text=c("Florida",", ",input$Sex),collapse=""),ylim=c(0,.025),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
    if(SelectBySex=="Male") {plot(TMinusOneAgeInit[,3]/sum(TMinusOneAgeInit[,3]),type="l",col="orange",main=paste(text=c("Florida",", ",input$Sex),collapse=""),ylim=c(0,.025),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
    
    if(SelectBySex=="Total") {lines(TMinusZeroAgeInit[,1]/sum(TMinusZeroAgeInit[,1]),col="blue",lwd=4)}
    if(SelectBySex=="Female") {lines(TMinusZeroAgeInit[,2]/sum(TMinusZeroAgeInit[,2]),col="blue",lwd=4)}
    if(SelectBySex=="Male") {lines(TMinusZeroAgeInit[,3]/sum(TMinusZeroAgeInit[,3]),col="blue",lwd=4)}
    
    if(SelectBySex=="Total") {lines(NewAge[,1]/sum(NewAge[,1]),col="dark green",lty=1,lwd=4)}
    if(SelectBySex=="Female") {lines(NewAge[,2]/sum(NewAge[,2]),col="dark green",lty=1,lwd=4)}
    if(SelectBySex=="Male") {lines(NewAge[,3]/sum(NewAge[,3]),col="dark green",lty=1,lwd=4)}
    
    if (input$radio==1) {
      mtext(side=1,"Age groups",line=4,cex=.75)
      axis(side=1,at=1:HALFSIZE,las=2,labels=agegroups,cex.axis=0.9)
      axis(side=2)
      legend(11.5, .0225, legend=c("2010 (estimate)","2015 (estimate)",paste(c(PROJECTIONYEAR),"(projection)"),"Stable"),
             col=c("orange","blue","dark green","black"), lty=c(1,1,1,3),lwd=c(4,4,4,1.5),cex=1.2)
    }
    
    if (input$radio==2) {
      mtext(side=1,"Age groups",line=4,cex=.75)
      axis(side=1,at=1:HALFSIZE,las=2,labels=agegroups,cex.axis=0.9)
      axis(side=2)
      legend(11.5, .0225, legend=c("2010 (estimate)","2015 (estimate)",paste(c(PROJECTIONYEAR),"(projection)")),
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
    
    if(SelectBySex=="Total") {GROWTHRATE<-paste(text=c("R (2015 to ",PROJECTIONYEAR,"):  ", round(log(sum(NewAge[,1])/sum(TMinusZeroAgeInit[,1]))/(STEPS)*100,2)),collapse="")}
    if(SelectBySex=="Female") {GROWTHRATE<-paste(text=c("R (2015 to ",PROJECTIONYEAR,"):  ", round(log(sum(NewAge[,2])/sum(TMinusZeroAgeInit[,2]))/(STEPS)*100,2)),collapse="")}
    if(SelectBySex=="Male") {GROWTHRATE<-paste(text=c("R (2015 to ",PROJECTIONYEAR,"):  ", round(log(sum(NewAge[,3])/sum(TMinusZeroAgeInit[,3]))/(STEPS)*100,2)),collapse="")}
    mtext(side=1,c(GROWTHRATE),line=-10,adj=.15,col="dark green")
    
    if (input$radio==1) {
      if(SelectBySex=="Total") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ", round(log(sum(StableAge[,1])/sum(TMinusZeroAgeInit[,1]))/(STEPSSTABLE)*100,2)),collapse="")}
      if(SelectBySex=="Female") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ", round(log(sum(StableAge[,2])/sum(TMinusZeroAgeInit[,2]))/(STEPSSTABLE)*100,2)),collapse="")}
      if(SelectBySex=="Male") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ", round(log(sum(StableAge[,3])/sum(TMinusZeroAgeInit[,3]))/(STEPSSTABLE)*100,2)),collapse="")}
      mtext(side=1,c(STABLEGROWTHRATE),line=-9,adj=.15,col="black")
      
      if(SelectBySex=="Total") {lines(StableAge[,1]/sum(StableAge[,1]),col="black",lty=3,lwd=1.5)}
      if(SelectBySex=="Female") {lines(StableAge[,2]/sum(StableAge[,2]),col="black",lty=3,lwd=1.5)}
      if(SelectBySex=="Male") {lines(StableAge[,3]/sum(StableAge[,3]),col="black",lty=3,lwd=1.5)}
    }
    
#    if (input$radio==2) {
#      if(SelectBySex=="Total") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ..."),collapse="")}
#      if(SelectBySex=="Female") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ..."),collapse="")}
#      if(SelectBySex=="Male") {STABLEGROWTHRATE<-paste(text=c("~r (2015 forward):  ..."),collapse="")}
#      mtext(side=1,c(STABLEGROWTHRATE),line=-9,adj=.15,col="black")
#    }
    
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
    
    ##SECOND GRAPH - COHORT CHANGE RATIOS WITH AND WITHOUT ADJUSTMENTS
    agegroups2<-as.character(c(1:85))
    agegroups2[85]<-"85+"
    plot(Ratios[2:86],type="l",col="dodger blue",main=paste(text=c("Effective Cohort Change Ratios, ",PROJECTIONYEAR-1," to ",PROJECTIONYEAR),collapse=""),ylim=c(.8,1.15),axes=FALSE,xlab="",ylab="Ratio",lwd=4)
    ##OPEN-ENDED AGE GROUP OPTION
    mtext(side=1,c("(Note: 85+ ratios are applied to full 84+ age groups)"),line=-42,adj=.50,col="black")
    abline(a=NULL, b=NULL, h=1, v=NULL)
    lines(Ratios[88:172],type="l",col="gold",lwd=4)
    lines(CCRatiosF,type="l",col="dodger blue",lty=2,lwd=2)
    lines(CCRatiosM,type="l",col="gold",lty=2,lwd=2)
    mtext(side=1,"Age groups",line=4,cex=.75)
    axis(side=1,at=1:(HALFSIZE-1),labels=agegroups2,las=2,cex.axis=0.9)
    axis(side=2)
    legend(7,1.125, legend=c("Female","Male", "Female, with migration and mortality adjustments","Male, with migration and mortality adjustments"),
           col=c("dodger blue","gold","dodger blue","gold"), lty=c(1,1,2,2),lwd=c(4,4,2,2),cex=1.2)
    
    if (input$ImputeMort=="YES") {
      mtext(side=1,c("Imputed e0, female:"),line=-10,adj=.125,col="black")
      mtext(side=1,c(round(CCRNew$e0FAdj,1)),line=-10,adj=.35,col="black")
      mtext(side=1,c("Imputed e0, male:"),line=-9,adj=.122,col="black")
      mtext(side=1,c(round(CCRNew$e0MAdj,1)),line=-9,adj=.35,col="black")
    }
    
    ##THIRD GRAPH - PYRAMID (FEMALE PORTION)
    barplot(NewAge_F,horiz=T,names=agegroups,space=0,xlim=c(max(NewAge_F)*1.1,0),col="dodger blue",las=1,main=paste(text=c("Female, ",PROJECTIONYEAR),collapse=""))
    
    ##FOURTH GRAPH - PYRAMID (MALE PORTION)
    barplot(NewAge_M,horiz=T,names=FALSE,space=0,xlim=c(0,max(NewAge_F)*1.1),col="gold",main=paste(text=c("Male, ",PROJECTIONYEAR),collapse=""))
#}
    
  },height=1200,width=1200)
  
}

shinyApp(ui = ui, server = server) 

