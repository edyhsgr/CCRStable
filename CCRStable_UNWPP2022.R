##########
##R CODE FOR COHORT CHANGE RATIO-BASED (HAMILTON-PERRY) WITH COMPONENTS AND STABLE POPULATION REVIEW SHINY APP - APPLIED TO UN WPP 2022 Data - DRAFTING
##
##EDDIE HUNSINGER, August 2022
##https://edyhsgr.github.io/eddieh/
##
##APPLIED DEMOGRAPHY TOOLBOX LISTING: https://applieddemogtoolbox.github.io/Toolbox/#CCRStable
##
##AN MS EXCEL SPREADSHEET THAT REPLICATES METHODS AND RESULTS (2010 to 2015 ratios for Alameda County, California, with any adjustments) IS AVAILABLE AT: 
##https://github.com/edyhsgr/CCRStable/blob/master/Oct2020Presentation/CCRAdjustmentSheet_December2021.xlsx
##
####MORE INFORMATION IS AVAILABLE AT: https://github.com/edyhsgr/CCRStable/tree/master/Oct2020Presentation
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
  
  tags$h3("DRAFTING - Cohort Change Ratio-Based Stable Population Review Shiny App - Applied to UN WPP 2022 Data - DRAFTING"),
  p("Related information and ",
    tags$a(href="https://www.r-project.org/", "R"),
    "code available at: ",
    tags$a(href="https://github.com/edyhsgr/CCRStable", 
           "CCRStable GitHub Repository")
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(

	selectizeInput(inputId = "Area", label = "Area", 
	choices = c(
"Africa"="Africa",
"Asia"="Asia",
"Europe"="Europe",
"Latin America and the Caribbean"="Latin America and the Caribbean",
"Northern America"="Northern America",
"Oceania"="Oceania",
"Eastern Africa"="Eastern Africa",
"Burundi"="Burundi",
"Comoros"="Comoros",
"Djibouti"="Djibouti",
"Eritrea"="Eritrea",
"Ethiopia"="Ethiopia",
"Kenya"="Kenya",
"Madagascar"="Madagascar",
"Malawi"="Malawi",
"Mauritius"="Mauritius",
"Mayotte"="Mayotte",
"Mozambique"="Mozambique",
"RÃ©union"="RÃ©union",
"Rwanda"="Rwanda",
"Seychelles"="Seychelles",
"Somalia"="Somalia",
"South Sudan"="South Sudan",
"Uganda"="Uganda",
"United Republic of Tanzania"="United Republic of Tanzania",
"Zambia"="Zambia",
"Zimbabwe"="Zimbabwe",
"Middle Africa"="Middle Africa",
"Angola"="Angola",
"Cameroon"="Cameroon",
"Central African Republic"="Central African Republic",
"Chad"="Chad",
"Congo"="Congo",
"Democratic Republic of the Congo"="Democratic Republic of the Congo",
"Equatorial Guinea"="Equatorial Guinea",
"Gabon"="Gabon",
"Sao Tome and Principe"="Sao Tome and Principe",
"Southern Africa"="Southern Africa",
"Botswana"="Botswana",
"Eswatini"="Eswatini",
"Lesotho"="Lesotho",
"Namibia"="Namibia",
"South Africa"="South Africa",
"Western Africa"="Western Africa",
"Benin"="Benin",
"Burkina Faso"="Burkina Faso",
"Cabo Verde"="Cabo Verde",
"CÃ´te d'Ivoire"="CÃ´te d'Ivoire",
"Gambia"="Gambia",
"Ghana"="Ghana",
"Guinea"="Guinea",
"Guinea-Bissau"="Guinea-Bissau",
"Liberia"="Liberia",
"Mali"="Mali",
"Mauritania"="Mauritania",
"Niger"="Niger",
"Nigeria"="Nigeria",
"Senegal"="Senegal",
"Sierra Leone"="Sierra Leone",
"Togo"="Togo",
"Northern Africa"="Northern Africa",
"Algeria"="Algeria",
"Egypt"="Egypt",
"Libya"="Libya",
"Morocco"="Morocco",
"Sudan"="Sudan",
"Tunisia"="Tunisia",
"Western Sahara"="Western Sahara",
"Western Asia"="Western Asia",
"Armenia"="Armenia",
"Azerbaijan"="Azerbaijan",
"Bahrain"="Bahrain",
"Cyprus"="Cyprus",
"Georgia"="Georgia",
"Iraq"="Iraq",
"Israel"="Israel",
"Jordan"="Jordan",
"Kuwait"="Kuwait",
"Lebanon"="Lebanon",
"Oman"="Oman",
"Qatar"="Qatar",
"Saudi Arabia"="Saudi Arabia",
"State of Palestine"="State of Palestine",
"Syrian Arab Republic"="Syrian Arab Republic",
"Turkey"="Turkey",
"United Arab Emirates"="United Arab Emirates",
"Yemen"="Yemen",
"Central Asia"="Central Asia",
"Kazakhstan"="Kazakhstan",
"Kyrgyzstan"="Kyrgyzstan",
"Tajikistan"="Tajikistan",
"Turkmenistan"="Turkmenistan",
"Uzbekistan"="Uzbekistan",
"Southern Asia"="Southern Asia",
"Afghanistan"="Afghanistan",
"Bangladesh"="Bangladesh",
"Bhutan"="Bhutan",
"India"="India",
"Iran (Islamic Republic of)"="Iran (Islamic Republic of)",
"Maldives"="Maldives",
"Nepal"="Nepal",
"Pakistan"="Pakistan",
"Sri Lanka"="Sri Lanka",
"Eastern Asia"="Eastern Asia",
"China"="China",
"China, Hong Kong SAR"="China, Hong Kong SAR",
"China, Macao SAR"="China, Macao SAR",
"China, Taiwan Province of China"="China, Taiwan Province of China",
"Dem. People's Republic of Korea"="Dem. People's Republic of Korea",
"Japan"="Japan",
"Mongolia"="Mongolia",
"Republic of Korea"="Republic of Korea",
"South-Eastern Asia"="South-Eastern Asia",
"Brunei Darussalam"="Brunei Darussalam",
"Cambodia"="Cambodia",
"Indonesia"="Indonesia",
"Lao People's Democratic Republic"="Lao People's Democratic Republic",
"Malaysia"="Malaysia",
"Myanmar"="Myanmar",
"Philippines"="Philippines",
"Singapore"="Singapore",
"Thailand"="Thailand",
"Timor-Leste"="Timor-Leste",
"Viet Nam"="Viet Nam",
"Caribbean"="Caribbean",
"Antigua and Barbuda"="Antigua and Barbuda",
"Aruba"="Aruba",
"Bahamas"="Bahamas",
"Barbados"="Barbados",
"Cuba"="Cuba",
"CuraÃ§ao"="CuraÃ§ao",
"Dominican Republic"="Dominican Republic",
"Grenada"="Grenada",
"Guadeloupe"="Guadeloupe",
"Haiti"="Haiti",
"Jamaica"="Jamaica",
"Martinique"="Martinique",
"Puerto Rico"="Puerto Rico",
"Saint Lucia"="Saint Lucia",
"Saint Vincent and the Grenadines"="Saint Vincent and the Grenadines",
"Trinidad and Tobago"="Trinidad and Tobago",
"United States Virgin Islands"="United States Virgin Islands",
"Central America"="Central America",
"Belize"="Belize",
"Costa Rica"="Costa Rica",
"El Salvador"="El Salvador",
"Guatemala"="Guatemala",
"Honduras"="Honduras",
"Mexico"="Mexico",
"Nicaragua"="Nicaragua",
"Panama"="Panama",
"South America"="South America",
"Argentina"="Argentina",
"Bolivia (Plurinational State of)"="Bolivia (Plurinational State of)",
"Brazil"="Brazil",
"Chile"="Chile",
"Colombia"="Colombia",
"Ecuador"="Ecuador",
"French Guiana"="French Guiana",
"Guyana"="Guyana",
"Paraguay"="Paraguay",
"Peru"="Peru",
"Suriname"="Suriname",
"Uruguay"="Uruguay",
"Venezuela (Bolivarian Republic of)"="Venezuela (Bolivarian Republic of)",
"Australia"="Australia",
"New Zealand"="New Zealand",
"Melanesia"="Melanesia",
"Fiji"="Fiji",
"New Caledonia"="New Caledonia",
"Papua New Guinea"="Papua New Guinea",
"Solomon Islands"="Solomon Islands",
"Vanuatu"="Vanuatu",
"Micronesia"="Micronesia",
"Guam"="Guam",
"Kiribati"="Kiribati",
"Micronesia (Fed. States of)"="Micronesia (Fed. States of)",
"Polynesia"="Polynesia",
"French Polynesia"="French Polynesia",
"Samoa"="Samoa",
"Tonga"="Tonga",
"EUROPE"="EUROPE",
"Eastern Europe"="Eastern Europe",
"Belarus"="Belarus",
"Bulgaria"="Bulgaria",
"Czechia"="Czechia",
"Hungary"="Hungary",
"Poland"="Poland",
"Republic of Moldova"="Republic of Moldova",
"Romania"="Romania",
"Russian Federation"="Russian Federation",
"Slovakia"="Slovakia",
"Ukraine"="Ukraine",
"Northern Europe"="Northern Europe",
"Channel Islands"="Channel Islands",
"Denmark"="Denmark",
"Estonia"="Estonia",
"Finland"="Finland",
"Iceland"="Iceland",
"Ireland"="Ireland",
"Latvia"="Latvia",
"Lithuania"="Lithuania",
"Norway"="Norway",
"Sweden"="Sweden",
"United Kingdom"="United Kingdom",
"Southern Europe"="Southern Europe",
"Albania"="Albania",
"Bosnia and Herzegovina"="Bosnia and Herzegovina",
"Croatia"="Croatia",
"Greece"="Greece",
"Italy"="Italy",
"Malta"="Malta",
"Montenegro"="Montenegro",
"North Macedonia"="North Macedonia",
"Portugal"="Portugal",
"Serbia"="Serbia",
"Slovenia"="Slovenia",
"Spain"="Spain",
"Western Europe"="Western Europe",
"Austria"="Austria",
"Belgium"="Belgium",
"France"="France",
"Germany"="Germany",
"Luxembourg"="Luxembourg",
"Netherlands"="Netherlands",
"Switzerland"="Switzerland",
"Canada"="Canada",
"United States of America"="United States of America"
	),
	options = list(placeholder = "Type in an area to see graphs", multiple = TRUE, maxOptions = 5000, onInitialize = I('function() { this.setValue(""); }'))
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
                    "2015 to 2020"="2020",
                    "2014 to 2019"="2019",
                    "2013 to 2018"="2018",
                    "2012 to 2017"="2017",
                    "2011 to 2016"="2016",
                    "2010 to 2015"="2015"
                  ),
      ),
      
      hr(),
      
      selectInput("ImposeTFR", "Impose iTFR?",
                  c(
                   "Yes"="YES",
                    "No"="NO"
                  ),
      ),
      
      numericInput("ImposedTFR","If Yes, iTFR level",1.8,0,10,step=.1),
      numericInput("ImposedTFR_ar","If Yes, iTFR AR(1)",.90,.00,.99,step=.01),

      hr(),

      numericInput("SRB","Projected sex ratio at birth",round((1-.4886)/.4886,3),0,2,step=.005),
      
      hr(),
      
      numericInput("NetMigrationAdjustLevel","Net migration adjustment (annual, percent of population)",0,-25,25,step=.1),

      numericInput("GrossMigrationAdjustLevel","Gross migration adjustment (percent of net migration ratios)",100,0,200,step=10),

      selectInput("GrossMigrationProfile", "Gross migration age profile to use", selected="California",
                  c(
                    "California 2013 to 2017"="California"
                  ),
      ),
      
      hr(),
      
      selectInput("ImputeMort", "Impute mortality?",
                  c(
                    "Yes"="YES",
                    "No"="NO"
                  ),
      ),
      
      numericInput("BAStart","If yes, Brass' model alpha for First projection step...",.03,-2,2,step=.03),
      numericInput("BAEnd","...and Brass' model alpha for Last projection step",.12,-2,2,step=.03),

      selectInput("LifeTable", "Life table standard to use", selected="Albania",
                  c(
		    "Albania 2011"="Albania",
		    "Venezuela 2010"="Venezuela"
                  ),
      ),
      
      hr(),
      
      p("This interface was made with ",
        tags$a(href="https://shiny.rstudio.com/", 
               "Shiny for R."),
        
        tags$a(href="https://edyhsgr.github.io/eddieh/", 
               "Eddie Hunsinger,"), 
        
        "August 2022."),

      p("Information including ", 
	tags$a(href="https://github.com/edyhsgr/CCRStable/tree/master/Oct2020Presentation",
		"formulas, a spreadsheet demonstration, and slides for a related talk, "),
	"as well as ",
	tags$a(href="https://www.r-project.org/",
		"R"),
	"code with input files for several examples, including a ",
	tags$a(href="https://shiny.demog.berkeley.edu/eddieh/CCRUnc/",
		"stochastic version, "), 
	"an ",	
	tags$a(href="https://shiny.demog.berkeley.edu/eddieh/CCRStable_ValView_Florida/",
		"errors review version"), 
	"and a ",
	tags$a(href="https://shiny.demog.berkeley.edu/eddieh/CCRStable_StateSingle_Florida/",
		"single-year-of-age version, "), 
	"is all available in the ",
	tags$a(href="https://github.com/edyhsgr/CCRStable", 
		"related GitHub repository.")),
      
      p("Population estimates inputs from ",
        tags$a(href="https://population.un.org/wpp/", 
               "United Nations World Population Prospects 2022.")),
      
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
        
          "Life tables are from the ",
          tags$a(href="https://www.lifetable.de/", 
               "Human Life Table Database.")),
      
      p(tags$a(href="https://applieddemogtoolbox.github.io/Toolbox/#CCRStable", 
             "Applied Demography Toolbox listing.")),

    width=3
    ),
    
    mainPanel(
      
      plotOutput("plots")
    ))
  )

server<-function(input, output) {	
  output$plots<-renderPlot({
    par(mfrow=c(2,2))

    ##NUMBER FORMATTING
    options(scipen=999)

##USMD CA SURVIVAL DATA (GENERIC)
if(input$LifeTable=="Albania") {lt<-read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Mortality/lt_Albania2011_HLD.csv",header=TRUE,sep=",")
}
if(input$LifeTable=="Venezuela") {lt<-read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Mortality/lt_Venezuela2010_HLD.csv",header=TRUE,sep=",")
}
lxF<-lt$lx_Female/100000
lxM<-lt$lx_Male/100000
lxT<-lt$lx_Both/100000
lxF<-c(lxF[1],lxF[3:21])
lxM<-c(lxM[1],lxM[3:21])
lxT<-c(lxT[1],lxT[3:21])

##SELECT CENSUS ACS (via IPUMS) MIGRATION DATA
if(input$GrossMigrationProfile=="California") {
Migration<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CCRStable/master/InputData/Migration/AGenericMigrationProfile_CA_2013to2017ACS.csv",header=TRUE,sep=","))
Migration<-c(Migration$CA_F,Migration$CA_M)
}

    ##RUN ONLY IF AREA INPUTS ARE PROVIDED
    if(input$Area=="") {
      plot.new()
      legend("topleft",legend=c("Select an area with the panel to the left"),cex=2,bty="n")
    }
  
if(input$Area!="") {

##########
##READING EXTERNAL DATA IN
##########

base_url<-"https://population.un.org/dataportalapi/api/v1"
Area_select<-data.frame(read.csv(paste(c(base_url,"/locations?sort=id&format=csv"),collapse=""),sep="|",skip=1))

Select_url<-paste(c(base_url,
	"/data/indicators/",
	46,			#46 is PopByAge5AndSex
	"/locations/",
	Area_select$Id[Area_select$Name==input$Area],
	"/start/",2010,
	"/end/",2020,
	"/?format=csv"),collapse="")
Select<-read.csv(Select_url,sep="|",skip=1)

SelectVal_url<-paste(c(base_url,
	"/data/indicators/",
	46,			#46 is PopByAge5AndSex
	"/locations/",
	Area_select$Id[Area_select$Name==input$Area],
	"/start/",input$STEP,
	"/end/",input$STEP,
	"/?format=csv"),collapse="")
SelectVal<-read.csv(SelectVal_url,sep="|",skip=1)

    ##########
    ##SCRIPT INPUTS
    ##########
    
    ##DIMENSIONS
    SIZE<-36
    HALFSIZE<-SIZE/2
    STEPS<-(input$STEP-2015)/5
    STEPSSTABLE<-STEPS+1000
    CURRENTSTEP<-0
    CURRENTSTEPSTABLE<-0
    PROJECTIONYEAR<-STEPS*5+2015
    FERTWIDTH<-35
    
    ##SELECTING RATIOS BASIS
    FirstYear<-strtoi(input$RatiosFrom)-5
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
    
    ##SELECT AREA
    Name<-paste(input$Area)
    
    ##SELECTING FROM THE INPUT POPULATION TABLE (K) BASED ON INPUTS
    TMinusOneAgeInit_F<-subset(Select,Sex=="Female" & TimeLabel==2010 & AgeStart<90)
    TMinusOneAgeInit_F<-TMinusOneAgeInit_F$Value
TMinusOneAgeInit_F_Max<-subset(Select,Sex=="Female" & TimeLabel==2010 & AgeStart>80)
TMinusOneAgeInit_F[18]<-sum(TMinusOneAgeInit_F_Max$Value)
    TMinusOneAge_F<-TMinusOneAgeInit_F

    TMinusOneAgeInit_M<-subset(Select,Sex=="Male" & TimeLabel==2010 & AgeStart<90)
    TMinusOneAgeInit_M<-TMinusOneAgeInit_M$Value
TMinusOneAgeInit_M_Max<-subset(Select,Sex=="Male" & TimeLabel==2010 & AgeStart>80)
TMinusOneAgeInit_M[18]<-sum(TMinusOneAgeInit_M_Max$Value)
    TMinusOneAge_M<-TMinusOneAgeInit_M
    
    TMinusOneAge<-TMinusOneAgeInit<-c(TMinusOneAge_F,TMinusOneAge_M)
    
    TMinusOneAgeInitRatios_F<-subset(Select,Sex=="Female" & TimeLabel==FirstYear & AgeStart<90)
    TMinusOneAgeInitRatios_F<-TMinusOneAgeInitRatios_F$Value
TMinusOneAgeInitRatios_F_Max<-subset(Select,Sex=="Female" & TimeLabel==2010 & AgeStart>80)
TMinusOneAgeInitRatios_F[18]<-sum(TMinusOneAgeInitRatios_F_Max$Value)
    TMinusOneAgeRatios_F<-TMinusOneAgeInitRatios_F

    TMinusOneAgeInitRatios_M<-subset(Select,Sex=="Male" & TimeLabel==FirstYear & AgeStart<90)
    TMinusOneAgeInitRatios_M<-TMinusOneAgeInitRatios_M$Value
TMinusOneAgeInitRatios_M_Max<-subset(Select,Sex=="Male" & TimeLabel==2010 & AgeStart>80)
TMinusOneAgeInitRatios_M[18]<-sum(TMinusOneAgeInitRatios_M_Max$Value)
    TMinusOneAgeRatios_M<-TMinusOneAgeInitRatios_M
    
    TMinusOneAgeRatios<-TMinusOneAgeInitRatios<-c(TMinusOneAgeRatios_F,TMinusOneAgeRatios_M)
    
    TMinusZeroAgeInit_F<-subset(Select,Sex=="Female" & TimeLabel==2015 & AgeStart<90)
    TMinusZeroAgeInit_F<-TMinusZeroAgeInit_F$Value
TMinusZeroAgeInit_F_Max<-subset(Select,Sex=="Female" & TimeLabel==2015 & AgeStart>80)
TMinusZeroAgeInit_F[18]<-sum(TMinusZeroAgeInit_F_Max$Value)
    TMinusZeroAge_F<-TMinusZeroAgeInit_F

    TMinusZeroAgeInit_M<-subset(Select,Sex=="Male" & TimeLabel==2015 & AgeStart<90)
    TMinusZeroAgeInit_M<-TMinusZeroAgeInit_M$Value
TMinusZeroAgeInit_M_Max<-subset(Select,Sex=="Male" & TimeLabel==2015 & AgeStart>80)
TMinusZeroAgeInit_M[18]<-sum(TMinusZeroAgeInit_M_Max$Value)
    TMinusZeroAge_M<-TMinusZeroAgeInit_M
    
    TMinusZeroAge<-TMinusZeroAgeInit<-c(TMinusZeroAge_F,TMinusZeroAge_M)
    
    TMinusZeroAgeInitRatios_F<-subset(Select,Sex=="Female" & TimeLabel==SecondYear & AgeStart<90)
    TMinusZeroAgeInitRatios_F<-TMinusZeroAgeInitRatios_F$Value
TMinusZeroAgeInitRatios_F_Max<-subset(Select,Sex=="Female" & TimeLabel==2015 & AgeStart>80)
TMinusZeroAgeInitRatios_F[18]<-sum(TMinusZeroAgeInitRatios_F_Max$Value)
    TMinusZeroAgeRatios_F<-TMinusZeroAgeInitRatios_F

    TMinusZeroAgeInitRatios_M<-subset(Select,Sex=="Male" & TimeLabel==SecondYear & AgeStart<90)
    TMinusZeroAgeInitRatios_M<-TMinusZeroAgeInitRatios_M$Value
TMinusZeroAgeInitRatios_M_Max<-subset(Select,Sex=="Male" & TimeLabel==2015 & AgeStart>80)
TMinusZeroAgeInitRatios_M[18]<-sum(TMinusZeroAgeInitRatios_M_Max$Value)
    TMinusZeroAgeRatios_M<-TMinusZeroAgeInitRatios_M
    
    TMinusZeroAgeRatios<-TMinusZeroAgeInitRatios<-c(TMinusZeroAgeRatios_F,TMinusZeroAgeRatios_M)

    TValAge_F<-subset(SelectVal,Sex=="Female" & TimeLabel==input$STEP & AgeStart<90 & Variant=="Median")
    TValAge_F<-TValAge_F$Value  
TValAge_F_Max<-subset(SelectVal,Sex=="Female" & TimeLabel==input$STEP & AgeStart>80 & Variant=="Median")
TValAge_F[18]<-sum(TValAge_F_Max$Value)
    TValAge_M<-subset(SelectVal,Sex=="Male" & TimeLabel==input$STEP & AgeStart<90 & Variant=="Median")
    TValAge_M<-TValAge_M$Value  
TValAge_M_Max<-subset(SelectVal,Sex=="Male" & TimeLabel==input$STEP & AgeStart>80 & Variant=="Median")
TValAge_M[18]<-sum(TValAge_M_Max$Value)
    TValAge<-c(TValAge_F,TValAge_M)

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
    B_F[1,4:10]<-Ratios[1]*ffab
    A_F<-B_F+S_F

    ##PLACING COHORT CHANGE RATIOS (MALE)
    S_M<-array(0,c(HALFSIZE,HALFSIZE))
    S_M<-rbind(0,cbind(diag(Ratios[(HALFSIZE+2):SIZE]),0))
    
    ##OPEN-ENDED AGE GROUP (MALE)
    S_M[HALFSIZE,HALFSIZE-1]<-TMinusZeroAgeRatios[SIZE]/(TMinusOneAgeRatios[SIZE-1]+TMinusOneAgeRatios[SIZE])
    Ratios[SIZE]<-S_M[HALFSIZE,HALFSIZE]<-S_M[HALFSIZE,HALFSIZE-1]
    
    ##BIRTHS AND MATRIX PORTION CONSTRUCTION (MALE)
    B_M<-0*S_M
    B_M[1,4:10]<-Ratios[1]*(1-ffab)
    
    ##STRUCTURAL ZEROES
    AEnd_Zero<-A_Zero<-array(0,c(HALFSIZE,HALFSIZE))
    
    ##MAKING FULL PROJECTION MATRIX (TWO-SEX)
    Acolone<-cbind(A_F,A_Zero)
    Acoltwo<-cbind(B_M,S_M)
    A<-rbind(Acolone,Acoltwo)
    
    ##IMPLIED TFR CALCUATION
    ImpliedTFR2010<-((TMinusOneAgeInit[1]+TMinusOneAgeInit[HALFSIZE+1])/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH
    ImpliedTFR2015<-((TMinusZeroAgeInit[1]+TMinusZeroAgeInit[HALFSIZE+1])/5)/sum(TMinusZeroAgeInit[4:10])*FERTWIDTH
          
      ##########
      ##PROJECTION FUNCTION
      ##########
  
      ##MAX STEPS IN CASE USER (ESP ME) GETS CARRIED AWAY
      if(STEPS<198){     
  
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
	e0FStart<-sum(LxFStart[1:(length(LxFStart)-1)]*5)
	e0MStart<-sum(LxMStart[1:(length(LxFStart)-1)]*5)

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
	e0FAdj<-sum(LxFAdj[1:(length(LxFStart)-1)]*5)
	e0MAdj<-sum(LxMAdj[1:(length(LxFStart)-1)]*5)

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
				{TMinusZeroAge<-NetMigrationAdjustLevel*5*sum(TMinusOneAgeNew)*Migration+TMinusZeroAge}
		if(UseImposedTFR=="YES") 
				{TMinusZeroAge[1]<-(ImpliedTFR*input$ImposedTFR_ar+ImposedTFR*(1-input$ImposedTFR_ar))*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*ffab
				TMinusZeroAge[HALFSIZE+1]<-(ImpliedTFR*input$ImposedTFR_ar+ImposedTFR*(1-input$ImposedTFR_ar))*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*(1-ffab)}
		if(UseImposedTFR=="NO") 
				{TMinusZeroAge[1]<-ImpliedTFR*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*ffab
				TMinusZeroAge[HALFSIZE+1]<-ImpliedTFR*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*(1-ffab)}
				}
        TMinusZeroAge_NDF<-TMinusZeroAge
	TMinusZeroAge<-data.frame(TMinusZeroAge)

	##CALCULATE iTFR
	ImpliedTFRNew<-((TMinusZeroAge_NDF[1]+TMinusZeroAge_NDF[HALFSIZE+1])/5)/sum(TMinusZeroAge_NDF[4:10])*FERTWIDTH

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
    
    ##########
    ##GRAPHING DATA (SOME ~HACKY LABELING SO MAY [LIKELY] NOT RENDER WELL)
    ##########
    
    ##FIRST GRAPH - MAJOR SUMMARY
    agegroups<-c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    if(SelectBySex=="Total") {plot(TMinusOneAgeInit[,1]/sum(TMinusOneAgeInit[,1]),type="l",col="orange",main=paste(text=c(input$Area,", ",input$Sex),collapse=""),ylim=c(0,.12),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
    if(SelectBySex=="Female") {plot(TMinusOneAgeInit[,2]/sum(TMinusOneAgeInit[,2]),type="l",col="orange",main=paste(text=c(input$Area,", ",input$Sex),collapse=""),ylim=c(0,.12),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
    if(SelectBySex=="Male") {plot(TMinusOneAgeInit[,3]/sum(TMinusOneAgeInit[,3]),type="l",col="orange",main=paste(text=c(input$Area,", ",input$Sex),collapse=""),ylim=c(0,.12),axes=FALSE,xlab="",ylab="Population (proportional)",lwd=4)}
    
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
    
    ##SECOND GRAPH - COHORT CHANGE RATIOS WITH AND WITHOUT ADJUSTMENTS
    agegroups2<-c("5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    plot(Ratios[2:18],type="l",col="dodger blue",main=paste(text=c("Effective Cohort Change Ratios, ",PROJECTIONYEAR-5," to ",PROJECTIONYEAR),collapse=""),ylim=c(.25,1.75),axes=FALSE,xlab="",ylab="Ratio",lwd=4)
    ##OPEN-ENDED AGE GROUP OPTION
    mtext(side=1,c("(Note: 85+ ratios are applied to full 80+ age groups)"),line=-42,adj=.50,col="black")
    abline(a=NULL, b=NULL, h=1, v=NULL)
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
    
    ##THIRD GRAPH - PYRAMID (FEMALE PORTION)
    barplot(NewAge_F,horiz=T,names=agegroups,space=0,xlim=c(max(NewAge_M)*2,0),col="dodger blue",las=1,main=paste(text=c("Female, ",PROJECTIONYEAR),collapse=""))
    barplot(TValAge[1:18],horiz=T,names=FALSE,col=1,space=0,density=5,angle=45,add=TRUE)    

    ##FOURTH GRAPH - PYRAMID (MALE PORTION)
    barplot(NewAge_M,horiz=T,names=FALSE,space=0,xlim=c(0,max(NewAge_M)*2),col="gold",main=paste(text=c("Male, ",PROJECTIONYEAR),collapse=""))
    barplot(TValAge[19:36],horiz=T,names=FALSE,col=1,space=0,density=5,angle=45,add=TRUE)    

legend("topright",inset=.165,legend="UN WPP 2022 Projection", col=1, angle=45, density=5, cex=1.85, bty="n")

}
    
  },height=1200,width=1200)
  
}

shinyApp(ui = ui, server = server) 

