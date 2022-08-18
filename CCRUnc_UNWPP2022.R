##########
##HAMILTON-PERRY WITH STOCHASTIC COMPONENTS POPULATION PROJECTION CODE - APPLIED TO UN WPP 2022 DATA
##
##EDDIE HUNSINGER, AUGUST 2022 (based on work from November 2020: https://shiny.demog.berkeley.edu/eddieh/CCRUnc/)
##https://edyhsgr.github.io/
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
  
  tags$h3("Cohort Change Ratio-Based Stochastic Population Projection Review Shiny App - Applied to UN WPP 2022 Data"),
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
                       "Afghanistan"="Afghanistan",
                       "Albania"="Albania",
                       "Algeria"="Algeria",
                       "American Samoa"="American Samoa",
                       "Andorra"="Andorra",
                       "Angola"="Angola",
                       "Antigua and Barbuda"="Antigua and Barbuda",
                       "Azerbaijan"="Azerbaijan",
                       "Argentina"="Argentina",
                       "Australia"="Australia",
                       "Austria"="Austria",
                       "Bahamas"="Bahamas",
                       "Bahrain"="Bahrain",
                       "Bangladesh"="Bangladesh",
                       "Armenia"="Armenia",
                       "Barbados"="Barbados",
                       "Belgium"="Belgium",
                       "Bermuda"="Bermuda",
                       "Bhutan"="Bhutan",
                       "Bolivia (Plurinational State of)"="Bolivia (Plurinational State of)",
                       "Bosnia and Herzegovina"="Bosnia and Herzegovina",
                       "Botswana"="Botswana",
                       "Brazil"="Brazil",
                       "Belize"="Belize",
                       "Solomon Islands"="Solomon Islands",
                       "British Virgin Islands"="British Virgin Islands",
                       "Brunei Darussalam"="Brunei Darussalam",
                       "Bulgaria"="Bulgaria",
                       "Myanmar"="Myanmar",
                       "Burundi"="Burundi",
                       "Belarus"="Belarus",
                       "Cambodia"="Cambodia",
                       "Cameroon"="Cameroon",
                       "Canada"="Canada",
                       "Cabo Verde"="Cabo Verde",
                       "Cayman Islands"="Cayman Islands",
                       "Central African Republic"="Central African Republic",
                       "Sri Lanka"="Sri Lanka",
                       "Chad"="Chad",
                       "Chile"="Chile",
                       "China"="China",
                       "China, Taiwan Province of China"="China, Taiwan Province of China",
                       "Colombia"="Colombia",
                       "Comoros"="Comoros",
                       "Mayotte"="Mayotte",
                       "Congo"="Congo",
                       "Dem. Rep. of the Congo"="Dem. Rep. of the Congo",
                       "Cook Islands"="Cook Islands",
                       "Costa Rica"="Costa Rica",
                       "Croatia"="Croatia",
                       "Cuba"="Cuba",
                       "Cyprus"="Cyprus",
                       "Czechia"="Czechia",
                       "Benin"="Benin",
                       "Denmark"="Denmark",
                       "Dominica"="Dominica",
                       "Dominican Republic"="Dominican Republic",
                       "Ecuador"="Ecuador",
                       "El Salvador"="El Salvador",
                       "Equatorial Guinea"="Equatorial Guinea",
                       "Ethiopia"="Ethiopia",
                       "Eritrea"="Eritrea",
                       "Estonia"="Estonia",
                       "Faroe Islands"="Faroe Islands",
                       "Falkland Islands (Malvinas)"="Falkland Islands (Malvinas)",
                       "Fiji"="Fiji",
                       "Finland"="Finland",
                       "France"="France",
                       "French Guiana"="French Guiana",
                       "French Polynesia"="French Polynesia",
                       "Djibouti"="Djibouti",
                       "Gabon"="Gabon",
                       "Georgia"="Georgia",
                       "Gambia"="Gambia",
                       "State of Palestine"="State of Palestine",
                       "Germany"="Germany",
                       "Ghana"="Ghana",
                       "Gibraltar"="Gibraltar",
                       "Kiribati"="Kiribati",
                       "Greece"="Greece",
                       "Greenland"="Greenland",
                       "Grenada"="Grenada",
                       "Guadeloupe"="Guadeloupe",
                       "Guam"="Guam",
                       "Guatemala"="Guatemala",
                       "Guinea"="Guinea",
                       "Guyana"="Guyana",
                       "Haiti"="Haiti",
                       "Honduras"="Honduras",
                       "China, Hong Kong SAR"="China, Hong Kong SAR",
                       "Hungary"="Hungary",
                       "Iceland"="Iceland",
                       "India"="India",
                       "Indonesia"="Indonesia",
                       "Iran (Islamic Republic of)"="Iran (Islamic Republic of)",
                       "Iraq"="Iraq",
                       "Ireland"="Ireland",
                       "Israel"="Israel",
                       "Italy"="Italy",
                       "CÃ´te d'Ivoire"="CÃ´te d'Ivoire",
                       "Jamaica"="Jamaica",
                       "Japan"="Japan",
                       "Kazakhstan"="Kazakhstan",
                       "Jordan"="Jordan",
                       "Kenya"="Kenya",
                       "Dem. People's Rep. of Korea"="Dem. People's Rep. of Korea",
                       "Republic of Korea"="Republic of Korea",
                       "Kuwait"="Kuwait",
                       "Kyrgyzstan"="Kyrgyzstan",
                       "Lao People's Dem. Republic"="Lao People's Dem. Republic",
                       "Lebanon"="Lebanon",
                       "Lesotho"="Lesotho",
                       "Latvia"="Latvia",
                       "Liberia"="Liberia",
                       "Libya"="Libya",
                       "Liechtenstein"="Liechtenstein",
                       "Lithuania"="Lithuania",
                       "Luxembourg"="Luxembourg",
                       "China, Macao SAR"="China, Macao SAR",
                       "Madagascar"="Madagascar",
                       "Malawi"="Malawi",
                       "Malaysia"="Malaysia",
                       "Maldives"="Maldives",
                       "Mali"="Mali",
                       "Malta"="Malta",
                       "Martinique"="Martinique",
                       "Mauritania"="Mauritania",
                       "Mauritius"="Mauritius",
                       "Mexico"="Mexico",
                       "Monaco"="Monaco",
                       "Mongolia"="Mongolia",
                       "Republic of Moldova"="Republic of Moldova",
                       "Montenegro"="Montenegro",
                       "Montserrat"="Montserrat",
                       "Morocco"="Morocco",
                       "Mozambique"="Mozambique",
                       "Oman"="Oman",
                       "Namibia"="Namibia",
                       "Nauru"="Nauru",
                       "Nepal"="Nepal",
                       "Netherlands"="Netherlands",
                       "CuraÃ§ao"="CuraÃ§ao",
                       "Aruba"="Aruba",
                       "Sint Maarten (Dutch part)"="Sint Maarten (Dutch part)",
                       "Bonaire, Sint Eustatius and Saba"="Bonaire, Sint Eustatius and Saba",
                       "New Caledonia"="New Caledonia",
                       "Vanuatu"="Vanuatu",
                       "New Zealand"="New Zealand",
                       "Nicaragua"="Nicaragua",
                       "Niger"="Niger",
                       "Nigeria"="Nigeria",
                       "Niue"="Niue",
                       "Norway"="Norway",
                       "Northern Mariana Islands"="Northern Mariana Islands",
                       "Micronesia (Fed. States of)"="Micronesia (Fed. States of)",
                       "Marshall Islands"="Marshall Islands",
                       "Palau"="Palau",
                       "Pakistan"="Pakistan",
                       "Panama"="Panama",
                       "Papua New Guinea"="Papua New Guinea",
                       "Paraguay"="Paraguay",
                       "Peru"="Peru",
                       "Philippines"="Philippines",
                       "Poland"="Poland",
                       "Portugal"="Portugal",
                       "Guinea-Bissau"="Guinea-Bissau",
                       "Timor-Leste"="Timor-Leste",
                       "Puerto Rico"="Puerto Rico",
                       "Qatar"="Qatar",
                       "RÃ©union"="RÃ©union",
                       "Romania"="Romania",
                       "Russian Federation"="Russian Federation",
                       "Rwanda"="Rwanda",
                       "Saint Helena"="Saint Helena",
                       "Saint Kitts and Nevis"="Saint Kitts and Nevis",
                       "Anguilla"="Anguilla",
                       "Saint Lucia"="Saint Lucia",
                       "Saint Pierre and Miquelon"="Saint Pierre and Miquelon",
                       "Saint Vincent and the Grenadines"="Saint Vincent and the Grenadines",
                       "San Marino"="San Marino",
                       "Sao Tome and Principe"="Sao Tome and Principe",
                       "Saudi Arabia"="Saudi Arabia",
                       "Senegal"="Senegal",
                       "Serbia"="Serbia",
                       "Seychelles"="Seychelles",
                       "Sierra Leone"="Sierra Leone",
                       "Singapore"="Singapore",
                       "Slovakia"="Slovakia",
                       "Viet Nam"="Viet Nam",
                       "Slovenia"="Slovenia",
                       "Somalia"="Somalia",
                       "South Africa"="South Africa",
                       "Zimbabwe"="Zimbabwe",
                       "Spain"="Spain",
                       "South Sudan"="South Sudan",
                       "Sudan"="Sudan",
                       "Western Sahara"="Western Sahara",
                       "Suriname"="Suriname",
                       "Eswatini"="Eswatini",
                       "Sweden"="Sweden",
                       "Switzerland"="Switzerland",
                       "Syrian Arab Republic"="Syrian Arab Republic",
                       "Tajikistan"="Tajikistan",
                       "Thailand"="Thailand",
                       "Togo"="Togo",
                       "Tokelau"="Tokelau",
                       "Tonga"="Tonga",
                       "Trinidad and Tobago"="Trinidad and Tobago",
                       "United Arab Emirates"="United Arab Emirates",
                       "Tunisia"="Tunisia",
                       "TÃ¼rkiye"="TÃ¼rkiye",
                       "Turkmenistan"="Turkmenistan",
                       "Turks and Caicos Islands"="Turks and Caicos Islands",
                       "Tuvalu"="Tuvalu",
                       "Uganda"="Uganda",
                       "Ukraine"="Ukraine",
                       "North Macedonia"="North Macedonia",
                       "Egypt"="Egypt",
                       "United Kingdom"="United Kingdom",
                       "Isle of Man"="Isle of Man",
                       "United Republic of Tanzania"="United Republic of Tanzania",
                       "United States of America"="United States of America",
                       "United States Virgin Islands"="United States Virgin Islands",
                       "Burkina Faso"="Burkina Faso",
                       "Uruguay"="Uruguay",
                       "Uzbekistan"="Uzbekistan",
                       "Venezuela (Bolivarian Republic of)"="Venezuela (Bolivarian Republic of)",
                       "Wallis and Futuna Islands"="Wallis and Futuna Islands",
                       "Samoa"="Samoa",
                       "Yemen"="Yemen",
                       "Zambia"="Zambia",
                       "Developed regions"="Developed regions",
                       "Developing regions"="Developing regions",
                       "Africa"="Africa",
                       "Latin America and the Caribbean"="Latin America and the Caribbean",
                       "Northern America"="Northern America",
                       "Eastern Asia"="Eastern Asia",
                       "Europe"="Europe",
                       "Oceania"="Oceania",
                       "Eastern Africa"="Eastern Africa",
                       "Middle Africa"="Middle Africa",
                       "Northern Africa"="Northern Africa",
                       "Southern Africa"="Southern Africa",
                       "Western Africa"="Western Africa",
                       "Caribbean"="Caribbean",
                       "Central America"="Central America",
                       "South-Eastern Asia"="South-Eastern Asia",
                       "Western Asia"="Western Asia",
                       "Eastern Europe"="Eastern Europe",
                       "Northern Europe"="Northern Europe",
                       "Southern Europe"="Southern Europe",
                       "Western Europe"="Western Europe",
                       "Australia/New Zealand"="Australia/New Zealand",
                       "Melanesia"="Melanesia",
                       "South America"="South America",
                       "Other developing regions"="Other developing regions",
                       "Asia"="Asia",
                       "Least developed countries"="Least developed countries",
                       "Sub-Saharan Africa"="Sub-Saharan Africa",
                       "Micronesia"="Micronesia",
                       "Polynesia"="Polynesia",
                       "Low-income countries"="Low-income countries",
                       "Lower-middle-income countries"="Lower-middle-income countries",
                       "Upper-middle-income countries"="Upper-middle-income countries",
                       "High-income countries"="High-income countries",
                       "Middle-income countries"="Middle-income countries",
                       "No income group available"="No income group available",
                       "Europe and Northern America"="Europe and Northern America",
                       "Central and Southern Asia"="Central and Southern Asia",
                       "Eastern and South-Eastern Asia"="Eastern and South-Eastern Asia",
                       "Northern Africa and Western Asia"="Northern Africa and Western Asia",
                       "Oceania (excluding Australia and New Zealand)"="Oceania (excluding Australia and New Zealand)",
                       "Central Asia"="Central Asia",
                       "Southern Asia"="Southern Asia"
                     ),
                     options = list(placeholder = "Type in an area to see graphs", multiple = TRUE, maxOptions = 5000, onInitialize = I('function() { this.setValue(""); }'))
      ),
      
      numericInput("STEP","Project to (year)",2030,2020,2100,step=5),
      
      selectInput("RatiosFrom", "Using ratios from",
                  c("2015 to 2020"="2015",
                    "2014 to 2019"="2014",
                    "2013 to 2018"="2013",
                    "2012 to 2017"="2012",
                    "2011 to 2016"="2011",
                    "2010 to 2015"="2010",
                    "Sample from listed periods"="Combined"
                  ),
      ),
      
      numericInput("ITER","Number of projection iterations (sample size)",100,100,1000,step=100),
      
      selectInput("ShowUN", "Show UN WPP Median Projection?",
                  c(
                    "No"="NO",
                    "Yes"="YES"
                  ),
      ),
      
      hr(),
      
      selectInput("ImposeTFR", "Impose iTFR?",
                  c(
                    "Yes"="YES",
                    "No"="NO"
                  ),
      ),
      
      sliderInput("ImposedTFR_ar","If Yes, iTFR AR(1) term (range inputs give uniform range option, for uncertain autocorrelation, etc.)",min=0,max=1,value=c(.75,1),step=0.05),
      sliderInput("ImposedTFR","...and iTFR level term",min=0,max=5,value=c(1.2,2.1),step=0.1),
      sliderInput("ImposedTFR_se","...and iTFR standard error term",min=0,max=.5,value=c(.05,.25),step=0.05),
      
      hr(),
      
      selectInput("AdjustMigr", "Adjust net migration? (Annual, percent of total population)",
                  c(
                    "Yes"="YES",
                    "No"="NO"
                  ),
      ),
      
      sliderInput("NetMigrationAdjustLevel_ar","If yes, net migration adjustment AR(1) term (range inputs give uniform range option, for uncertain autocorrelation, etc.)",min=-1,max=1,value=c(0,1),step=0.05),
      sliderInput("NetMigrationAdjustLevel","...and net migration adjustment level term",min=-2,max=2,value=c(-.5,.5),step=0.1),
      sliderInput("NetMigrationAdjustLevel_se","...and net migration adjustment standard error term",min=0,max=.5,value=c(.00,.25),step=0.05),
      
      selectInput("GrossMigrationProfile", "Gross migration age profile to use", selected="California",
                  c(
                    "California 2013 to 2017"="California"
                  ),
      ),
      
      hr(),
      
      sliderInput("BAStart","Brass' mortality model alpha for First projection step (range inputs give uniform range option, for uncertain drift, etc.)",min=-1,max=1,value=c(-1,.25),step=0.01),
      sliderInput("BAEnd","...and Brass' model alpha drift term (increase per step)",min=-.5,max=.5,value=c(-.01,.1),step=0.01),
      sliderInput("BA_se","...and Brass' model alpha standard error term",min=0,max=.25,value=c(.00,.05),step=0.01),
      
      selectInput("LifeTable", "Life table standard to use", selected="California",
                  c(
                    "California 2010 to 2014"="California"
                  ),
      ),
      
      hr(),
      
      p("This interface was made with ",
        tags$a(href="https://shiny.rstudio.com/", 
               "Shiny for R,"),
        
        "based on",
        
        tags$a(href="https://shiny.demog.berkeley.edu/eddieh/CCRUnc/", 
               "work from November 2020."), 
        
        tags$a(href="https://edyhsgr.github.io/", 
               "Eddie Hunsinger,"), 
        
        "August 2022."),
      
      p("Information including ", 
        tags$a(href="https://github.com/edyhsgr/CCRStable/tree/master/Oct2020Presentation",
               "formulas, a spreadsheet demonstration, and slides for a related talk, "),
        "as well as ",
        tags$a(href="https://www.r-project.org/",
               "R"),
        "code with input files for several examples, including the ",
        tags$a(href="https://shiny.demog.berkeley.edu/eddieh/CCRStable/",
               "main stable population review version "), 
        "that it's based on, an ",	
        tags$a(href="https://edyhsgr.shinyapps.io/CCRStable_ValView_Florida/",
               "errors review version"), 
        "and a ",
        tags$a(href="https://shiny.demog.berkeley.edu/eddieh/CCRStable_StateSingle_Florida/",
               "single-year-of-age version, "), 
        "is all available in the ",
        tags$a(href="https://github.com/edyhsgr/CCRStable", 
               "related GitHub repository.")),
      
      p("Population estimates inputs from ",
        tags$a(href="https://population.un.org/wpp/", 
               "United Nations World Population Prospects 2022."),
        
        "Information on accessing United Nations World Population Prospects 2022 data through R statistical software: ",
        tags$a(href="https://bonecave.schmert.net/un-api-1-year-pyramids-Argentina.html", 
               "Schmertmann (2022).")),
      
      p("More information on cohort change ratios: ",
        tags$a(href="https://www.worldcat.org/title/cohort-change-ratios-and-their-applications/oclc/988385033", 
               "Baker, Swanson, Tayman, and Tedrow (2017)."),
        
        p("Supporting work and thinking on stochastic population projection: ",
          tags$a(href="https://applieddemogtoolbox.github.io/#StochasticForecast", 
                 "Hunsinger (2011).")),
        
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
        
        p(tags$a(href="https://twitter.com/ApplDemogToolbx/status/1079286699941752832", 
                 "Graph of e0 and Brass' relational life table alpha by US state."),
          
          "Model life table (0.0 alpha) is the 5x5 2010 to 2014 life table for California from the ",
          tags$a(href="https://usa.mortality.org/index.php", 
                 "United States Mortality Database."))),
      
      p(tags$a(href="https://applieddemogtoolbox.github.io/#CCRStable", 
               "Applied Demography Toolbox listing.")),
      
      width=3
    ),
    
    mainPanel(
      
      plotOutput("plots")
    ))
)

server<-function(input, output) {	
  output$plots<-renderPlot({
    par(mfrow=c(3,2))
    
    ##NUMBER FORMATTING
    options(scipen=999)
    
    ##########
    ##READING EXTERNAL DATA IN
    ##########
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
    
    ##RUN ONLY IF AREA INPUTS ARE PROVIDED
    if(input$Area=="") {										# | input$STEP>2100) {
      plot.new()
      legend("topleft",legend=c("Select an area with the panel to the left"),cex=2,bty="n")		#, "'Project to (year)' maximum for this version is 2100"),cex=1.85,bty="n")
    }
    
    if(input$Area!="" & input$STEP<=2200) {
      
      base_url<-"https://population.un.org/dataportalapi/api/v1"
      Area_select<-data.frame(read.csv(paste(c(base_url,"/locations?sort=id&format=csv"),collapse=""),sep="|",skip=1))
      Select_url<-paste(c(base_url,		#Thanks for guidance from https://bonecave.schmert.net/un-api-1-year-pyramids-Argentina.html !
                          "/data/indicators/",
                          46,				#46 is PopByAge5AndSex
                          "/locations/",
                          Area_select$Id[Area_select$Name==input$Area],
                          "/start/",2010,
                          "/end/",2020,
                          "/?format=csv"),collapse="")
      Select<-read.csv(Select_url,sep="|",skip=1)
      
      if(input$STEP<=2100) {
        SelectVal_url<-paste(c(base_url,
                               "/data/indicators/",
                               46,			#46 is PopByAge5AndSex
                               "/locations/",
                               Area_select$Id[Area_select$Name==input$Area],
                               "/start/",input$STEP,
                               "/end/",input$STEP,
                               "/?format=csv"),collapse="")
        SelectVal<-read.csv(SelectVal_url,sep="|",skip=1)
      }
      
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
      FirstYear<-rep(strtoi(input$RatiosFrom),ITER)
      SecondYear<-FirstYear+5
      if(input$RatiosFrom=="Combined") {
        FirstYear<-sample(c(2010:2015),ITER,replace=TRUE)
        SecondYear<-FirstYear+5
      }
      
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
      
      ##SELECT BY SEX
      SelectBySex<-"Total"
      
      ##SELECT AREA
      Name<-paste(input$Area)
      
      ##NUMBER FORMATTING
      options(scipen=999)
      
      ##SELECTING FROM THE INPUT POPULATION TABLE (Select) BASED ON INPUTS
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
      
      TMinusOneAge<-TMinusOneAgeInit<-array(c(TMinusOneAge_F,TMinusOneAge_M),c(SIZE,1,ITER))
      
      TMinusOneAgeRatios_F<-TMinusOneAgeInitRatios_F<-TMinusOneAgeInitRatios_F_Max<-list(Select,ITER)
      for (i in 1:ITER) {
        TMinusOneAgeInitRatios_F[[i]]<-subset(Select,Sex=="Female" & TimeLabel==FirstYear[i] & AgeStart<90)
        TMinusOneAgeInitRatios_F[[i]]<-TMinusOneAgeInitRatios_F[[i]]$Value
        TMinusOneAgeInitRatios_F_Max[[i]]<-subset(Select,Sex=="Female" & TimeLabel==FirstYear[i] & AgeStart>80)
        TMinusOneAgeInitRatios_F[[i]][18]<-sum(TMinusOneAgeInitRatios_F_Max[[i]]$Value)
        TMinusOneAgeRatios_F<-TMinusOneAgeInitRatios_F
      }
      
      TMinusOneAgeRatios_M<-TMinusOneAgeInitRatios_M<-TMinusOneAgeInitRatios_M_Max<-list(Select,ITER)
      for (i in 1:ITER) {
        TMinusOneAgeInitRatios_M[[i]]<-subset(Select,Sex=="Male" & TimeLabel==FirstYear[i] & AgeStart<90)
        TMinusOneAgeInitRatios_M[[i]]<-TMinusOneAgeInitRatios_M[[i]]$Value
        TMinusOneAgeInitRatios_M_Max[[i]]<-subset(Select,Sex=="Male" & TimeLabel==FirstYear[i] & AgeStart>80)
        TMinusOneAgeInitRatios_M[[i]][18]<-sum(TMinusOneAgeInitRatios_M_Max[[i]]$Value)
        TMinusOneAgeRatios_M<-TMinusOneAgeInitRatios_M
      }
      
      TMinusOneAgeRatios<-TMinusOneAgeInitRatios<-array(,c(SIZE,1,ITER))
      for (i in 1:ITER) {
        TMinusOneAgeRatios[,,i]<-TMinusOneAgeInitRatios[,,i]<-c(unlist(TMinusOneAgeRatios_F[[i]]),unlist(TMinusOneAgeRatios_M[[i]]))
      }
      
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
      
      TMinusZeroAge<-TMinusZeroAgeInit<-array(c(TMinusZeroAge_F,TMinusZeroAge_M),c(SIZE,1,ITER))
      
      TMinusZeroAgeRatios_F<-TMinusZeroAgeInitRatios_F<-TMinusZeroAgeInitRatios_F_Max<-list(Select,ITER)
      for (i in 1:ITER) {
        TMinusZeroAgeInitRatios_F[[i]]<-subset(Select,Sex=="Female" & TimeLabel==SecondYear[i] & AgeStart<90)
        TMinusZeroAgeInitRatios_F[[i]]<-TMinusZeroAgeInitRatios_F[[i]]$Value
        TMinusZeroAgeInitRatios_F_Max[[i]]<-subset(Select,Sex=="Female" & TimeLabel==SecondYear[i] & AgeStart>80)
        TMinusZeroAgeInitRatios_F[[i]][18]<-sum(TMinusZeroAgeInitRatios_F_Max[[i]]$Value)
        TMinusZeroAgeRatios_F<-TMinusZeroAgeInitRatios_F
      }
      
      TMinusZeroAgeRatios_M<-TMinusZeroAgeInitRatios_M<-TMinusZeroAgeInitRatios_M_Max<-list(Select,ITER)
      for (i in 1:ITER) {
        TMinusZeroAgeInitRatios_M[[i]]<-subset(Select,Sex=="Male" & TimeLabel==SecondYear[i] & AgeStart<90)
        TMinusZeroAgeInitRatios_M[[i]]<-TMinusZeroAgeInitRatios_M[[i]]$Value
        TMinusZeroAgeInitRatios_M_Max[[i]]<-subset(Select,Sex=="Male" & TimeLabel==SecondYear[i] & AgeStart>80)
        TMinusZeroAgeInitRatios_M[[i]][18]<-sum(TMinusZeroAgeInitRatios_M_Max[[i]]$Value)
        TMinusZeroAgeRatios_M<-TMinusZeroAgeInitRatios_M
      }
      
      TMinusZeroAgeRatios<-TMinusZeroAgeInitRatios<-array(,c(SIZE,1,ITER))
      for (i in 1:ITER) {
        TMinusZeroAgeRatios[,,i]<-TMinusZeroAgeInitRatios[,,i]<-c(unlist(TMinusZeroAgeRatios_F[[i]]),unlist(TMinusZeroAgeRatios_M[[i]]))
      }
      
      ##SELECTING THE UNWPP POPULATION PROJECTION TABLE (SelectVal) BASED ON INPUTS
      if(input$STEP<=2100) {
        TValAge_F<-subset(SelectVal,Sex=="Female" & TimeLabel==input$STEP & AgeStart<90 & Variant=="Median")
        TValAge_F<-TValAge_F$Value  
        TValAge_F_Max<-subset(SelectVal,Sex=="Female" & TimeLabel==input$STEP & AgeStart>80 & Variant=="Median")
        TValAge_F[18]<-sum(TValAge_F_Max$Value)
        TValAge_M<-subset(SelectVal,Sex=="Male" & TimeLabel==input$STEP & AgeStart<90 & Variant=="Median")
        TValAge_M<-TValAge_M$Value  
        TValAge_M_Max<-subset(SelectVal,Sex=="Male" & TimeLabel==input$STEP & AgeStart>80 & Variant=="Median")
        TValAge_M[18]<-sum(TValAge_M_Max$Value)
        TValAge<-c(TValAge_F,TValAge_M)
      }
      
      ##########
      ##CALCULATIONS
      ##########
      
      ##COHORT CHANGE RATIOS
      Ratios<-array(0,c(SIZE,ITER))
      for (i in 2:SIZE) {
        for (j in 1:ITER) {
          Ratios[i,j]<-TMinusZeroAgeRatios[i,,j]/TMinusOneAgeRatios[i-1,,j]}}
      #for (i in 1:ITER) {Ratios[1,i]<-(TMinusZeroAgeRatios[1,,i]+TMinusZeroAgeRatios[HALFSIZE+1,,i])/sum(TMinusOneAgeRatios[4:10,,i])}
      
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
      
      if(STEPS<=37 & ITER<=2000){		##MAX STEPS AND ITER IN CASE USER (ESP ME) GETS CARRIED AWAY
        
        ##RUN THE PROJECTION WITH SURV ADJUSTMENT (BY SOURCE() OF PROJECTION FILE)
        repeat{
          SurvChange<-array(0,ITER)
          SurvChange_e<-array(0,ITER)
          ##ADDING TO BRASS ALPHA WITH EACH STEP
          for (i in 1:ITER) {SurvChange_e[i]<-rnorm(1,0,BA_se[i])}
          #if(ImputeMort=="YES") {for (i in 1:ITER) {SurvChange[i]<-BA_start[i]+BA_end[i]+SurvChange_e[i]}}
          for (i in 1:ITER) {SurvChange[i]<-BA_start[i]+BA_end[i]+SurvChange_e[i]}
          for (i in 1:ITER) {BA_start[i]<-SurvChange[i]}
          source("https://raw.githubusercontent.com/edyhsgr/CCRStable/master/CCR_Unc_CA_Supporting_Project.R",local=TRUE)
          
          ##MAKING TIME SERIES OBJECTS
          KProj<-array(,dim=ITER)
          for (i in 1:ITER) {KProj[i]<-sum(TMinusZeroAge[,,i])}
          
          assign(paste(text=c("K_",CURRENTSTEP),collapse=""),KProj[])
          assign(paste(text=c("ImpliedTFR_",CURRENTSTEP),collapse=""),ImpliedTFRNew[])
          assign(paste(text=c("NetMigrAdj_",CURRENTSTEP),collapse=""),NetMigrAdjust[])
          assign(paste(text=c("e0F_",CURRENTSTEP),collapse=""),e0FAdj[])
          assign(paste(text=c("e0M_",CURRENTSTEP),collapse=""),e0MAdj[])
          
          K_0<-sum(TMinusZeroAgeInit[,,1])
          ImpliedTFR_0<-ImpliedTFR2015
          NetMigrAdj_0<-0
          e0F_0<-e0FStart
          e0M_0<-e0MStart
          
          K_Project<-paste0('K_',0:CURRENTSTEP)
          ImpliedTFR_Project<-paste0('ImpliedTFR_',0:CURRENTSTEP)
          NetMigrAdj_Project<-paste0('NetMigrAdj_',0:CURRENTSTEP)
          e0F_Project<-paste0('e0F_',0:CURRENTSTEP)
          e0M_Project<-paste0('e0M_',0:CURRENTSTEP)
          
          K_Project<-do.call(cbind,mget(K_Project))
          ImpliedTFR_Project<-do.call(cbind,mget(ImpliedTFR_Project))
          NetMigrAdj_Project<-do.call(cbind,mget(NetMigrAdj_Project))
          e0F_Project<-do.call(cbind,mget(e0F_Project))
          e0M_Project<-do.call(cbind,mget(e0M_Project))
          
          CURRENTSTEP <- CURRENTSTEP+1
          
          if(CURRENTSTEP > STEPS) {break}}
      }
      
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
      
      ##FIRST GRAPH - PYRAMID (FEMALE PORTION)
      barplot2(NewAge_F,plot.ci=TRUE,ci.l=NewAge_F_Low,ci.u=NewAge_F_High,horiz=T,names=agegroups,cex.main=2,cex.names=1.2,cex.axis=1.5,space=0,xlim=c(max(NewAge_M)*2,0),col="dodger blue",las=1,main=paste(text=c("Female, ",PROJECTIONYEAR),collapse=""))
      if(input$STEP<=2100 & input$ShowUN=="YES") {barplot(TValAge[1:18],horiz=T,names=FALSE,axes=FALSE,col=1,space=0,density=5,angle=45,add=TRUE)}
      
      ##SECOND GRAPH - PYRAMID (MALE PORTION)
      barplot2(NewAge_M,plot.ci=TRUE,ci.l=NewAge_M_Low,ci.u=NewAge_M_High,horiz=T,names=FALSE,cex.main=2,cex.names=1.25,cex.axis=1.5,space=0,xlim=c(0,max(NewAge_M)*2),col="gold",main=paste(text=c("Male, ",PROJECTIONYEAR),collapse=""))
      if(input$STEP<=2100 & input$ShowUN=="YES") {barplot(TValAge[19:36],horiz=T,names=FALSE,axes=FALSE,col=1,space=0,density=5,angle=45,add=TRUE)    
        if(input$STEP<2025) {legend("topright",inset=.15,legend="UN WPP Estimate", col=1, angle=45, density=5, cex=2.25, bg="white")}
        if(input$STEP>2020) {legend("topright",inset=.15,legend="UN WPP Median Projection", col=1, angle=45, density=5, cex=2.25, bg="white")}
      }
      
      ##THIRD GRAPH - TOTAL POPULATION
      plot(K_Project[1,],type="l",ylim=c(min(K_Project)*.9,max(K_Project)*1.1),xlab="Year",ylab="",main="Total Population by Year",cex.lab=2,cex.main=2,axes=F)
      for (i in 1:ITER) {lines(K_Project[i,],col=sample(6))}
      axis(side=1,at=0:CURRENTSTEP,labels=paste(seq(2010,CURRENTSTEP*5+2010,5)),cex.axis=1.5)
      axis(side=2,cex.axis=1.5)
      
      ##FOURTH GRAPH - iTFR
      plot(ImpliedTFR_Project[1,],type="l",ylim=c(0,8),xlab="Time Step End Year",ylab="",main="Implied TFR by Time Step End Year",cex.lab=2,cex.main=2,axes=F)
      for (i in 1:ITER) {lines(ImpliedTFR_Project[i,],col=sample(6))}
      axis(side=1,at=0:CURRENTSTEP,labels=paste(seq(2010,CURRENTSTEP*5+2010,5)),cex.axis=1.5)
      axis(side=2,cex.axis=1.5)
      
      ##FIFTH GRAPH - NET MIGRATION
      plot(NetMigrAdj_Project[1,],type="l",ylim=c(-.02,.02),xlab="Time Step End Year",ylab="",main="Net Migration Adjustment by Time Step End Year",cex.lab=2,cex.main=2,axes=F)
      for (i in 1:ITER) {lines(NetMigrAdj_Project[i,],col=sample(6))}
      axis(side=1,at=0:CURRENTSTEP,labels=paste(seq(2010,CURRENTSTEP*5+2010,5)),cex.axis=1.5)
      axis(side=2,cex.axis=1.5)
      
      ##SIXTH GRAPH - LIFE EXPECTANCY AT BIRTH (FEMALE AND MALE)
      plot(e0F_Project[1,],type="l",ylim=c(50,110),xlab="Time Step End Year",ylab="",main="e0 (Female and Male) by Time Step End Year",cex.lab=2,cex.main=2,axes=F)
      for (i in 1:ITER) {lines(e0F_Project[i,],col=sample(6))}
      for (i in 1:ITER) {lines(e0M_Project[i,],col=sample(6))}
      axis(side=1,at=0:CURRENTSTEP,labels=paste(seq(2010,CURRENTSTEP*5+2010,5)),cex.axis=1.5)
      axis(side=2,cex.axis=1.5)
      
    }   
  },height=1800,width=1200)
  
  
}

shinyApp(ui = ui, server = server) 


