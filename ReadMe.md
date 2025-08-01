<pre>
R CODE FOR HAMILTON-PERRY PROJECTION WITH COMPONENTS AND STABLE POPULATION INFORMATION

-----
Some ideas for potential future work: 
      -possible cohort change ratios averaging and/or trending (on longer time series of single-year data)
      -employment- and/or housing-based migration setup
            (following https://applieddemogtoolbox.github.io/Toolbox/#EmplPopHousProj)
      -use of uncertain starting data (along with stochastic option) as a potential way to manage uncertain from Differential Privacy on inputs
      -optimize input indices (selected, etc) for fit to historical data (to better see/understand best-possible) (may not be helpful, but may be interesting)
      -consider a 0-net-migration-level-based benchmark for gross migration adjustment
      -use age-adjusted net migration-adjustment profiles  
      -Apply a direct fertility option - thinking, similar to mortality, setting by initial and end-step or time series rule - to offer as alternative to iTFR
      
-----
References, resources, and related info: 
  Software: 
    -R: https://www.r-project.org/
    -Shiny for R: https://shiny.rstudio.com/

  Description, formulas used, and spreadsheet demonstration: 
    -Hunsinger (2020 and 2021): https://github.com/edyhsgr/CCRStable/tree/master/Oct2020Presentation 
  
  Population estimates inputs: 
    -US Census Bureau Population Estimates: https://www.census.gov/programs-surveys/popest.html
    -Carl Schmertmann's R code to tabulate and (re-)aggregate US Census Bureau's American Community Survey estimates by demographic characteristics: https://github.com/schmert/bonecave/blob/master/demography-US-congressional-districts/population-pyramids-by-party.R 
    
  More information on iTFR: 
    -Hauer and Schmertmann (2019): https://osf.io/adu98/
    -Hauer, Baker, and Brown (2013): https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0067226

  Net migration by age over time comparisons from Alaska data: 
    -Hunsinger (2018): https://raw.githubusercontent.com/edyhsgr/AKMigrationProfiles/refs/heads/master/AKMigrationProfiles_app.R 
    
  Interface with net migration adjustment examples and comparisons: 
    -Hunsinger (2019): https://raw.githubusercontent.com/edyhsgr/NetMigrationAdjustCompare/refs/heads/master/app.R  
    
  Graphs of e0 and Brass relational life table model by US state: 
    -https://github.com/edyhsgr/BrassRelationalMortOverTime_USAStates 
    
  Model life table (0.0 alpha) is the 5x5 and 1x5 2010 to 2014 life tables for selected states from the United States Mortality Database:
    -https://usa.mortality.org/index.php
    
  Migration adjustment profile was made from the US Census Bureau's 2013 to 2017 American Community Survey Public Use Microdata Sample, accessed via IPUMS USA, University of Minnesota: 
    -https://usa.ipums.org/usa/

  Supporting work and thinking on stochastic population projection: 
    -https://applieddemogtoolbox.github.io#StochasticForecast

  Applied Demography Toolbox listing: 
    -https://applieddemogtoolbox.github.io#CCRStable

  Related Shiny for R application: 
    -(Hunsinger 2019) https://edyhsgr.shinyapps.io/CCRStable_California/
    
-Eddie Hunsinger, August 2019 (updated July 2025)

-----
<a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/3.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by-sa/3.0/igo/">Creative Commons Attribution-ShareAlike 3.0 International License</a>.
</pre>
