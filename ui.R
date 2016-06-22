
# This is the user-interface definition for the Bayesian von Bertalanffy Growth Model
# Shiny web application.

# Loading library and source code
library(shiny)


# User interface

shinyUI(
 fluidPage(
   
  # Application title
    titlePanel("B-VBGM! Bayesian Von Bertalanffy Growth Model"),
    
   
  # Application 
  navbarPage(NULL,
             tabPanel("Introduction",
                      h2("Welcome fish friend!", align = "center"),
                      h4("In this webapp you can estimate the growth parameters of your fish population, using size and age data. By using a Bayesian approach to the estimation of parameters, you can derive both population and individual-based growth parameters. The current parametrization is based on the data used in Catalán et al (submitted, MEPS)."),
                      br(),
                      HTML("<h4>The Von Bertalanffy growth model (VBGM) is widely used to fit both Population and Individual data (Pilling et al. 2002), which is based on three parameters: t0 (the age where the fish would have had zero size), a growth rate (k, which is constant throughout the lifespan of the individual) and an asymptotic length (Linf).  <img src='vonbertalanffy.gif' height='300' width='500'align='right'></h4>" ),
                      br(),
                      HTML("<h4>The Bayesian estimation of the parameters of the growth models are implemented and run using the R2jags library of the"),
                      a("R package", href="http://www.r-project.org/"),
                      HTML("which opens"),
                      a("JAGS.", href="http://mcmc-jags.sourceforge.net/"),
                      HTML("<h4>Three Monte Carlo Markov Chains (MCMC) were run and semi-informative priors were considered. More information about the exact Bayesian parameters, such as the initials, priors, thinning to avoid autocorrelation and number of iterations used can be found in the 'extra information' tabs. The Bayesian Von Bertalanffy models implemented in this app were written by Dr. Miquel Palmer from the"),
                      a("FishEcology",href="http://www.fishecology.es"),
                      HTML("group at the Mediterranean Institute of Advances Studies"),
                      a("(IMEDEA)",href="http://imedea.uib-csic.es/") ,
                      h4("To get started you can use our example data from the paper “Life-history growth-mortality tradeoffs as revealed by otolith geochemistry in a sedentary coastal fish” submitted to MEPS or load your own data."),
                      h4("If you use our webapp to fit a Bayesian Von Bertalanffy Growth Model (B-VBGM) please cite our work as:..."),
                    br(),
                    HTML("<h4>Warning!!! If your specie does not fit in our parameter configuration (see 'extra information' tabs), feel free to download the R scripts from our repository at"),   
                    a("Github",href="https://github.com/cdiazgil/B-BVGM"),
                    HTML(" and modify it! ")
                    
             ),
              
             tabPanel("Use the models", 
                      h4("To get started you can use our example data from the paper 'Life-history growth-mortality tradeoffs as revealed by otolith geochemistry in a sedentary coastal fish' submitted to MEPS or load your own data. You only need to name 'size', 'age' and 'fishID' (which can be alphanumeric) columns."),
                      h4("Important! Please select the adequate Linf range from the slider below and the number of iterations before hitting 'population' or 'individual' tabs (that will lunch the models)."),
                      br(),
                      sidebarPanel(
                        
  #                      actionButton("goButton", "Use the example"),
                        p('If you want a sample .csv file to upload,',
                          'you can first download the samples from the two populations',
                          a(href = 'DannularisPalma.csv','Palma Bay'),'and',
                          a(href = 'DannularisCabrera.csv','Cabrera National Park'),
                          'files, and then try uploading them.'
                        ),
                        tags$hr(),
                        
                        
                        fileInput('file', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')),
                        
                        tags$hr(),
                        checkboxInput('header', 'Header', TRUE),
                        radioButtons('sep', 'Separator',
                                     c(Comma=',',
                                       Semicolon=';',
                                       Tab='\t'),
                                     ';'),
                        tags$hr(),
                        sliderInput("linfrange", label = h3("Linf range:"), min = 1, 
                                    max = 500, value = c(1, 10)
                        ),
                        
                        sliderInput("iterationNumber", label=h3("Number of Iterations:"), 
                                    min=1000, max=100000, value=5000)
                                      
                      ),
                      
                      mainPanel(   
                        tags$link(rel="stylesheet", type="text/css",href="styles.css"),

                        
                        uiOutput("tb")
                       
                        )
                      
                      ),
             tabPanel("Extra information population model",
                      h2("Population B-VBGM", align = "center"),
                      h4("The VBGM parameters are estimated for the whole population using the size-at-age of capture information (years-size), i.e. each individual has only one data point. It can be used with “size” referring to otolith size or fish length. Using a Bayesian hierarchical fitting strategy (Lunn et al. 2000) allows to use some a priori information for the estimates of the parameters. The model is implemented and run using the R2jags library of the R package (http://www.r-project.org/), which opens JAGS (Just Another Gibbs Sampler, http://mathstat.helsinki.fi/openbugs/). Three Monte Carlo Markov Chains (MCMC) are run and semi-informative priors are considered. Specifically, prior distributions are based on the reasoning that all individuals come hierarchically from the same population with common normal distribution (mean = 0 and tolerance (1/square root of s.d.) equal to a value sampled from a gamma distribution with parameters with shape and scale = 0.001). Uniform distributions were considered for the VBGM parameters: t0 from -2 to 2 years, Linf range from 0 to 300 size units (to include the otolith measurements) and k range from 0 to 1 years-1. These values were based on the previous literature reported for small bodied coastal species at population levels (Gordoa and Molí 1997; Pajuelo and Lorenzo 2001; Alós et al. 2010) and in all cases, the posterior distributions were much narrower than a the priors distributions. The first 100,000 iterations for all of the parameters were discarded (burned) and a thinning strategy adopted to ensure the temporal independence of successive values within the chain (only one out of every 10 consecutive values was kept). The convergence of the MCMC chains of all parameters was assessed by visual inspection of the plots of the iterations and tested using the Gelman-Rubin Statistic (Plummer et al. 2006), with values < 1.1 indicating convergence (Gelman et al. 2014). In all cases convergence was obtained after various iterations depending on the simulation (between 3,000 and 9,000 valid iterations) after applying a burning and thinning of the posterior distribution as described above. Among-population differences in the VBGM can be visualized using the Bayesian Credibility Intervals (2.5%-97.5%) of the posterior distribution of each parameter, and the Bayesian means can be used for testing the relationship among the VBGM parameters and any variable of interest."),
                      h4("If you use our webapp to fit a Bayesian Von Bertalanffy Growth Model (B-VBGM) please cite our work as:..."),
                      br(),
                      HTML("<h4>Warning!!! If your specie does not fit in our parameter configuration (see 'extra information' tabs), feel free to download the R scripts from our repository at"),   
                      a("Github",href="https://github.com/cdiazgil/B-BVGM"),
                      HTML(" and modify it! ")                      
                      
             ),
             tabPanel("Extra information Individual model",
                      h2("Individual B-VBGM", align = "center"),
                      h4("The VBGM parameter are estimated for each individual using a Bayesian hierarchical fitting strategy (Lunn et al. 2000), which allows to use some a priori information for the estimates of the parameters and the complexity of individual modelling. This is the case when you have several size-at-age points for each individual, i.e. back-calculated sizes at different ages (from the otoliths or scales). The model is implemented and run using the R2jags library of the R package (http://www.r-project.org/), which opens JAGS (Just Another Gibbs Sampler, http://mathstat.helsinki.fi/openbugs/). Three Monte Carlo Markov Chains (MCMC) were run and semi-informative priors were considered. Specifically, prior distribution are based on the reasoning that all individuals come hierarchically from the same population with common normal distribution (mean = 0 and tolerance (1/square root of s.d.) equal to a value sampled from a gamma distribution with parameters with shape and scale = 0.001). Uniform distributions were considered for the VBGM parameters: t0 from -2 to 2 years, Linf range from 0 to 300 size units (to include the otolith measurements) and k range from 0 to 1 years-1. These values were based on the previous literature reported for small bodied coastal species at population level (Gordoa and Molí 1997; Pajuelo and Lorenzo 2001; Alós et al. 2010) and in all cases, the posterior distributions were much narrower than a priori distributions. The first 100,000 iterations for all of the parameters were discarded (burned) and a thinning strategy adopted to ensure the temporal independence of successive values within the chain (only one out of every 10 consecutive values was kept). The convergence of the MCMC chains of all parameters was assessed by visual inspection of the plots of the iterations and tested using the Gelman-Rubin Statistic (Plummer et al. 2006), with values < 1.1 indicating convergence (Gelman et al. 2014). In all cases convergence was obtained after various iterations depending on the simulation (between 3,000 and 9,000 valid iterations) after applying a burning and thinning of the posterior distribution as described above. Among-individual differences in the VBGM were visualized using the Bayesian Credibility Intervals (2.5%-97.5%) of the posterior distribution of each parameter, and the Bayesian mean was used for testing the relationship among the VBGM parameters and for example the micro-chemical composition of the otoliths as seen in the paper “Life-history growth-mortality tradeoffs as revealed by otolith geochemistry in a sedentary coastal fish”."),
                      h4("If you use our webapp to fit a Bayesian Von Bertalanffy Growth Model (B-VBGM) please cite our work as:..."),
                      br(),
                      HTML("<h4>Warning!!! If your specie does not fit in our parameter configuration (see 'extra information' tabs), feel free to download the R scripts from our repository at"),   
                      a("Github",href="https://github.com/cdiazgil/B-BVGM"),
                      HTML(" and modify it! ")                      
             ),
  tabPanel("References",
           h2("References", align = "center"),
           
           h4("Alós, J., Palmer, M., Alonso-Fernández, A. and Morales-Nin, B. (2010) Individual variability and sex-related differences in the growth of Diplodus annularis (Linnaeus, 1758). Fisheries Research 101, 60–69."),
           h4("Gelman, A., Carlin, J.B., Stern, H.S. and Rubin, D.B. (2014) Bayesian data analysis. Taylor & Francis."),
           h4("Gordoa, A. and Molí, B. (1997) Age and growth of the sparids Diplodus vulgaris, D. sargus and D. annularis in adult populations and the differences in their juvenile growth patterns in the north-western Mediterranean Sea . Fisheries Research 33, 123–129."),
           h4("Pajuelo, J.G. and Lorenzo, J.M. (2001) Biology of the annular seabream, Diplodus annularis (Sparidae), in coastal waters of the Canary Islands. Journal of Applied Ichthyology 17, 121–125."),
           h4("Pilling, G.M., Kirkwood, G.P. and Walker, S.G. (2002) An improved method for estimating individual growth variability in fish, and the correlation between von Bertalanffy growth parameters. Canadian Journal of Fisheries and Aquatic Sciences 59, 424–432."),
           h4("Plummer, M., Best, N., Cowles, K. and Vines, K. (2006) CODA: convergence diagnosis and output analysis for MCMC. R News 6."),
           h4("Rohlf, F.J. (2004) tpsDig. version 2.16 (software)."),
           br(),
           
           h4("If you use our webappto fit a Bayesian Von Bertalanffy Growth Model (B-VBGM) please cite our work as:..."),
           HTML("<h4>Warning!!! If your specie does not fit in our parameter configuration (see 'extra information' tabs), feel free to download the R scripts from our repository at"),   
           a("Github",href="https://github.com/cdiazgil/B-BVGM"),
           HTML(" and modify it! ")
           
  )
             
             ),
 
  

  fluidRow(column(12,
                  HTML("<br>"),
                  HTML("<h5>This web app was written by"),
                  HTML("Carlos Díaz-Gil and Roc Itxart Alba"),
                  a("Fishecology group at IMEDEA", href="http://www.fishecology.es"), 
                  HTML("Balearic Islands, Spain"),
                  a("R using RStudio's Shiny package. ", href="http://shiny.rstudio.com/"),
                  a("Contact Carlos about the app.", href="mailto:cdiaz@imedea.uib-csic.es")
  ))
))

#------------------------------------------------------------

# "Life-history growth-mortality tradeoffs as revealed by 
#  otolith geochemistry in a sedentary coastal fish"

# I.A. Catal�n, J. Al�s, C. D�az-Gil, S. P�rez, 
# G. Basterretxea, B. Morales-Nin and M. Palmer

# Bayesian von Bertalanffy Growth Model (B-VBGM)
# Model designed and implemented by Miquel Palmer
# Shiny app designed and constructed by Carlos D�az-Gil and
# Roc Itxart Alba
# 
#
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)
# Laboratory of Marine Research and Aquaculture (LIMIA)
#
#------------------------------------------------------------
