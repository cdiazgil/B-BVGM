# B-BVGM

With this shiny app you can estimate the growth parameters of your fish population, using size and age data. By using a Bayesian approach to the estimation of parameters, you can derive both population and individual-based growth parameters. The current parametrization is based on the data used in Catalán et al (submitted, MEPS).


The Von Bertalanffy growth model (VBGM) is widely used to fit both Population and Individual data (Pilling et al. 2002), which is based on three parameters: t0 (the age where the fish would have had zero size), a growth rate (k, which is constant throughout the lifespan of the individual) and an asymptotic length (Linf). 


The Bayesian estimation of the parameters of the growth models are implemented and run using the R2jags library of the R package which opens JAGS.

Three Monte Carlo Markov Chains (MCMC) were run and semi-informative priors were considered. More information about the exact Bayesian parameters, such as the initials, priors, thinning to avoid autocorrelation and number of iterations used can be found in the 'extra information' tabs. The Bayesian Von Bertalanffy models implemented in this app were written by Dr. Miquel Palmer from the FishEcology group at the Mediterranean Institute of Advances Studies (IMEDEA)

To get started you can use our example data from the paper “Life-history growth-mortality tradeoffs as revealed by otolith geochemistry in a sedentary coastal fish” submitted to MEPS or load your own data.

If you use our webapp to fit a Bayesian Von Bertalanffy Growth Model (B-VBGM) please cite our work as:...
