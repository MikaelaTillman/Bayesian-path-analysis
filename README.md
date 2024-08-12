# Bayesian path analysis of life-history trait in a capital breeder

## Project description
We have developed a mechanistic path model to disentangle the direct and indirect effects of density dependence and environmental variability on body mass and reproductive success in a capital breeder. All models and visualisations are run in R.
This study focuses on one extensively monitored herd of semi-domestic reindeer in northern Norway. We included individuals of known age and older than 1 year, with at least one complete set of observations on three consecutive occasions: in autumn, the subsequent spring, and the following autumn. This resulted in a sample size of 814 observations from 235 females in 18 years.
To assess the contribution of direct, indirect, and total effects of environmental variables and population density on seasonal body mass and reproductive success, we built a mechanistic path model. The path model consisted of three sub-models, one for female spring body mass, one for reproductive success, and one for female autumn body mass. The model allowed for the mediating effect of spring body mass on reproductive success and the mediating effect of spring body mass and reproductive success on autumn body mass.
The path model was first fitted in piecewiseSEM to test the conditional independence claims stated by the path model. Then the path coefficients were estimated in a Bayesian framework in STAN through the brms-package.

## Directory structure
1. /data: Contains data files used in the project
2. /scripts: Contains R scripts and notebooks used for data analysis
3. /results: Contains the results of your analysis
4. /output: Output of all the visualization of data analysis

## Acknowledgments
I would like to express my gratitude to my supervisors Torkild Tveraa, Sandra Hamel, John-André Henden, Audun Stien and Nigel Gilles Yoccoz or their guidance and support throughout this project.

## Contact
* Mikaela Tillman
* mikaela.tilman@nina.no


