#using growth tools to get growth rate estimates with Ije
## ----install packages --------------------------------------------------------
remotes::install_github("ctkremer/mleTools")
remotes::install_github("ctkremer/growthTools")

library(growthTools)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(gridExtra)
library(grid)
library(png)
library(cowplot)

#data
allrfu <- read_csv("data/tpc_processed_all_rfus.csv") %>% 
  mutate(rfu = as.numeric(RFU)) %>% 
  mutate(log_rfu = log(RFU))

ggplot(allrfu,aes(x=days,y=log_rfu))+
  geom_point(aes(colour=treatment))+theme_bw()+
  facet_wrap(~temp)

#subset data
sdat2<-allrfu[allrfu$unique_well =='1_E11_14',]

res<-get.growth.rate(sdat2$days,sdat2$log_rfu,plot.best.Q = T,id = '1_E11_14')
res$best.model
res$best.slope

res$best.model.rsqr

res$best.se

#summary
gdat <- allrfu %>% group_by(unique_well, treatment) %>% 
  do(grs=get.growth.rate(x=.$days,y=.$log_rfu,
                         id=.$unique_well,plot.best.Q=T,fpath="figures/gdat-summaries"))  

sumgdat <- gdat %>% summarise(unique_well, treatment, mu=grs$best.slope,best.model=grs$best.model,
                   best.se=grs$best.se)
View(sumgdat)

gdat3 <- gdat %>% summarise(unique_well, treatment, mu=grs$best.slope,best.model=grs$best.model,
                            best.se=grs$best.se,best.R2=grs$best.model.rsqr,
                            nobs.exp=grs$best.model.slope.n)

write_csv(gdat3, "data/growthtool-gdat-sum.csv")
View(gdat3) #ije code 06, line 77

gdat4 <- gdat3 %>% 
  filter(treatment != "blank_blank") %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp"), sep = "_") %>% 
  unite(plate, well, col = "unique_well", sep = "_")

#  separate(col = treatment, into = c("acclimation_trt", "replicate_flask"), sep = "_")

# gdat4 <- gdat4 %>% 
#   unite(plate, well, tpctemp, col = "unique_well", sep = "_") %>% 
#   unite(acclimation_trt, replicate_flask, col = "treatment", sep = "_")

################################################################################
#ije's code 07 tpc nls fitting
library(tidyverse)
library(broom)
library(lubridate)
library(minpack.lm)
library(rootSolve)
library(forcats)
library(dplyr)
#install.packages("rTPC")
library(rTPC)
#install.packages("nls.multstart")
library(nls.multstart)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
#install.packages("MuMIn")
library(MuMIn)

#ije line 38
# choose model - redoing this with lactin2_1995
mod = 'lactin2_1995'

tester <- gdat4 %>% 
  filter (unique_well == "1_C10")
str(tester)
tester$tpctemp <- as.numeric(tester$tpctemp)

# get start vals
start_vals <- get_start_vals(tester$tpctemp, tester$mu, model_name = 'lactin2_1995')

#>         a          b       tmax    delta_t 
#0.1194843 -0.2540080 42.0000000  6.0000000

#wliune 66 remove ed
# get limits
low_lims <- get_lower_lims(tester$tpctemp, tester$mu, model_name = 'lactin2_1995')
low_lims
#>  a       b    tmax delta_t 
#>     0     -10      12       0 

upper_lims <- get_upper_lims(tester$tpctemp, tester$mu, model_name = 'lactin2_1995')
upper_lims
#>     a       b    tmax delta_t 
#>       1       1     420    3840 

start_lower <- pmax(start_vals - 10, low_lims)
start_upper <- pmin(start_vals + 10, upper_lims)

lactinfit <- nls_multstart(mu ~ lactin2_1995(temp = tpctemp, a, b, tmax, delta_t),
                           data = tester,
                           iter = c(4,4,4,4),
                           start_lower = get_start_vals(tester$tpctemp, tester$mu, model_name = 'lactin2_1995') - 10,
                           start_upper = get_start_vals(tester$tpctemp, tester$mu, model_name = 'lactin2_1995') + 10,
                           lower = low_lims,
                           upper = upper_lims,
                           supp_errors = 'Y', 
                           convergence_count = FALSE)
lactinfit

calc_params(lactinfit) %>%
  mutate_all(round, 2)

#> rmax topt ctmin ctmax    e    eh  q10 thermal_safety_margin
#>  0.6 27.5 11.95 36.08 1.33 -3.31 6.26                  8.58
#> thermal_tolerance breadth skewness
#>             24.13    9.84     4.64

