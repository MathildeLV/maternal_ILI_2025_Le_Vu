#.libPaths(c("H:/Documents/R/win-library/4.1","C:/Program Files/R/R-4.4.1/library"))

# packages  ####
library(lme4) # for mixed effect model
library(conflicted)
library(lubridate)
library(openxlsx)
library(here)
library(dplyr)
library(readxl)
library(tidyverse)
library(rmarkdown)
library(Hmisc)
library(gt)
library(gtsummary) # for gt summary tables of models (gtsummary::tbl_regression(m))
library(mgcv)
library(ggplot2)
library(effects)
library(ggeffects)
library(emmeans)
library(scales)
library(splines)
library(gridExtra)
library(janitor)
library(questionr) # for predicted probabilites/OR by different subgroups etc ggpredict, ggeffects
library("Hmisc") #I use it for labeling variables
library(ggdist) # for rainclouds/box plots, visualizing distributions and uncertainty
library(egg)
library(grid)
library(smd) # for adding add_difference in tbl_summary
library(car) #to check for VIF factor of collinearity
library(scales)
library(ggplot2)
library(scatterplot3d)
library(gamlss)
library(latexpdf)
library(tinytex)
library(rlang)
library(backports)
library(knitr)
library(ggstance)
library(tidycmprsk)
library(cardx)
library(ggsci)
library(MASS)
library(patchwork)
library(ggpubr)
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("count", "dplyr")

# data ####
laus <- read_excel(here("data", "laus.xlsx"))
flu_waves <- read.csv2(here("data", "laus_vaud_flu_waves.csv"))
abortions <- read_excel(here("data", "abortions.xlsx")) 

# graph parameters ####
lwdline <- 1.2
size_axis <- 14
size_axis_title <- 16
size_legend <- 12
size_legend_title <- 14
axis.title.x.position <- element_text(margin = margin(t =12))
size_title <- element_text(size=12)
lwd_size <- 0.6

# colours adapted for colour blind persons
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

text.y.axis=element_text(color="black",size=14)
text.x.axis=element_text(color="black",size=14,angle=45,hjust=1.2)
text.legend=element_text(size=13)
title.legend=element_text(size=14)
title.axis <- element_text(size=14)

# loading R codes ####
source("R/cleaning.R") 
source("R/functions.R") 
source("R/flu_cleaning.R") 
source("R/Inclusions.R") 
source("R/miscarriages_cleaning.R") 

# tables ####
## maternal and neonatal charact, overall and by year ####
source("R/tables/suppl/tables_descriptive_maternal_infant_charact_global_and_yearly.R") 

## maternal charact, dependinf on ILI, ILI timing, ILI severity and by year ####
source("R/tables/suppl/tables_descriptive_gt_infected_mothers.R") 

## comparing neonat outcomes depending on flu exposure, by trimester and severity of flu ####
source("R/tables/neonat_outcome_flu_infection.R") 
source("R/tables/neonat_outcomes_flu_trimester_and_severity.R") 
### by sex ####
source("R/tables/sex_stratif/neonat_outcome_flu_infection_by_sex.R") 
source("R/tables/suppl/sex_stratifications/neonat_outcome_flu_trimester_and_severity_by_sex.R") 

## miscarriages: year trend and comparison of mat charact depending on delivery vs abortion ####
source("R/tables/suppl/miscarriages.R") 

# models  ####
## main models  ####
render("R/main_GLMs/univariable_GLMs_RLMs.Rmd", output_file = ("../../output/tables/main_models/main_RLMs_GLMs_univariable.html"))
render("R/main_GLMs/multivariable_GLMs_RLMs.Rmd", output_file = ("../../output/tables/main_models/main_RLMs_GLMs_multivariable.html"))

## models by trimester  ####
render("R/GLMs_trimester_severity/univariable_RLMs_GLMs_trimester.Rmd", output_file = ("../../output/tables/models_flu_trimester_or_severity/trimester/univariable/univariable_RLMs_GLMs.html"))
render("R/GLMs_trimester_severity/multivariable_RLMs_GLMs_trimester.Rmd", output_file = ("../../output/tables/models_flu_trimester_or_severity/trimester/multivariable/multivariable_RLMs_GLMs.html"))

## models by severity  ####
render("R/GLMs_trimester_severity/univariable_RLMs_GLMs_sev.Rmd", output_file = ("../../output/tables/models_flu_trimester_or_severity/severity/univariable/univariable_RLMs_GLMs.html"))
render("R/GLMs_trimester_severity/multivariable_RLMs_GLMs_sev.Rmd", output_file = ("../../output/tables/models_flu_trimester_or_severity/severity/multivariable/multivariable_RLMs_GLMs.html"))

## sex statified models  ####
render("R/main_GLMs/sex_stratif/univariable_RLM_GLMs_strat_sex.Rmd", output_file = ("../../../output/tables/main_models/sex_stratif/univariable_GLMs_RLMs.html"))
### by trimester  ####
render("R/GLMs_trimester_severity/sex_stratif/univariable_RLMs_GLMs_trimester_sex.Rmd", output_file = ("../../../output/tables/models_flu_trimester_or_severity/sex_stratif/trimester/sex_stratif_trimester_univariable_GLMs_RLMs.html"))
### by severity  ####
render("R/GLMs_trimester_severity/sex_stratif/univariable_RLMs_GLMs_sev_sex.Rmd", output_file = ("../../../output/tables/models_flu_trimester_or_severity/sex_stratif/severity/sex_stratif_severity_univariable_RLMs_GLMs.html"))


# graphs ####
## timing of the pandemic waves: incidence of deaths in Lausanne and incidence of hosp due to resp diseases in Vaud ####
source("R/graphs/flu_incid_death_counts_hospit_Laus_Vaud.R") 
## distribution maternal and neonatal variables over the years ####
source("R/graphs/distrib_mat_neonat_var_per_year.R") 
## distribution of maternal ILI cases during the pandemic ####
source("R/graphs/maternal_flu_cases_over_time.R") 
## miscarriages over time and relative to deliveries ####
source("R/graphs/miscarriages_over_time_and_relative_to_births.R") 
## forest plots: summary of models ####
source("R/graphs/forest_plot_rlm_glm_marginall_means_prop.R") 
