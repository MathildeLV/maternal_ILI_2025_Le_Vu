
# tbl_summary version, including counts and percentages ####
# adding p value
## Males ####
tab_neonat_flu_tbl_summary_sex_M <- pandemy %>%
  mutate(
    Flu_in_pregn_and_in_pandemic = forcats::fct_relevel(Flu_in_pregn_and_in_pandemic, "yes"),  
    LBW = as.numeric(LBW == "yes"),
    PTB_corrected = as.numeric(PTB_corrected == "yes"),
    stillbirth = as.numeric(stillbirth == "yes"),
    neonat_mort_d1_d5 = as.numeric(neonat_mort_d1_d5 == "yes"),
    # Flu_in_pregn_and_in_pandemic = as.factor(Flu_in_pregn_and_in_pandemic)
  ) %>%
filter(sex=="Male") %>%
  select(birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW, PTB_corrected, stillbirth, neonat_mort_d1_d5, Flu_in_pregn_and_in_pandemic
  ) %>%  
  gtsummary::tbl_summary(
    by=Flu_in_pregn_and_in_pandemic,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_dichotomous() ~ 0
    ),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
add_difference()
tab_neonat_flu_tbl_summary_sex_M

tab_neonat_flu_tbl_summary_sex_M %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu/sex_stratifications", "neonatal_characteristics_based_on_mat_flu_tbl_pandemy_only_M.docx"))


## Females ####
tab_neonat_flu_tbl_summary_sex_F <- pandemy %>%
  filter(sex=="Female") %>%
  mutate(
    Flu_in_pregn_and_in_pandemic = forcats::fct_relevel(Flu_in_pregn_and_in_pandemic, "yes"),  
    LBW = as.numeric(LBW == "yes"),
    PTB_corrected = as.numeric(PTB_corrected == "yes"),
    stillbirth = as.numeric(stillbirth == "yes"),
    neonat_mort_d1_d5 = as.numeric(neonat_mort_d1_d5 == "yes"),
    # Flu_in_pregn_and_in_pandemic = as.factor(Flu_in_pregn_and_in_pandemic)
  ) %>%
  select(birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW, PTB_corrected,stillbirth, neonat_mort_d1_d5, Flu_in_pregn_and_in_pandemic
  ) %>%  
  gtsummary::tbl_summary(
    by=Flu_in_pregn_and_in_pandemic,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_dichotomous() ~ 0
    ),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  add_difference()
tab_neonat_flu_tbl_summary_sex_F

tab_neonat_flu_tbl_summary_sex_F %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu/sex_stratifications", "neonatal_characteristics_based_on_mat_flu_tbl_pandemy_only_F.docx"))


