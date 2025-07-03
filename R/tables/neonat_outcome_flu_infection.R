## tbl_summary version, including counts and percentages ####
### adding p value ####
tab_neonat_flu_tbl_summary_pandemy_only <- pandemy %>%
  mutate(
    Flu_in_pregn_and_in_pandemic = forcats::fct_relevel(Flu_in_pregn_and_in_pandemic, "yes"),  
    LBW = as.numeric(LBW == "yes"),
    PTB_corrected = as.numeric(PTB_corrected == "yes"),
    stillbirth = as.numeric(stillbirth == "yes"),
    neonat_mort_d1_d5 = as.numeric(neonat_mort_d1_d5 == "yes"),
    sex = as.numeric(sex == "Male"),  # or adjust based on your coding
    # Flu_in_pregn_and_in_pandemic = as.factor(Flu_in_pregn_and_in_pandemic)
  ) %>%
  select(birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         sex,LBW,
         PTB_corrected, stillbirth, neonat_mort_d1_d5,Flu_in_pregn_and_in_pandemic
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
tab_neonat_flu_tbl_summary_pandemy_only

tab_neonat_flu_tbl_summary_pandemy_only %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu", "neonatal_characteristics_based_on_matern_flu_pandemy_only.docx"))

