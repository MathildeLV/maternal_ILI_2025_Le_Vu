# Neonatal characteristics: flu severity #### 
# severity ####
## all severity ####
tab_neonat_flu_sev <-  pandemy %>%
  select(Grippe_severity3,birthweight,head_circ,placentaweight, 
         babylength,PI, GA_weeks_corrected,
         sex,LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=Grippe_severity3,
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
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_sev

tab_neonat_flu_sev %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu", "tab_neonat_flu_sev.docx"))


## mild symptoms (difference) ####
tab_neonat_flu_sev_mild <-  pandemy %>%
  mutate(
    Grippe_severity3 = forcats::fct_relevel(Grippe_severity3, "mild"),  
    LBW = as.numeric(LBW == "yes"),
    PTB_corrected = as.numeric(PTB_corrected == "yes"),
    stillbirth = as.numeric(stillbirth == "yes"),
    neonat_mort_d1_d5 = as.numeric(neonat_mort_d1_d5 == "yes"),
    sex = as.numeric(sex == "Male"),  
  ) %>%
  filter(Grippe_severity3=="not infected" | Grippe_severity3=="mild") %>%
  mutate(Grippe_severity3=droplevels(Grippe_severity3))%>%
  select(Grippe_severity3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         sex,LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=Grippe_severity3,
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
tab_neonat_flu_sev_mild

tab_neonat_flu_sev_mild %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu", "tab_neonat_flu_sev_mild.docx"))


## severe symptoms (difference) ####
tab_neonat_flu_sev_severe <-  pandemy %>%
  mutate(
    Grippe_severity3 = forcats::fct_relevel(Grippe_severity3, "severe"),  
    LBW = as.numeric(LBW == "yes"),
    PTB_corrected = as.numeric(PTB_corrected == "yes"),
    stillbirth = as.numeric(stillbirth == "yes"),
    neonat_mort_d1_d5 = as.numeric(neonat_mort_d1_d5 == "yes"),
    sex = as.numeric(sex == "Male"),  
  ) %>%
  filter(Grippe_severity3=="not infected" | Grippe_severity3=="severe") %>%
  mutate(Grippe_severity3=droplevels(Grippe_severity3))%>%
  select(Grippe_severity3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         sex,LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=Grippe_severity3,
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
tab_neonat_flu_sev_severe

tab_neonat_flu_sev_severe %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu", "tab_neonat_flu_sev_severe.docx"))

# Neonatal characteristics: trimester of infection #### 
## all trimesters ####
tab_neonat_flu_trim <-  pandemy %>%
  mutate(
    Flu_in_pregn_and_in_pandemic = forcats::fct_relevel(Flu_in_pregn_and_in_pandemic, "yes"),  
    LBW = as.numeric(LBW == "yes"),
    PTB_corrected = as.numeric(PTB_corrected == "yes"),
    stillbirth = as.numeric(stillbirth == "yes"),
    neonat_mort_d1_d5 = as.numeric(neonat_mort_d1_d5 == "yes"),
    sex = as.numeric(sex == "Male"),  
  ) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI, GA_weeks_corrected,
         sex,LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
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
  gtsummary::modify_header(label ~ "**Variable**")
tab_neonat_flu_trim

tab_neonat_flu_trim %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu", "neonatal_characteristics_based_on_matern_flu_timing_trim.docx"))


## T1 ####
tab_neonat_flu_trim_T1 <-  pandemy %>%
  mutate(
    trimester_real_def3 = forcats::fct_relevel(trimester_real_def3, "first"),  
    LBW = as.numeric(LBW == "yes"),
    PTB_corrected = as.numeric(PTB_corrected == "yes"),
    stillbirth = as.numeric(stillbirth == "yes"),
    neonat_mort_d1_d5 = as.numeric(neonat_mort_d1_d5 == "yes"),
    sex = as.numeric(sex == "Male"),  
  ) %>%
  filter(trimester_real_def3=="not infected" | trimester_real_def3=="first") %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI, GA_weeks_corrected,
         sex,LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
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
tab_neonat_flu_trim_T1

tab_neonat_flu_trim_T1 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu", 
              "neonatal_characteristics_based_on_matern_flu_timing_trim_T1.docx"))

## T2 ####
tab_neonat_flu_trim_T2 <-  pandemy %>%
  mutate(
    trimester_real_def3 = forcats::fct_relevel(trimester_real_def3, "second"),  
    LBW = as.numeric(LBW == "yes"),
    PTB_corrected = as.numeric(PTB_corrected == "yes"),
    stillbirth = as.numeric(stillbirth == "yes"),
    neonat_mort_d1_d5 = as.numeric(neonat_mort_d1_d5 == "yes"),
    sex = as.numeric(sex == "Male"),  
  ) %>%
  filter(trimester_real_def3=="not infected" | trimester_real_def3=="second") %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI, GA_weeks_corrected,
         sex,LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
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
tab_neonat_flu_trim_T2

tab_neonat_flu_trim_T2 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu", 
              "neonatal_characteristics_based_on_matern_flu_timing_trim_T2.docx"))

## T3 ####
tab_neonat_flu_trim_T3 <-  pandemy %>%
  mutate(
    trimester_real_def3 = forcats::fct_relevel(trimester_real_def3, "third"),  
    LBW = as.numeric(LBW == "yes"),
    PTB_corrected = as.numeric(PTB_corrected == "yes"),
    stillbirth = as.numeric(stillbirth == "yes"),
    neonat_mort_d1_d5 = as.numeric(neonat_mort_d1_d5 == "yes"),
    sex = as.numeric(sex == "Male"),  
  ) %>%
  filter(trimester_real_def3=="not infected" | trimester_real_def3=="third") %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI, GA_weeks_corrected,
         sex,LBW,
         PTB_corrected, 
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
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
tab_neonat_flu_trim_T3

tab_neonat_flu_trim_T3 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/neonatal_outcomes_depending_on_flu", 
              "neonatal_characteristics_based_on_matern_flu_timing_trim_T3.docx"))
