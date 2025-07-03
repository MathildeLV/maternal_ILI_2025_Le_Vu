# Maternal characteristics of those who had the flu (before or during pregnancy or at delivery) vs. those who did not ####
# pandemic timing only
# cleaning variables
pandemy1 <- pandemy %>%
  mutate(
    Flu_in_pregn_and_in_pandemic = forcats::fct_relevel(Flu_in_pregn_and_in_pandemic, "yes"),  
    civil_status_cat2_mar = as.numeric(civil_status_cat2=="married"),  
    parity_cat2_1 = as.numeric(parity_cat2=="1"),  
    parity_cat2_2 = as.numeric(parity_cat2=="2"),  
    parity_cat2_more2 = as.numeric(parity_cat2==">2"),  
    morphology_neit = as.numeric(morphology=="neither"),  
    morphology_ob= as.numeric(morphology=="obese"),  
    morphology_th= as.numeric(morphology=="thin"),  
    hisco_class_3_2= as.numeric(hisco_class_3=="2"),  
    hisco_class_3_1= as.numeric(hisco_class_3=="1"),  
    hisco_class_3_3= as.numeric(hisco_class_3=="3"),  
    hisco_class_3_mis= as.numeric(hisco_class_3=="missing"),  
    season_sp= as.numeric(season=="spring"),  
    season_su= as.numeric(season=="summer"),
    season_au= as.numeric(season=="autumn"),
    season_wi= as.numeric(season=="winter"),
Goitre = as.numeric(Goitre == "yes"),
rickets = as.numeric(rickets == "yes"),
Syphilis_2 = as.numeric(Syphilis_2 == "yes"),
Mother_deceased = as.numeric(Mother_deceased == "yes"),
Lausanne_NEU = as.numeric(Lausanne_NEU == "yes"))

mat_charc_had_flu_pandemy <- pandemy1 %>%
  select(age_mother, height,
         civil_status_cat2_mar,  
         parity_cat2_1, parity_cat2_2, parity_cat2_more2, Lausanne_NEU,  
         morphology_neit,morphology_ob, morphology_th,
         Goitre, rickets,
         Flu_in_pregn_and_in_pandemic, Syphilis_2, 
         Mother_deceased, 
         hisco_class_3_2, hisco_class_3_1, hisco_class_3_3, hisco_class_3_mis,
         season_sp, season_su, season_au, season_wi
         ) %>%  
  gtsummary::tbl_summary(by=Flu_in_pregn_and_in_pandemic,
                         statistic = list(
                           all_continuous() ~ "{mean} ({sd})",
                           all_categorical() ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  add_difference()
mat_charc_had_flu_pandemy

mat_charc_had_flu_pandemy %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_char_flu_vs_no_flu_pandemy_diff.docx"))


# Maternal characteristics of depending on flu timing (trimester)  ####
# pandemic timing only
## ALL trimesters ####
mat_charc_flu_timing <- pandemy %>%
  select(age_mother, height,
         civil_status_cat2, parity_cat2, Lausanne_NEU,  
         morphology, Goitre, rickets,
         Syphilis_2, hisco_class_3, season, trimester_real_def3
  ) %>%  
  gtsummary::tbl_summary(by=trimester_real_def3,
                         statistic = list(
                           all_continuous() ~ "{mean} ({sd})",
                          all_categorical() ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_flu_timing

mat_charc_flu_timing %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_charc_flu_timing.docx"))

## add diff: flu 1st trim vs no flu ####
mat_charc_flu_timing_diff_1st <- pandemy1 %>%
  filter(trimester_real_def3=="not infected" |
           trimester_real_def3=="first") %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  mutate(trimester_real_def3 = forcats::fct_relevel(trimester_real_def3, "first"))%>%
  select(age_mother, height,
         civil_status_cat2_mar,  
         parity_cat2_1, parity_cat2_2, parity_cat2_more2, Lausanne_NEU,  
         morphology_neit,morphology_ob, morphology_th,
         Goitre, rickets,
         Syphilis_2, 
         Mother_deceased, 
         hisco_class_3_2, hisco_class_3_1, hisco_class_3_3, hisco_class_3_mis,
         season_sp, season_su, season_au, season_wi,
         trimester_real_def3
  ) %>%  
  gtsummary::tbl_summary(by=trimester_real_def3,
                         statistic = list(
                           all_continuous() ~ "{mean} ({sd})",
                           all_categorical() ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  add_difference() %>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_flu_timing_diff_1st

mat_charc_flu_timing_diff_1st %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_charc_flu_timing_diff_1st.docx"))

## add diff: flu 2nd trim vs no flu ####
mat_charc_flu_timing_diff_2nd <- pandemy1 %>%
  filter(trimester_real_def3=="not infected" |
           trimester_real_def3=="second") %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  mutate(trimester_real_def3 = forcats::fct_relevel(trimester_real_def3, "second"))%>%
  select(age_mother, height,
         civil_status_cat2_mar,  
         parity_cat2_1, parity_cat2_2, parity_cat2_more2, Lausanne_NEU,  
         morphology_neit,morphology_ob, morphology_th,
         Goitre, rickets,
         Syphilis_2, 
         Mother_deceased, 
         hisco_class_3_2, hisco_class_3_1, hisco_class_3_3, hisco_class_3_mis,
         season_sp, season_su, season_au, season_wi,
         trimester_real_def3
  ) %>%  
  gtsummary::tbl_summary(by=trimester_real_def3,
                         statistic = list(
                           all_continuous() ~ "{mean} ({sd})",
                           all_categorical() ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  add_difference() %>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_flu_timing_diff_2nd

mat_charc_flu_timing_diff_2nd %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_charc_flu_timing_diff_2nd.docx"))

## add diff: flu 3rd trim vs no flu ####
mat_charc_flu_timing_diff_3rd <- pandemy1 %>%
  filter(trimester_real_def3=="not infected" |
           trimester_real_def3=="third") %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  mutate(trimester_real_def3 = forcats::fct_relevel(trimester_real_def3, "third"))%>%
  select(age_mother, height,
         civil_status_cat2_mar,  
         parity_cat2_1, parity_cat2_2, parity_cat2_more2, Lausanne_NEU,  
         morphology_neit,morphology_ob, morphology_th,
         Goitre, rickets,
         Syphilis_2, 
         Mother_deceased, 
         hisco_class_3_2, hisco_class_3_1, hisco_class_3_3, hisco_class_3_mis,
         season_sp, season_su, season_au, season_wi,
         trimester_real_def3
  ) %>%  
  gtsummary::tbl_summary(by=trimester_real_def3,
                         statistic = list(
                           all_continuous ()~ "{mean} ({sd})",
                           all_categorical()  ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  add_difference() %>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_flu_timing_diff_3rd

mat_charc_flu_timing_diff_3rd %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_charc_flu_timing_diff_3rd.docx"))

## add diff: flu missing trim vs no flu ####
mat_charc_flu_timing_diff_miss <- pandemy1 %>%
  filter(trimester_real_def3=="not infected" |
           trimester_real_def3=="unknown") %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  mutate(trimester_real_def3 = forcats::fct_relevel(trimester_real_def3, "unknown"))%>%
  select(age_mother, height,
         civil_status_cat2_mar,  
         parity_cat2_1, parity_cat2_2, parity_cat2_more2, Lausanne_NEU,  
         morphology_neit,morphology_ob, morphology_th,
         Goitre, rickets,
         Syphilis_2, 
         Mother_deceased, 
         hisco_class_3_2, hisco_class_3_1, hisco_class_3_3, hisco_class_3_mis,
         season_sp, season_su, season_au, season_wi,
         trimester_real_def3
  ) %>%  
  gtsummary::tbl_summary(by=trimester_real_def3,
                         statistic = list(
                           all_continuous() ~ "{mean} ({sd})",
                           all_categorical() ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  add_difference() %>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_flu_timing_diff_miss

mat_charc_flu_timing_diff_miss %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_charc_flu_timing_diff_missing.docx"))

# Maternal characteristics of depending on flu severity  ####
# pandemic timing only
## all severities ####
mat_charc_flu_sev <- pandemy1 %>%
  select(age_mother, height,
         civil_status_cat2, parity_cat2, Lausanne_NEU,  
         morphology, Goitre, rickets,
         Syphilis_2, hisco_class_3, season, Grippe_severity3
  ) %>%  
  gtsummary::tbl_summary(by=Grippe_severity3,
                         statistic = list(
                           all_continuous ()~ "{mean} ({sd})",
                           all_categorical() ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_flu_sev

mat_charc_flu_sev %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_charc_flu_sev.docx"))


## add diff: flu mild vs no flu ####
mat_charc_flu_sev_dif_mild <- pandemy1 %>%
  filter(Grippe_severity3=="mild" | Grippe_severity3 =="not infected") %>%
  mutate(Grippe_severity3=droplevels(Grippe_severity3)) %>%
  mutate(Grippe_severity3 = forcats::fct_relevel(Grippe_severity3, "mild"))%>%
  select(age_mother, height,
         civil_status_cat2_mar,  
         parity_cat2_1, parity_cat2_2, parity_cat2_more2, Lausanne_NEU,  
         morphology_neit,morphology_ob, morphology_th,
         Goitre, rickets,
         Syphilis_2, 
         Mother_deceased, 
         hisco_class_3_2, hisco_class_3_1, hisco_class_3_3, hisco_class_3_mis,
         season_sp, season_su, season_au, season_wi, Grippe_severity3
  ) %>%  
  gtsummary::tbl_summary(by=Grippe_severity3,
                         statistic = list(
                           all_continuous ()~ "{mean} ({sd})",
                           all_categorical()  ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  add_difference() %>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_flu_sev_dif_mild

mat_charc_flu_sev_dif_mild %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_charc_flu_sev_dif_mild.docx"))

## add diff: flu severe vs no flu ####
mat_charc_flu_sev_dif_severe <- pandemy1 %>%
  filter(Grippe_severity3=="severe" | Grippe_severity3 =="not infected") %>%
  mutate(Grippe_severity3=droplevels(Grippe_severity3)) %>%
  mutate(Grippe_severity3 = forcats::fct_relevel(Grippe_severity3, "severe"))%>%
  select(age_mother, height,
         civil_status_cat2_mar,  
         parity_cat2_1, parity_cat2_2, parity_cat2_more2, Lausanne_NEU,  
         morphology_neit,morphology_ob, morphology_th,
         Goitre, rickets,
         Syphilis_2, 
         Mother_deceased, 
         hisco_class_3_2, hisco_class_3_1, hisco_class_3_3, hisco_class_3_mis,
         season_sp, season_su, season_au, season_wi, Grippe_severity3
  ) %>%  
  gtsummary::tbl_summary(by=Grippe_severity3,
                         statistic = list(
                           all_continuous ()~ "{mean} ({sd})",
                           all_categorical()  ~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  add_difference() %>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_flu_sev_dif_severe

mat_charc_flu_sev_dif_severe %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_charc_flu_sev_dif_severe.docx"))

## add dif: flu sympt missing vs no flu ####
mat_charc_flu_sev_dif_miss <- pandemy1 %>%
  filter(Grippe_severity3=="missing" | Grippe_severity3 =="not infected") %>%
  mutate(Grippe_severity3=droplevels(Grippe_severity3)) %>%
  mutate(Grippe_severity3 = forcats::fct_relevel(Grippe_severity3, "missing"))%>%
  select(age_mother, height,
         civil_status_cat2_mar,  
         parity_cat2_1, parity_cat2_2, parity_cat2_more2, Lausanne_NEU,  
         morphology_neit,morphology_ob, morphology_th,
         Goitre, rickets,
         Syphilis_2, 
         Mother_deceased, 
         hisco_class_3_2, hisco_class_3_1, hisco_class_3_3, hisco_class_3_mis,
         season_sp, season_su, season_au, season_wi, Grippe_severity3
  ) %>%  
  gtsummary::tbl_summary(by=Grippe_severity3,
                         statistic = list(
                           all_continuous() ~ "{mean} ({sd})",
                           all_categorical()~ "{n} ({p}%)"
                         ),
                         digits = list(
                           all_continuous() ~ 1,
                           all_dichotomous() ~ 0
                         ),
                         missing_text = "missing"
  )%>%
  add_difference() %>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_flu_sev_dif_miss

mat_charc_flu_sev_dif_miss %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data", "mat_charc_flu_sev_dif_miss.docx"))
