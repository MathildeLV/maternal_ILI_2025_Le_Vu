# Severity ####
## Males ####
### All severity ####
tab_neonat_flu_sev_M <-  pandemy %>%
  filter(sex=="Male") %>%
  select(Grippe_severity3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected, 
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=Grippe_severity3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected", 
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_sev_M

tab_neonat_flu_sev_M %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonat_flu_sev_M.docx"))


### Mild vs not infected (to have pval) ####
tab_neonat_flu_sev_M_mild <-  pandemy %>%
  filter(sex=="Male" & (Grippe_severity3=="not infected" | Grippe_severity3 =="mild")) %>%
  mutate(Grippe_severity3=droplevels(Grippe_severity3)) %>%
  select(Grippe_severity3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=Grippe_severity3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected", 
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p()%>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_sev_M_mild

tab_neonat_flu_sev_M_mild %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonat_flu_sev_M_mild.docx"))

### Severe vs not infected (to have pval) ####
tab_neonat_flu_sev_M_severe <-  pandemy %>%
  filter(sex=="Male" & (Grippe_severity3=="not infected" | Grippe_severity3 =="severe")) %>%
  mutate(Grippe_severity3=droplevels(Grippe_severity3)) %>%
  select(Grippe_severity3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected, 
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=Grippe_severity3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected", 
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p()%>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_sev_M_severe

tab_neonat_flu_sev_M_severe %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonat_flu_sev_M_severe.docx"))


## Females ####
### All severity ####
tab_neonat_flu_sev_F <-  pandemy %>%
  filter(sex=="Female") %>%
  select(Grippe_severity3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=Grippe_severity3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_sev_F

tab_neonat_flu_sev_F %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonat_flu_sev_F.docx"))

### Mild vs not infected (to have pval) ####
tab_neonat_flu_sev_F_mild <-  pandemy %>%
  filter(sex=="Female" & (Grippe_severity3=="not infected" | Grippe_severity3 =="mild")) %>%
  mutate(Grippe_severity3=droplevels(Grippe_severity3)) %>%
  select(Grippe_severity3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=Grippe_severity3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p()%>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_sev_F_mild

tab_neonat_flu_sev_F_mild %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonat_flu_sev_F_mild.docx"))

### Severe vs not infected (to have pval) ####
tab_neonat_flu_sev_F_severe <-  pandemy %>%
  filter(sex=="Female" & (Grippe_severity3=="not infected" | Grippe_severity3 =="severe")) %>%
  mutate(Grippe_severity3=droplevels(Grippe_severity3)) %>%
  select(Grippe_severity3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=Grippe_severity3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p()%>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_sev_F_severe

tab_neonat_flu_sev_F_severe %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonat_flu_sev_F_severe.docx"))


# Trimester of infection #### 
## Males ####
### All trim ####
tab_neonat_flu_trim_M <-  pandemy %>%
  filter(sex=="Male") %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI, GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_trim_M

tab_neonat_flu_trim_M %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonatal_flu_timing_trim_M.docx"))

### T1 ####
tab_neonat_flu_trim_M_T1 <-  pandemy %>%
  filter(sex=="Male" & 
           (trimester_real_def3 =="not infected" | trimester_real_def3=="first")) %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p() %>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_trim_M_T1

tab_neonat_flu_trim_M_T1 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonatal_flu_timing_trim_M_T1.docx"))

### T2 ####
tab_neonat_flu_trim_M_T2 <-  pandemy %>%
  filter(sex=="Male" & 
           (trimester_real_def3 =="not infected" | trimester_real_def3=="second")) %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p() %>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_trim_M_T2

tab_neonat_flu_trim_M_T2 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", 
              "tab_neonatal_flu_timing_trim_M_T2.docx"))

### T3 ####
tab_neonat_flu_trim_M_T3 <-  pandemy %>%
  filter(sex=="Male" & 
           (trimester_real_def3 =="not infected" | trimester_real_def3=="third")) %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p() %>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_trim_M_T3

tab_neonat_flu_trim_M_T3 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonatal_flu_timing_trim_M_T3.docx"))


## Females ####
### All trim ####
tab_neonat_flu_trim_F <-  pandemy %>%
  filter(sex=="Female") %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_trim_F

tab_neonat_flu_trim_F %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", "tab_neonatal_flu_timing_trim_F.docx"))


### T1 ####
tab_neonat_flu_trim_F_T1 <-  pandemy %>%
  filter(sex=="Female" & 
           (trimester_real_def3 =="not infected" | trimester_real_def3=="first")) %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p() %>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_trim_F_T1

tab_neonat_flu_trim_F_T1 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification",
              "tab_neonatal_flu_timing_trim_F_T1.docx"))

### T2 ####
tab_neonat_flu_trim_F_T2 <-  pandemy %>%
  filter(sex=="Female" & 
           (trimester_real_def3 =="not infected" | trimester_real_def3=="second")) %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p() %>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_trim_F_T2

tab_neonat_flu_trim_F_T2 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification", 
              "tab_neonatal_flu_timing_trim_F_T2.docx"))

### T3 ####
tab_neonat_flu_trim_F_T3 <-  pandemy %>%
  filter(sex=="Female" & 
           (trimester_real_def3 =="not infected" | trimester_real_def3=="third")) %>%
  mutate(trimester_real_def3=droplevels(trimester_real_def3)) %>%
  select(trimester_real_def3,birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    by=trimester_real_def3,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing = "no"# i dont show the missing
  )%>%
  add_p() %>%
  gtsummary::modify_header(label ~ "**Variable**") 
tab_neonat_flu_trim_F_T3

tab_neonat_flu_trim_F_T3 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/sex_stratification",
              "tab_neonatal_flu_timing_trim_F_T3.docx"))

