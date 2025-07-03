# abortions characteristics  ####
## by year ####
abortions_charc_yr_1909_21 <- abortions1_graph %>%
  select(age_mother, civil_status_cat2, Lausanne,  
         Spontane, parity_cat2, hisco_class_3,
         GA_weeks, Syphilis_2, Grippe_during_pregn_3, birthyear) %>%  
  gtsummary::tbl_summary(
    by=birthyear,
    statistic = list(
      c("age_mother", "GA_weeks") ~ "{mean} ({sd})",
      c("civil_status_cat2", "parity_cat2",'Lausanne', 'hisco_class_3',
        'Spontane', 'Syphilis_2',
        'Grippe_during_pregn_3') ~ "{n} ({p}%)"
    ),
    digits = list(
      c("civil_status_cat2","parity_cat2",'Lausanne', 'hisco_class_3',
        'Spontane', 'Syphilis_2', 
        'Grippe_during_pregn_3') ~ 0,
      c("age_mother", "GA_weeks") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")
abortions_charc_yr_1909_21

abortions_charc_yr_1909_21 %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/abortions", "abortions_charc_yr_1909_21.docx"))

# comparing abortions vs not abortions ####
## pandemic years ####
abortions_not_abortions_charc_pandemic <- abortions_not_abortions_graph%>%
  mutate(
    abortion = forcats::fct_relevel(abortion, "yes"),  
    Grippe_during_pregn_3 = as.numeric(Grippe_during_pregn_3== "yes"),  
    civil_status_cat2_mar = as.numeric(civil_status_cat2=="married"),  
    parity_cat2_1 = as.numeric(parity_cat2=="1"),  
    parity_cat2_2 = as.numeric(parity_cat2=="2"),  
    parity_cat2_more2 = as.numeric(parity_cat2==">2"),  
    
    hisco_class_3_2= as.numeric(hisco_class_3=="2"),  
    hisco_class_3_1= as.numeric(hisco_class_3=="1"),  
    hisco_class_3_3= as.numeric(hisco_class_3=="3"),  
    hisco_class_3_mis= as.numeric(hisco_class_3=="missing"),  
   
    Syphilis_2 = as.numeric(Syphilis_2 == "yes"),
    
    Lausanne = as.numeric(Lausanne == "yes")) %>%
    filter(birthyear==1918 | birthyear == 1919 | birthyear ==1920 ) %>%
select(abortion,age_mother, civil_status_cat2_mar,
       parity_cat2_1,parity_cat2_1, parity_cat2_2, parity_cat2_more2, 
       Lausanne, 
         GA_weeks, Syphilis_2, Grippe_during_pregn_3, 
       hisco_class_3_2, hisco_class_3_1, hisco_class_3_3, hisco_class_3_mis) %>% 
  gtsummary::tbl_summary(
    by=abortion,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_dichotomous() ~ 0),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")%>%
  add_difference()
abortions_not_abortions_charc_pandemic

abortions_not_abortions_charc_pandemic %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/abortions", "abortions_not_abortions_charc_pandemic_diff.docx"))

