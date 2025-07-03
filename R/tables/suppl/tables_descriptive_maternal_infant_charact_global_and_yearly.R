# maternal characteristics  #### 
## overall, simplified, all years together ####
mat_charc <- lausgraph %>%
  select(age_mother, height,
         civil_status_cat2, parity_cat2, Lausanne_NEU,  
         morphology, Goitre, rickets,
         Flu_in_pregn_and_in_pandemic, Syphilis_2,
         Mother_deceased, hisco_class_3, season) %>%  
  gtsummary::tbl_summary(
    statistic = list(
      c("age_mother", "height") ~ "{mean} ({sd})",
      c("civil_status_cat2", "parity_cat2",'Lausanne_NEU',
        'morphology', 'Goitre', 'rickets',
        'Flu_in_pregn_and_in_pandemic', 'Syphilis_2',
        'Mother_deceased', 'hisco_class_3', 'season') ~ "{n} ({p}%)"
    ),
    digits = list(
      c("civil_status_cat2","parity_cat2",'Lausanne_NEU',
        'morphology', 'Goitre', 'rickets', 
        'Flu_in_pregn_and_in_pandemic', 'Syphilis_2',
        'Mother_deceased','hisco_class_3', 'season') ~ 0,
      c("age_mother", "height") ~ 1),
    # label = grade ~ "Tumor Grade",
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") 
mat_charc

mat_charc %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/description_of_the_population", "maternal_characteristics_overall_1905_24.docx"))


## during the pandemic only ####
mat_charc_pand <- pandemy %>%
  select(age_mother, height,
         civil_status_cat2, parity_cat2, Lausanne_NEU,  
         morphology, Goitre, rickets,
         Flu_in_pregn_and_in_pandemic, Syphilis_2,
         Mother_deceased, hisco_class_3, season) %>%  
  gtsummary::tbl_summary(
    statistic = list(
      c("age_mother", "height") ~ "{mean} ({sd})",
      c("civil_status_cat2", "parity_cat2",'Lausanne_NEU',
        'morphology', 'Goitre', 'rickets',
        'Flu_in_pregn_and_in_pandemic', 'Syphilis_2',
        'Mother_deceased', 'hisco_class_3', 'season') ~ "{n} ({p}%)"
    ),
    digits = list(
      c("civil_status_cat2","parity_cat2",'Lausanne_NEU',
        'morphology', 'Goitre', 'rickets', 
        'Flu_in_pregn_and_in_pandemic', 'Syphilis_2',
        'Mother_deceased','hisco_class_3', 'season') ~ 0,
      c("age_mother", "height") ~ 1),
    # label = grade ~ "Tumor Grade",
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**") 
mat_charc_pand

mat_charc_pand %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/description_of_the_population", "maternal_characteristics_mat_charc_pand.docx"))

## by year ####
mat_charc_by_year <- lausgraph %>%
  select(age_mother, height,
         civil_status_cat2, parity_cat2, Lausanne_NEU,  
         morphology, Goitre, rickets, 
         Flu_in_pregn_and_in_pandemic, Syphilis_2,
         Mother_deceased,hisco_class_3,
         birthyear, season) %>%  
  gtsummary::tbl_summary(
    by=birthyear,
    statistic = list(
      c("age_mother", "height") ~ "{mean} ({sd})",
      c("civil_status_cat2", "parity_cat2",'Lausanne_NEU',
        'morphology', 'Goitre', 'rickets', 
        'Flu_in_pregn_and_in_pandemic', 'Syphilis_2',
        'Mother_deceased',
        'hisco_class_3' ,'season') ~ "{n} ({p}%)"
    ),
    digits = list(
      c("civil_status_cat2","parity_cat2",'Lausanne_NEU',
        'morphology', 'Goitre', 'rickets', 
        'Flu_in_pregn_and_in_pandemic','Syphilis_2',
        'Mother_deceased',
        'hisco_class_3','season') ~ 0,
      c("age_mother", "height") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")
mat_charc_by_year

mat_charc_by_year %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/description_of_the_population", "maternal_characteristics_by_year_1905_24.docx"))


#  neonatal characteristics  #### 
## overall, simplified, all years together ####
neonat_charc <- lausgraph %>%
  select(birthweight,head_circ,placentaweight, babylength, PI, GA_weeks_corrected,
         sex,LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
         ) %>%  
  gtsummary::tbl_summary(
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("sex","LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("sex","LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")
neonat_charc

neonat_charc %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/description_of_the_population", "neonatal_characteristics_overall_1905_24.docx"))

## during the pandemic only ####
neonat_charc_pand <- pandemy %>%
  select(birthweight,head_circ,placentaweight, babylength, PI, GA_weeks_corrected,
         sex,LBW,
         PTB_corrected,
         stillbirth, neonat_mort_d1_d5
  ) %>%  
  gtsummary::tbl_summary(
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("sex","LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("sex","LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")
neonat_charc_pand

neonat_charc_pand %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/description_of_the_population", "neonatal_characteristics_pandemy.docx"))


## by year ####
neonat_charc_by_year <- lausgraph %>%
  select(birthweight,head_circ,placentaweight, 
         babylength,PI,GA_weeks_corrected,
         sex,LBW,
         PTB_corrected, 
         stillbirth, neonat_mort_d1_d5,birthyear
  ) %>%  
  gtsummary::tbl_summary(
    by=birthyear,
    statistic = list(
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ "{mean} ({sd})",
      c("sex","LBW","PTB_corrected", 
        "stillbirth", "neonat_mort_d1_d5") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("sex","LBW","PTB_corrected",
        "stillbirth", "neonat_mort_d1_d5") ~ 0,
      c("birthweight", "head_circ", "placentaweight", 
        "babylength", "PI", "GA_weeks_corrected") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")
neonat_charc_by_year

neonat_charc_by_year %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables/suppl_data/description_of_the_population", "neonatal_characteristics_by_year_1905_1924.docx"))

