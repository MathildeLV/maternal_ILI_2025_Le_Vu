# If BW > or < 3 sd for each sex separately, depending on GA, then exclude?
laus1.1 <- laus1%>%
filter(birthyear<1925 & birthyear >1904)

# and exclude home births
laus1.2 <- laus1.1 %>%
  filter(!(Homebirth2 == "yes"))

# keep only GA >20 weeks and birthweight >500g 
laus1.3 <- laus1.2 %>%
  filter(!(GA_weeks_corrected < 22),
         !birthweight < 500)

# excl. extreme outliers for birthweight, placenta weight, birth length head circumference 
  laus2 <- laus1.3 %>%
    filter(((birthweight_Z_sex2< 7& birthweight_Z_sex2>(-7)) |is.na(birthweight_Z_sex2))  &
             ((placentaweight_Z_sex2< 7&placentaweight_Z_sex2>(-7)) |is.na(placentaweight_Z_sex2))  &
             ((head_circ_Z_sex2< 7&head_circ_Z_sex2>(-7)) |is.na(head_circ_Z_sex2))  &
             ((birthlength_Z_sex2< 7& birthlength_Z_sex2>(-7)) |is.na(birthlength_Z_sex2)) 
    )
# Exclude cases with both flu AND syphilis
#   laus4 <- laus2 %>%
#     filter(flu_or_syph != "flu and syphilis") %>%
#     mutate(flu_or_syph = droplevels(flu_or_syph))
#   
# # Exclude cases with both flu AND syphilis, LIVEBIRTHS only
#   laus4_livebirths <- laus4 %>%
#     filter(stillbirth=="livebirth") 

# Livebirths only
laus2_livebirth <- laus2 %>%
  filter(stillbirth=="livebirth")


  ## LABELS AND NAME OF CATEGORIES, FOR TABLES #### 
  lausgraph<- laus2
  attr(lausgraph$parity_cat2, 'levels')<- c("1","2", ">2")
  attr(lausgraph$Obese, 'levels')<- c("no", "yes")
  attr(lausgraph$Thin, 'levels')<- c("no", "yes")
  attr(lausgraph$Goitre, 'levels')<- c("no", "yes")
  attr(lausgraph$rickets, 'levels')<- c("no", "yes")
  attr(lausgraph$Grippe_during_pregn_3, 'levels')<- c("no", "yes")
  attr(lausgraph$Syphilis_2, 'levels')<- c("no", "yes")
  attr(lausgraph$stillbirth, 'levels')<- c("no", "yes")
  attr(lausgraph$postbirth_death, 'levels')<- c("no", "yes")
  attr(lausgraph$neonat_mort_day1, 'levels')<- c("no", "yes")
  attr(lausgraph$neonat_mort_d1_d5, 'levels')<- c("no", "yes")
  attr(lausgraph$PTB_corrected, 'levels')<- c("no", "yes")
  attr(lausgraph$LBW, 'levels')<- c("no", "yes")
  desired_order <- c("[10,33)", "[33,37)", "[37,41)", "[41,52]")  # Replace with your desired order
  lausgraph <- lausgraph %>%
    mutate(GA_weeks_cat_corrected2 = factor(GA_weeks_cat_corrected2, levels = desired_order))
  attr(lausgraph$GA_weeks_cat_corrected2, 'levels')<- c("<33","33-37", "37-41", ">41")
  
  
  my_labels <- c(age_mother = "maternal age (years)",  # Create labels
                 height= "height (cm)",
                 waist_circ="waist circumference (cm)",
                 parity_cat2 = "parity",
                 civil_status_cat2= "civil status",
                 Lausanne= "living in Lausanne",
                 Lausanne_NEU= "living in Lausanne",
                 hisco_class_3="HISCO class",
                 Obese="obese",
                 Thin="thin",
                 Goitre="goitre",
                 Infection="infection",
                 rickets="rickets",
                 birthweight="birth weight (g)", 
                 head_circ="head circumference (cm)",hc.perturbed="head circumference (cm)",
                 sex="sex",
                 GA_weeks_cat_corrected2="gestational age (weeks)",
                 GA_weeks_corrected="gestational age (weeks)",
                 stillbirth="stillbirth", postbirth_death="died after birth",
                 PTB_corrected="preterm birth (<37 weeks) ",LBW="low birth weight (<2'500g)",
                 babylength="birth length (cm)",
                 Grippe_during_pregn_3="flu during pregnancy",
                 Syphilis_2="syphilis",
                 morphology="morphology",
                 neonat_mort_d1_d5="neonatal mortality d1-5",
                 NEWTestDone="Wasserman test done",
                 Flu_in_pregn_and_in_pandemic="flu during pregnancy and pandemic",
                 Grippe_during_pregn_3="flu during pregnancy",
                 placentaweight="placenta weight (g)",
                 PI="ponderal index",
                 season="season",
                 Mother_deceased="maternal mortality"
  )
  label(lausgraph) <- as.list(my_labels[match(names(lausgraph), # Assign labels to data frame variables
                                              names(my_labels))])
  label(lausgraph)
  
  # Flu-specific subsets
  ## 1918-1920 pandemy
   pandemy <- lausgraph %>%
    filter((birthdate>= "1918-07-01" & month_1<"1919-04-01") |
             birthdate>="1920-01-01" & month_1<"1920-04-01")
   table(pandemy$Flu_in_pregn_and_in_pandemic, useNA = "always")
   
  # round(prop.table(table(pandemy$Grippe, useNA="always"))*100,2)
  # table(pandemy$Grippe_cat, useNA = "always")
  # round(prop.table(table(pandemy$Grippe_cat, useNA="always"))*100,2)
  # table(pandemy$Grippe_severity, useNA = "always")
  # round(prop.table(table(pandemy$Grippe_severity, useNA="always"))*100,2)
  # pandemy_PTB1 <- subset(pandemy, PTB1=="preterm")
  # pandemy_PTB2 <- subset(pandemy, PTB2=="preterm")
  # pandemy_term1 <- subset(pandemy, PTB1=="term")
  # pandemy_term2 <- subset(pandemy, PTB2=="term")
  pandemy_livebirths <- subset(pandemy, stillbirth=="no")
  
  
  ## for flu_cases only, during the pandemic
  # flu_cases_pandemy <- subset(pandemy, Grippe=="yes")
  # table(flu_cases_pandemy$Grippe, useNA = "always")
  # table(flu_cases_pandemy$Grippe_cat, useNA = "always")
  # round(prop.table(table(flu_cases_pandemy$Grippe_cat, useNA="always"))*100,2)
  # table(flu_cases_pandemy$Grippe_severity, useNA = "always")
  # round(prop.table(table(flu_cases_pandemy$Grippe_severity, useNA="always"))*100,2)
  
  flu_cases_during_pregn_pandemy_sev <- subset(pandemy, Grippe_severity3!="missing")  %>%
    mutate(Grippe_severity3 = droplevels(Grippe_severity3))
  
  flu_cases_during_pregn_pandemy_sev_livebirths <- flu_cases_during_pregn_pandemy_sev %>%
    filter(stillbirth =="no")
  
  flu_cases_during_pregn_pandemy_trimester <- pandemy %>%
    filter(trimester_real_def3!="unknown") %>%
    mutate(trimester_real_def3=droplevels(trimester_real_def3))
  
  flu_cases_during_pregn_pandemy_trimester_livebirths <- flu_cases_during_pregn_pandemy_trimester %>%
    filter(stillbirth =="no")
  flu_cases_during_pregn_pandemy <- pandemy %>%
    filter(Flu_in_pregn_and_in_pandemic == "yes")

  