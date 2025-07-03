#Formatting variables
# Put variables in the correct format
laus1 <- laus %>%
  mutate(birthdate=as.Date(birthdate, format = "%Y-%m-%d", useNA= "always"),
        lastperiod=as.Date(lastperiod, format = "%Y-%m-%d", useNA= "always"),
        birthyear=as.numeric(birthyear),
        birthmonth=as.numeric(birthmonth),
        birthday=format(birthdate, format="%d"),
        season=case_when(birthmonth>=3 & birthmonth<6 ~"spring",
                          birthmonth>=6 & birthmonth<9 ~"summer",
                          birthmonth>=9 & birthmonth <12 ~"autumn",
                          birthmonth==12 | birthmonth <3 ~"winter"),
        Week_1st_day_test=floor_date(birthdate, unit="week", week_start=7),
        week= week(Week_1st_day_test),       
        Jahr_Week_1st_day_test=format(Week_1st_day_test, format="%Y"),
        birth_month_year=paste0(birthyear, "-", birthmonth, "-", "01"),
        birth_month_year=as.Date(birth_month_year, format="%Y-%m-%d"),
        Grippe_short_date_year=year(Grippe_short_date),
        Grippe_short_date_month=month(Grippe_short_date),
        Grippe_short_date_year_month=paste0(Grippe_short_date_year, "-", Grippe_short_date_month, "-", "01"),
        Grippe_short_date_year_month=as.Date(Grippe_short_date_year_month, format="%Y-%m-%d"),
        Namex= paste0(Jahr_Week_1st_day_test,"/W",week),
          across(c(civil_status,Religion,Lausanne,
                   Lausanne_NEU,
                  sex,stillbirth, postbirth_death, Mother_deceased,
                  feeding, Obese, Thin, Goitre, Infection,
                  Etat_general_cat,rickets,eclampsia,
                  hisco_class_12, hisco_class_3,
                  LBW, age_baby_cat, GA_weeks_cat, GA_weeks_cat_corrected,
                  PTB_corrected, post_term_corrected,
                  Grippe, Grippe_cat, Grippe_severity,
                  Syphilis, Gonorrhoea,season), as.factor),
      # PTB_corrected: based on both GA var
        Obese=replace(Obese, (is.na(Obese)), 0),
        Infection=replace(Infection, (is.na(Infection)), 0),
        Thin=replace(Thin, (is.na(Thin)), 0),
        Goitre=replace(Goitre, (is.na(Goitre)), 0),
       Mother_deceased=replace(Mother_deceased, (is.na(Mother_deceased)), "no"),
      Lausanne_NEU=replace(Lausanne_NEU, (Lausanne_NEU=="unsure"), "no"),
      ) 

laus1$civil_status <-  relevel(laus1$civil_status, ref=3)
laus1$Lausanne <-  relevel(laus1$Lausanne, ref="yes")
laus1$Lausanne_NEU <- droplevels(laus1$Lausanne_NEU[laus1$Lausanne_NEU != "unsure"])
laus1$Lausanne_NEU <-  relevel(laus1$Lausanne_NEU, ref="yes")
laus1$feeding <-  relevel(laus1$feeding, ref=3)
laus1$age_baby_cat <-  relevel(laus1$age_baby_cat, ref=3)
laus1$LBW <-  relevel(laus1$LBW, ref=1)
laus1$GA_weeks_cat <-  relevel(laus1$GA_weeks_cat, ref=7)
laus1$GA_weeks_cat_corrected <-  relevel(laus1$GA_weeks_cat_corrected, ref=6)
laus1$hisco_class_3 <-  relevel(laus1$hisco_class_3, ref=2)
laus1$birthyear1 <-  relevel(as.factor(laus1$birthyear), ref="1913") #to be updated
desired_order <- c("spring", "summer", "autumn", "winter")
laus1  <- laus1  %>%
  mutate(season = factor(season, levels = desired_order))


laus1 <- laus1 %>%
  mutate(civil_status_cat2 = recode (civil_status, 
                           "divorcee"= "single or missing",
                           "veuve"= "single or missing",
                           "celibataire"= "single or missing",
                            "mariee" = "married"), # pooling divorcee veuve as single
         civil_status_cat2=replace(civil_status_cat2, (is.na(civil_status_cat2)),"single or missing"),
         parity_cat2 = cut(parity, breaks=c(0,1, 2, 20)), # creating parity cat 2: 1, 2, >2 parities 
         agemother_cat2 = cut(age_mother, breaks=c(12,20,25,30,35,50)), 
         Religion2 = recode (Religion, "catholic"= "catholic",
                                   "protestant"= "protestant",
                                   "other"= "other or missing",
                                   "missing" = "other or missing"),
         feeding2 = recode(feeding, "maternal"="maternal",
                              "artifical"="artificial",
                              "human milk" ="other",
                              "maternal then artificial"= "maternal then other",
                              "maternal then mixed"= "maternal then other",
                              "maternal then mixed then artificial"= "maternal then other",
                              "mixed"="mixed",
                              "other"="other"),
          Homebirth2=recode(Homebirth, "yes" = "yes", "no" = "no", "0"="no"),
          Homebirth2=replace(Homebirth2, (is.na(Homebirth)), "no"),
          morphology=case_when(Thin==1 ~"thin",
                              Obese==1 ~"obese",
                              (Thin==0 & Obese==0 ~"neither")),
          Syphilis_2=case_when(
          Syphilis=="positive" | Syphilis=="yes" ~"positive",
          Syphilis=="negative" | Syphilis == "no" | Syphilis=="douteux"~"no"),
          GA_weeks_cat2= cut(as.numeric(GA_weeks), 
                           breaks=c(10, 33, 37, 41, 52), include.lowest = TRUE,  right = FALSE),
          GA_weeks_cat_corrected2= cut(as.numeric(GA_weeks_corrected), 
                           breaks=c(10, 33, 37, 41, 52), include.lowest = TRUE,  right = FALSE),
         GA_days_corrected=GA_weeks_corrected*7,
         pregnancy_start = birthdate - GA_days_corrected,
        across(c(civil_status_cat2,parity_cat2, agemother_cat2,Religion2, Homebirth2,
                 morphology,Syphilis_2,neonat_mort_d1_d5,neonat_mort_day1), as.factor))%>%
  mutate(PI=(100*birthweight)/(babylength^3))

laus1$agemother_cat2 <-  relevel(laus1$agemother_cat2, ref=2)
laus1$GA_weeks_cat2 <-  relevel(laus1$GA_weeks_cat2, ref=3)
laus1$GA_weeks_cat_corrected2 <-  relevel(laus1$GA_weeks_cat_corrected2, ref=3)



table(laus1$postbirth_death, useNA = "a")
table(laus1$day_of_death, useNA = "a") 
# day of death: counting from day of delivery= day1
# missing (NA) means: we know the neonate died after birth but we do not know on which day
# 0 means: the neonate did not die (i.e. not concerned)

#date
laus1$birthdate1 <- as.factor(laus1$birthdate)
laus1$birthdate_num <- as.numeric(laus1$birthdate1)
table(laus1$birthdate_num)
#extract week
 laus1$birthweek <- format(as.Date(laus1$birthdate), "%Y-%V")

laus1 <- laus1 %>%
  mutate(GA_weeks_corr_rounded=round(GA_weeks_corrected, digits=0))

## Z transformed variables
###  anthropometric Z-scores
laus1 <- laus1 %>%
  mutate(Birthweight_Z =(birthweight - mean(birthweight, na.rm=TRUE)) / sd(birthweight, na.rm=TRUE),
         Placentaweight_Z =(placentaweight - mean(placentaweight, na.rm=TRUE)) / sd(placentaweight, na.rm=TRUE),
         Head_circ_Z =(head_circ - mean(head_circ, na.rm=TRUE)) / sd(head_circ, na.rm=TRUE),
         Birthlength_Z =(babylength - mean(babylength, na.rm=TRUE)) / sd(babylength, na.rm=TRUE)) 

### sex-related anthropometric Z-scores
  laus1 <- laus1 %>%
    group_by(sex) %>%
    mutate(birthweight_Z_sex2 =(birthweight - mean(birthweight, na.rm=TRUE)) / sd(birthweight, na.rm=TRUE),
           head_circ_Z_sex2 =(head_circ - mean(head_circ, na.rm=TRUE)) / sd(head_circ, na.rm=TRUE),
           placentaweight_Z_sex2 =(placentaweight - mean(placentaweight, na.rm=TRUE)) / sd(placentaweight, na.rm=TRUE),
           birthlength_Z_sex2 =(babylength - mean(babylength, na.rm=TRUE)) / sd(babylength, na.rm=TRUE),
           ponderal_index_Z_sex2 =(PI - mean(PI, na.rm=TRUE)) / sd(PI, na.rm=TRUE)) %>%
    ungroup()  

#renaming some categories
attr(laus1$PTB_corrected, "levels")<- c('term', 'preterm')
attr(laus1$LBW, "levels")<- c('normal BW', 'LBW')
attr(laus1$sex, 'levels')<- c("Male", "Female")
attr(laus1$stillbirth, 'levels')<- c("livebirth", "stillbirth")
attr(laus1$Infection, "levels")<- c('Not Infected', 'Infected')

laus1$birthdate_num_yr<- 
  laus1$birthdate_num/365.25

# ADDING time variable by month
laus1$birthdate_Y_M <- with(laus1, sprintf("%d-%02d", birthyear, birthmonth))
table(laus1$birthdate_Y_M, useNA = "a")
table(laus1$birthdate_Y_M, laus1$birthmonth)
laus1$birthdate_Y_M_num <- as.factor(laus1$birthdate_Y_M)
laus1$birthdate_Y_M_num <- as.numeric(laus1$birthdate_Y_M_num)
table(laus1$birthdate_Y_M_num, useNA = "a")

# Add trimester begin and end date
laus1 <- laus1 %>%
  mutate(end_T1 = (pregnancy_start + weeks(12)),
         beginning_T2= (pregnancy_start + weeks(12)+days(1)),
         end_T2=(pregnancy_start + weeks(26)),
         end_T2_truncated=(pregnancy_start + weeks(22)),
         beginning_T3=(pregnancy_start + weeks(26)+days(1)),
         end_T3=(birthdate))
t <- laus1 %>%
  select(birthdate, GA_weeks_corrected, pregnancy_start, end_T1, beginning_T2, end_T2, beginning_T3, end_T3)

