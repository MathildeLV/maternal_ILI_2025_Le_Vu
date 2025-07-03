abortions1 <- abortions %>%
  mutate(date_event =as.Date(date_event, format = "%d.%m.%Y", useNA= "always"),
         lastperiod=as.Date(lastperiod, format="%d.%m.%Y", useNA="always"),
         YearMonth = format(date_event, "%Y-%m"),
         parity_cat2=cut(parity, breaks = c(0,1,2,20)),
         civil_status_cat2=case_when(civil_status=="mariee"~"married",
                                     civil_status=="divorcee" | civil_status=="celibat" | 
                                       civil_status=="separee" | civil_status=="veuve" ~ "single or missing"),
         Syphilis_2=case_when((syphilis=="y" | syphilis=="Y")~ "positive",
                              TRUE ~"no"),
         Grippe_during_pregn_3=case_when((flu=="y" | flu=="Y")~ "yes",
                                         TRUE ~"no"),
         Lausanne=case_when((Lausanne=="y" | Lausanne=="Y") ~ "yes",
                            Lausanne=="N" ~"no",
                            TRUE ~"unsure"),
         Spontane=case_when(Spontane=="Y"~"yes",
                            TRUE ~"no or unsure"),
         GA_weeks=as.numeric(difftime(date_event, lastperiod, units="weeks")),
         across(c(civil_status_cat2, Lausanne, Spontane,birthyear,Syphilis_2, Grippe_during_pregn_3), 
                as.factor),
         birthyear=as.factor(birthyear),
         abortion="yes",
         quarter=quarter(date_event,type = "year.quarter")
  ) %>%
  filter(birthyear!=1922,
         date_event!=is.na(NA))



my_labels <- c(age_mother = "maternal age (years)",  # Create labels
               parity_cat2 = "parity",
               civil_status_cat2= "civil status",
               Lausanne= "living in Lausanne",
               hisco_class_3="HISCO class",
               GA_weeks= "gestational age (weeks)" ,
               Grippe_during_pregn_3="flu during pregnancy",
               Syphilis_2="syphilis"
)



abortions1_graph <- abortions1
attr(abortions1_graph$parity_cat2, 'levels')<- c("1","2", ">2")
attr(abortions1_graph$Grippe_during_pregn_3, 'levels')<- c("no", "yes")
attr(abortions1_graph$Syphilis_2, 'levels')<- c("no", "yes")
label(abortions1_graph) <- as.list(my_labels[match(names(abortions1_graph), # Assign labels to data frame variables
                                                   names(my_labels))])


# compare abortions with the main data ####
laus2_prep <- laus2 %>%
  filter(birthyear<1922 & birthyear>1908) %>%
  mutate(birthyear=as.factor(birthyear),
         quarter=quarter(birthdate,type = "year.quarter")) %>%
  select("age_mother", "GA_weeks","civil_status_cat2", "parity_cat2",'Lausanne',
         'Syphilis_2','Grippe_during_pregn_3', 
         'birthyear',  "hisco_class_3", 'quarter')
abortions1 <- abortions1%>%
  select("age_mother", "GA_weeks","civil_status_cat2", "parity_cat2",'Lausanne',
         'Syphilis_2','Grippe_during_pregn_3', 
         'birthyear',  "hisco_class_3","abortion", 'quarter')
# merge the two datasets
abortions_not_abortions <- bind_rows(laus2_prep, abortions1)
abortions_not_abortions <- abortions_not_abortions%>%
  mutate(abortion=coalesce(abortion, "no"),
         abortion=as.factor(abortion))
attr(abortions_not_abortions$parity_cat2, 'levels')<- c("1","2", ">2")
attr(abortions_not_abortions$Grippe_during_pregn_3, 'levels')<- c("no", "yes")
attr(abortions_not_abortions$Syphilis_2, 'levels')<- c("no", "yes")


abortions_not_abortions_graph <- abortions_not_abortions
label(abortions_not_abortions_graph) <- as.list(my_labels[match(names(abortions_not_abortions_graph), # Assign labels to data frame variables
                                                   names(my_labels))])


a <- abortions1 %>%
  filter((GA_weeks)>=22)

