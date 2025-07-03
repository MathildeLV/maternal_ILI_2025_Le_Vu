# Recoding Flu cases, categories and symptoms

# Grippe_during_pregn_2 and Grippe_during_pregn_3 : for flu cases during pregnancy, not necessarily during the pandemic
# Grippe_during_pregn_2: category missing means that the mother had the flu but we dont know when
# Grippe_during_pregn_3: the "missing category" are pooled with the "no". I.e. if we are not sure whether the infection took place during pregancy, 
# we consider it was not the case )= more conservative)
# Flu_in_pregn_and_in_pandemic: infection during pregnancy (based on Grippe_during_pregn_3) and during the pandemic (based on date of delivery)
# flu_or_syph: infection by either flu or syphilis or both (during pregnancy), and for flu also during the pandemic
laus1 <- laus1 %>%
  mutate(Grippe_during_pregn_2=case_when(Grippe_cat=="B" |Grippe_cat=="C" ~"yes",
                                             Grippe_cat=="A" | Grippe_cat=="D"  ~ "no",
                                             Grippe_cat=="NULL" ~"no",
                                             is.na(Grippe_cat) ~ "missing"),

          Grippe_during_pregn_3=case_when(Grippe_cat=="B" |Grippe_cat=="C" ~"yes",
                                          Grippe_cat=="A" | Grippe_cat=="D"  ~ "no",
                                          Grippe_cat=="NULL" ~"no",
                                          is.na(Grippe_cat) ~ "no"),

         Flu_in_pregn_and_in_pandemic=
           ifelse(Grippe_during_pregn_3 == "yes" &
                    ((birthdate>= "1918-07-01" & month_1<"1919-04-01")| 
                        (birthdate>="1920-01-01" & month_1<"1920-04-01") &
                       (is.na(Grippe_short_date) | 
                        (Grippe_short_date>= "1918-07-01" & Grippe_short_date<="1919-03-31") |
                          Grippe_short_date>= "1920-01-01" & Grippe_short_date<"1920-04-01")), "yes", "no"),
         mutate(across(c(Flu_in_pregn_and_in_pandemic,  
                         Grippe_during_pregn_3, Grippe_during_pregn_2), as.factor)))

table(laus1$Grippe_during_pregn_2, useNA = "a")
laus1$Grippe_during_pregn_2 <-  relevel(laus1$Grippe_during_pregn_2, ref=2)
table(laus1$Grippe_during_pregn_3, useNA = "a")
table(laus1$Flu_in_pregn_and_in_pandemic, useNA = "a")

# Severity (Grippe_severity3) and trimester (trimester2) variables: only for cases with flu cases during pregnancy and during pandemic!!!
# Grippe_severity3group cases of bronchitis+pneumonia+bronchopneumonia together
# trimester 2: consider only infection in first/last/middle , not first+last or middle+last, etc.
laus1 <- laus1 %>%
  mutate(Grippe_severity3=case_when(Flu_in_pregn_and_in_pandemic =="yes" &
                                      (Grippe_severity=="bronchopneumonia"|Grippe_severity=="pneumonia" |  Grippe_severity=="bronchitis")
                                        ~"severe",
                                    Flu_in_pregn_and_in_pandemic =="yes" &
                                    Grippe_severity=="mild" ~"mild",
                                    Flu_in_pregn_and_in_pandemic =="yes" &
                                    is.na(Grippe_severity) ~"missing",
                                    TRUE ~"not infected"),
          trimester2=case_when(Flu_in_pregn_and_in_pandemic =="yes" &
                                 trimester=="first" ~"first", 
                               Flu_in_pregn_and_in_pandemic =="yes" &
                                 trimester=="last" ~"last",
                               Flu_in_pregn_and_in_pandemic =="yes" &
                                 trimester=="middle"~"middle"),
         trimester_real_def2=case_when(Flu_in_pregn_and_in_pandemic =="yes" &
                                trimester_real_def=="first" ~"first", 
                              Flu_in_pregn_and_in_pandemic =="yes" &
                                trimester_real_def=="second" ~"second",
                              Flu_in_pregn_and_in_pandemic =="yes" &
                                trimester_real_def=="third"~"third"),
         half2=case_when(Flu_in_pregn_and_in_pandemic=="yes" &
                           half=="first" ~ "first half",
                         Flu_in_pregn_and_in_pandemic=="yes" &
                           half=="last" ~"second half"),
         mutate(across(c(Grippe_severity3, trimester, trimester2, half, half2,trimester_real_def2), as.factor)))
table(laus1$Grippe_severity3)
table(laus1$Grippe_severity3, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")
table(laus1$trimester, useNA = "a")
table(laus1$trimester, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")
table(laus1$trimester2, useNA = "a")
table(laus1$trimester2, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")
table(laus1$trimester,laus1$Grippe_cat, useNA = "always")
table(laus1$half, useNA = "always")
table(laus1$half2, useNA = "always")
table(laus1$trimester_real_def, useNA = "a")
table(laus1$trimester_real_def2, useNA="a")
# Define the order based on 'location'
order <- c(3, 1, 4, 2)
# Reorder 'Grippe_severity3' based on 'location'
laus1$Grippe_severity3 <- factor(laus1$Grippe_severity3, levels = levels(laus1$Grippe_severity3)[order])
table(laus1$Grippe_severity3)

# Mannually change severity for some specific entries
laus1 <- laus1 %>%
   mutate(Grippe_severity3=replace(Grippe_severity3,
                                   (Grippe_com=="en octobre, avec complications pulmonaires. 3sem de lit"|
                                      Grippe_com=="aout, 17 jours de traitement a l hopital" |
                                      Grippe_com=="grippe tres forte avec epistaxis, fin septembre"|
                                      Grippe_com=="01 au 18 novembre pleuresie seche"),
                                   "severe"))
 
 table(laus1$Grippe_severity3, useNA = "always")

#For Flu category=C (infected when arriving at the maternity),
 # put trimester2=last
# if the mother was infected (Grippe_during_pregn_2= yes or =missing) and
 # trimester2=NA, then create new category "unknown"
 table(laus1$trimester2, useNA = "always")
laus1 <- laus1 %>%
   mutate(trimester3= case_when(trimester2=="first" ~"first",
                                trimester2=="last" ~"last",
                                trimester2=="middle" ~"middle",
                                (Grippe_cat=="C" & Flu_in_pregn_and_in_pandemic =="yes")~ "last",
                              is.na(trimester2) & Flu_in_pregn_and_in_pandemic=="yes"~ "unknown",
                              TRUE~"not infected"),
            trimester_real_def3=case_when(trimester_real_def2=="first"~"first",
                                          trimester_real_def2=="second"~"second",
                                          trimester_real_def2=="third"~"third",
                                          (Grippe_cat=="C" & Flu_in_pregn_and_in_pandemic=="yes") ~"third",
                                          is.na(trimester_real_def2) & Flu_in_pregn_and_in_pandemic=="yes"~ "unknown",
                                          TRUE ~"not infected"),
          half3=case_when(half2=="first half" ~ "first half",
                          half2=="second half" ~ "second half",
                          (Grippe_cat=="C" & Flu_in_pregn_and_in_pandemic =="yes")~ "second half",
                          is.na(half2) & Flu_in_pregn_and_in_pandemic=="yes" ~ "unknown",
                          TRUE ~ "not infected"),
          across(c(trimester3, trimester_real_def3, half3), as.factor))
table(laus1$trimester2, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")
table(laus1$trimester3, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")
table(laus1$trimester3, laus1$Grippe_cat, useNA = "always")
table(laus1$trimester_real_def, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")
table(laus1$trimester_real_def2, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")
table(laus1$trimester_real_def3, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")

table(laus1$half2, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")
table(laus1$half3, useNA = "a")
table(laus1$half3, laus1$Flu_in_pregn_and_in_pandemic, useNA = "always")

# Define the order based on 'location'
order <- c(4, 1, 3, 2, 5)
# Reorder 'trimester3' based on 'location'
laus1$trimester3 <- factor(laus1$trimester3, levels = levels(laus1$trimester3)[order])
table(laus1$trimester3)

order <- c(2,1,3,4)
laus1$half3 <- factor(laus1$half3, levels = levels(laus1$half3)[order])
table(laus1$half3)

order <- c(2, 1, 3,4,5)
laus1$trimester_real_def3 <- factor(laus1$trimester_real_def3, levels = levels(laus1$trimester_real_def3)[order])


t<- laus1%>%
       filter(is.na(Grippe_cat) & Grippe=="yes" &
                 birthdate> "1918-06-01" &
                 birthdate < "1921-01-01" & month_1<"1920-04-01") 
