lausgraph1 <- lausgraph

lausgraph1 <- lausgraph1%>%
  mutate(parity_cat2= case_when(is.na(parity_cat2) ~ "Missing",
                                 TRUE ~as.character(parity_cat2)),
         sex=case_when(is.na(sex)~"Missing",
                       TRUE ~as.character(sex)),
         across(c(parity_cat2), as.factor)) 

desired_order_parity_cat2<- c("Missing",">2",  "2", "1")
desired_order_civil_status_cat_2<- c("married","single or missing")
desired_order_Lausanne<- c("unsure","yes", "no")
desired_order_Lausanne_NEU<- c("unsure","yes", "no")
desired_order_morphology<- c("thin","obese", "neither")
desired_order_hisco_class_3<- c("missing","3", "2", "1")
desired_order_GA_cat <- c(">41", "37-41", "33-37", "<33") 
desired_order_sex <- c("Missing", "Female", "Male") 

attr(lausgraph1$stillbirth, 'levels')<- c("livebirth", "stillbirth")
attr(lausgraph1$sex, 'levels')<- c("Male", "Female")
attr(lausgraph1$PTB_corrected, "levels")<- c('term', 'preterm')
attr(lausgraph1$neonat_mort_d1_d5, "levels")<- c('alive', 'dead')
attr(lausgraph1$LBW, "levels")<- c('normal BW', 'LBW')

lausgraph1 <- lausgraph1 %>%
  mutate(parity_cat2 = factor(parity_cat2, levels = desired_order_parity_cat2),
         Lausanne = factor(Lausanne, levels = desired_order_Lausanne),
         Lausanne_NEU = factor(Lausanne_NEU, levels = desired_order_Lausanne),
         morphology = factor(morphology, levels = desired_order_morphology),
         civil_status_cat2 = factor(civil_status_cat2, levels = desired_order_civil_status_cat_2),
         hisco_class_3 = factor(hisco_class_3, levels = desired_order_hisco_class_3),
         GA_weeks_cat_corrected2 = factor(GA_weeks_cat_corrected2, levels = desired_order_GA_cat),
         sex=factor(sex, levels=desired_order_sex))

lausgraph1$civil_status_cat2 <- relevel(lausgraph1$civil_status_cat2, ref="single or missing")



### nb of births per year #### 
ggplot_births_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear)) +
  geom_bar(fill = "grey", show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 1400, by = 200), limits = c(0, 1400)) +
  scale_x_continuous(breaks = seq(1905, 1924, by=2)) +
  labs(
    x = "Year",
    y = "Number of births"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(color = "black", size = 20),
    axis.text.x = element_text(color = "black", size = 20, angle = 45, hjust = 1.2),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) 
ggplot_births_per_year
ggsave("output/graphs/suppl_data/nb_births_by_year_1905_24.pdf",
       ggplot_births_per_year, width = 10, height = 7)


# SETTING SIZE ELEMENTS ####
size_axis <- 14
size_axis_title <- 16
size_legend <- 12
size_legend_title <- 14
axis.title.x.position <- element_text(margin = margin(t =12))
text.y.axis=element_text(color="black",size=14)
text.x.axis=element_text(color="black",size=14,angle=45,hjust=1.2)
text.legend=element_text(size=13)
title.legend=element_text(size=14)


# maternal charactertics ####
## Categorical variables ####
### parity cat per year #### 
custom_colors <- c("grey","#458B74","#7FFFD4","#FFB6C1") 
ggplot_parity_cat_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, fill = parity_cat2)) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks=seq(1905,1924, by=2))+
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(x = "", y="",
       fill="parity") +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_parity_cat_per_year

### civil status per year #### 
custom_colors <- c("pink","#458B74") 
ggplot_civil_stat_cat_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, fill = civil_status_cat2)) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(
       x = "",
       y = "",
       fill="civil status") +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_civil_stat_cat_per_year

### HISCO class3 per year #### 
custom_colors <- c("grey","#458B74","#7FFFD4","#FFB6C1") 
ggplot_HISCO_3_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, fill = hisco_class_3)) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) + 
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(
    #title = "Share of Parity Categories per Birth Year",
    x = "",
    y = "",
    fill="HISCO class") +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_HISCO_3_per_year

### Living in Lausanne per year #### 
custom_colors <- c("green4", "olivedrab2")  # Replace with your desired colors
ggplot_living_in_lausanne_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, fill = Lausanne_NEU)) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(x = "",
    y = "",
    fill="living in Lausanne") +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_living_in_lausanne_per_year


### Morphology per year #### 
custom_colors <- c("purple", "magenta", "pink") 
ggplot_morphology_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, fill = morphology)) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(x = "",
    y = "",
    fill="morphology") +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_morphology_per_year

### Flu per year #### 
ggplot_flu_per_year <- lausgraph1 %>%
  group_by(birthyear)%>%
  summarise(percentage=sum(Flu_in_pregn_and_in_pandemic=="yes", na.rm = TRUE)/n())%>%
  ggplot(aes(x = birthyear, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits=c(0,0.2)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(x = "",
    y = "") +
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_flu_per_year

### Syphilis per year #### 
ggplot_syph_per_year <- lausgraph1 %>%
  group_by(birthyear)%>%
  summarise(percentage=sum(Syphilis_2=="yes", na.rm = TRUE)/n())%>%
  ggplot(aes(x = birthyear, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits=c(0,0.05)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(x = "",
       y = "") +
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_syph_per_year

### Goitre per year #### 
ggplot_goitre_per_year <- lausgraph1 %>%
  group_by(birthyear)%>%
  summarise(percentage=sum(Goitre=="yes", na.rm = TRUE)/n())%>%
  ggplot(aes(x = birthyear, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits=c(0,0.2)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(x = "",
       y = "") +
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_goitre_per_year

### Rickets per year #### 
ggplot_rickets_per_year <- lausgraph1 %>%
  group_by(birthyear)%>%
  summarise(percentage=sum(rickets=="yes", na.rm = TRUE)/n())%>%
  ggplot(aes(x = birthyear, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits=c(0,0.25)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(x = "",
       y = "") +
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_rickets_per_year

### season per year #### 
custom_colors <- c("olivedrab2","#458B74","#7FFFD4","#FFB6C1") 
ggplot_season_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, fill = season)) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks=seq(1905,1924, by=2))+
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(x = "", y="",
       fill="season") +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_season_per_year

### Maternal mortality per year #### 
ggplot_mat_death_per_year <- lausgraph1 %>%
  group_by(birthyear)%>%
  summarise(percentage=sum(Mother_deceased=="yes", na.rm = TRUE)/n())%>%
  ggplot(aes(x = birthyear, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits=c(0,0.02)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(x = "",
       y = "") +
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_mat_death_per_year


## Continuous variables ####
### Mat age ####
gg_birthy_mat_age <- ggplot(lausgraph1, aes(x = birthyear, y = age_mother, group = birthyear)) +
  geom_violin(fill = "#7FFFD4") +  
  labs(x = "", y = "years") +
  scale_x_continuous(breaks = seq(1905, 1924, by=2)) + 
  scale_y_continuous(breaks=seq(10,50,by=5), limits=c(10,50))+
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
gg_birthy_mat_age

### Mat height ####
gg_birthy_height <- ggplot(lausgraph1, aes(x = birthyear, y = height, group = birthyear)) +
  geom_violin(fill = "#7FFFD4") +  
  labs(x = "", y = "cm") +
scale_x_continuous(breaks = seq(1905, 1924, by=2)) + 
  scale_y_continuous(breaks=seq(95,185,by=10), limits=c(95,185))+
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
gg_birthy_height


## Save grap mat charact over years  ####
mat_var <- egg::ggarrange(gg_birthy_mat_age,gg_birthy_height,
                     ggplot_civil_stat_cat_per_year, ggplot_parity_cat_per_year,
                     ggplot_living_in_lausanne_per_year, ggplot_morphology_per_year,
                     ggplot_goitre_per_year, ggplot_rickets_per_year,
                     ggplot_flu_per_year, ggplot_syph_per_year,ggplot_mat_death_per_year,
                     ggplot_HISCO_3_per_year,ggplot_season_per_year,
                       labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
                       ncol=3,
                       nrow=5)
mat_var
ggsave("ggpl_maternal_var_over_years.pdf", mat_var, width = 16, height = 12, units = "in", path = here("output/graphs/suppl_data"))


# Neonatal characteristics per year #### 
## Categorical variables ####
## gest age per year #### 
custom_colors <- c("olivedrab2","#458B74","#7FFFD4","#FFB6C1") 
ggplot_GA_weeks_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, fill = GA_weeks_cat_corrected2)) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(
    x = "year",
    y = "",
    fill="weeks") +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend)
ggplot_GA_weeks_per_year

## sex #### 
custom_colors <- c("grey","#7FFFD4","#458B74") 
ggplot_sex_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, fill = sex)) +
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  scale_fill_manual(values=custom_colors) +  # Set custom labels
  labs(
    x = "year",
    y = "",
    fill="weeks") +
  guides(fill = guide_legend(title = NULL)) + # Remove legend title
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend)
ggplot_sex_per_year

## stillbirth per year #### 
ggplot_stillbirth_per_year <- lausgraph1 %>%
  group_by(birthyear)%>%
  summarise(percentage=sum(stillbirth=="stillbirth", na.rm = TRUE)/n())%>%
  ggplot(aes(x = birthyear, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits=c(0,0.07)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  labs(x = "",
       y = "") +
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend)
ggplot_stillbirth_per_year

## neonatal mortality d1-d5 per year #### 
ggplot_neonat_mort_d1_5_per_year <- lausgraph1 %>%
  group_by(birthyear)%>%
  summarise(percentage=sum(neonat_mort_d1_d5=="dead", na.rm = TRUE)/n())%>%
  ggplot(aes(x = birthyear, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits=c(0,0.06)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  labs(x = "",
       y = "") +
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend)
ggplot_neonat_mort_d1_5_per_year

## LBW per year #### 
ggplot_LBW_per_year <- lausgraph1 %>%
  group_by(birthyear)%>%
  summarise(percentage=sum(LBW=="LBW", na.rm = TRUE)/n())%>%
  ggplot(aes(x = birthyear, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits=c(0,0.15)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  labs(x = "",
       y = "") +
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend)
ggplot_LBW_per_year

## PTB per year #### 
ggplot_PTB_per_year <- lausgraph1 %>%
  group_by(birthyear)%>%
  summarise(percentage=sum(PTB_corrected=="preterm", na.rm = TRUE)/n())%>%
  ggplot(aes(x = birthyear, y = percentage)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits=c(0,0.2)) +
  scale_x_continuous(breaks = seq(1905,1924, by=2)) +  # Customize x-axis breaks and limits
  labs(x = "",
       y = "") +
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend)
ggplot_PTB_per_year


## birthweight per year #### 
  ggplot_BW_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, y = birthweight, group = birthyear)) +
  geom_violin(fill = "#7FFFD4") +  
  labs(x = "", y = "g") +
  scale_x_continuous(breaks = seq(1905, 1924, by=2)) + 
  scale_y_continuous(breaks=seq(500,5500,by=500), limits=c(500,5500))+
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_BW_per_year

## Head circ #### 
ggplot_HC_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, y = head_circ, group = birthyear)) +
  geom_violin(fill = "#7FFFD4", width=0.5, position = position_dodge(100)) +  
  labs(x = "", y = "cm") +
  scale_x_continuous(breaks = seq(1905, 1924, by=2)) + 
  scale_y_continuous(breaks=seq(20,50,by=5), limits=c(20,50))+
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_HC_per_year

## Placenta  #### 
ggplot_PW_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, y = placentaweight, group = birthyear)) +
  geom_violin(fill = "#7FFFD4", width=0.5, position = position_dodge(100)) +  
  labs(x = "", y = "g") +
  scale_x_continuous(breaks = seq(1905, 1924, by=2)) + 
  scale_y_continuous(breaks=seq(50,1300,by=100), limits=c(50,1300))+
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_PW_per_year

## Birth length  #### 
ggplot_BL_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, y = babylength, group = birthyear)) +
  geom_violin(fill = "#7FFFD4", width=0.5, position = position_dodge(100)) +  
  labs(x = "", y = "cm") +
  scale_x_continuous(breaks = seq(1905, 1924, by=2)) + 
  scale_y_continuous(breaks=seq(25,65,by=5), limits=c(25,65))+
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_BL_per_year

## Ponderal index  #### 
ggplot_PI_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, y = PI, group = birthyear)) +
  geom_violin(fill = "#7FFFD4", width=0.5, position = position_dodge(100)) +  
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(1905, 1924, by=2)) + 
  scale_y_continuous(breaks=seq(0.5,6,by=0.5), limits=c(0.5,6))+
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_PI_per_year

## Gestational age #### 
ggplot_GA_per_year <- lausgraph1 %>%
  ggplot(aes(x = birthyear, y = GA_weeks_corrected, group = birthyear)) +
  geom_violin(fill = "#7FFFD4", width=0.5, position = position_dodge(100)) +  
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(1905, 1924, by=2)) + 
  scale_y_continuous(breaks=seq(28,44,by=2), limits=c(28,44))+
  theme_minimal()+
  theme(axis.text.y=text.y.axis, #size_axis
        axis.text.x=text.x.axis, #size_axis
        legend.text=text.legend,
        legend.title=title.legend,
  )
ggplot_GA_per_year

## Save grap neonat charact over years  ####
neonat_var <- egg::ggarrange(ggplot_BW_per_year,ggplot_HC_per_year,
                        ggplot_PW_per_year, ggplot_BL_per_year,
                        ggplot_PI_per_year, ggplot_GA_per_year,
                        ggplot_sex_per_year,
                        ggplot_LBW_per_year, ggplot_PTB_per_year,
                        ggplot_stillbirth_per_year,
                        ggplot_neonat_mort_d1_5_per_year,
                     labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
                     ncol=3,
                     nrow=4)
neonat_var
ggsave("ggpl_neonatal_var_over_years.pdf", neonat_var, width = 16, height = 12, units = "in", path = here("output/graphs/suppl_data"))

