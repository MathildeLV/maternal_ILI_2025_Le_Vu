
## COUNTS PER YEAR ####
### abortion number through the years ####
miscarriages <- abortions1 %>%
  group_by(birthyear) %>%
  summarise(n_miscar=as.numeric(n()))
n_misc_year <- ggplot(miscarriages, aes(x = birthyear, y = n_miscar)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "counts", title = "Miscarriage Counts per year")+
  theme(plot.title = size_title)
n_misc_year
### birth number through the years ####
births <- laus2_prep %>%
  group_by(birthyear) %>%
  summarise(n_births=as.numeric(n()))
n_births_year <- ggplot(births, aes(x = birthyear, y = n_births)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "counts", title = "Birth counts per year")+
  theme(plot.title = size_title)
n_births_year

## COUNTS PER QUARTER OF THE YEAR ####
### abortion number by quarter ####
miscarriages_n_by_quarter <- abortions1 %>%
  mutate(quarter=as.factor(quarter)) %>%
  group_by(quarter) %>%
  summarise(n_miscar=as.numeric(n()))
n_misc_quarter <- ggplot(miscarriages_n_by_quarter, aes(x = quarter, y = n_miscar)) +
  geom_bar(stat = "identity") +
  labs(x = "quarter", y = "counts", title = "Miscarriage counts per quarter")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = size_title)+
  scale_x_discrete(breaks=label_every_second)
n_misc_quarter

### birth number by quarter ####
laus2_n_by_quarter <- laus2_prep %>%
  mutate(quarter=as.factor(quarter)) %>%
  group_by(quarter) %>%
  summarise(n_births=as.numeric(n()))
n_births_quarter <- ggplot(laus2_n_by_quarter, aes(x = quarter, y = n_births)) +
  geom_bar(stat = "identity") +
  labs(x = "quarter", y = "counts", title = "Births counts per quarter")+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = size_title) +
  scale_x_discrete(breaks=label_every_second)
n_births_quarter


## BOTH MISCARRIAGES AND BIRTHS ####
### Miscarriage % by year ####
abortions_vs_no_abortions_n_by_year <- abortions_not_abortions %>%
  group_by(birthyear) %>%
  summarise(n_total=as.numeric(n()),
            n_abortions=sum(abortion=="yes", na.rm=T),
            n_births=sum(abortion=="no", na.rm = T),
            perc_abort_total=100*n_abortions/n_total)

perc_misc_vs_all_deliv_year <- ggplot(abortions_vs_no_abortions_n_by_year, aes(x = birthyear, y = perc_abort_total)) +
  geom_bar(stat = "identity") +
  labs(x = "quarter", y = "percentage", title = "% of abortions vs. all deliveries per year") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = size_title) 
perc_misc_vs_all_deliv_year

### Miscarriage % by quarter ####
abortions_vs_no_abortions_n_by_quarter <- abortions_not_abortions %>%
  mutate(quarter=as.factor(quarter)) %>%
  group_by(quarter) %>%
  summarise(n_total=as.numeric(n()),
            n_abortions=sum(abortion=="yes", na.rm=T),
            n_births=sum(abortion=="no", na.rm = T),
            perc_abort_total=100*n_abortions/n_total)

perc_misc_vs_all_deliv_quarter <- ggplot(abortions_vs_no_abortions_n_by_quarter, aes(x = quarter, y = perc_abort_total)) +
  geom_bar(stat = "identity") +
  labs(x = "quarter", y = "percentage", title = "% of abortions vs. all deliveries per quarter") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = size_title
  ) +
  scale_x_discrete(breaks=label_every_second)
perc_misc_vs_all_deliv_quarter

miscar_births <- egg::ggarrange(n_misc_year,n_births_year,
                           n_misc_quarter, n_births_quarter,
                           perc_misc_vs_all_deliv_year,perc_misc_vs_all_deliv_quarter)
ggsave(here("output/graphs/suppl_data", "misc_and_births_year_quarter_counts_perc.pdf"), miscar_births, height=10, width = 10)
