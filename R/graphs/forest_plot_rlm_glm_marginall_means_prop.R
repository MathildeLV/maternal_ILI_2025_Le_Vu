cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
lwdline <- 1.2
size_title <- 15
size_axis <- 12
size_axis_title <- 16
size_legend <- 14
size_legend_title <- 16
axis.title.x.position <- element_text(margin = margin(t =12))
lwd_size <- 0.7

# MAIN MODELS ####
# Forest plot binary outcomes ####
a_m_bin_outc_u #univ
a_m_bin_outc_multiv # multiv
a_m_bin_outc_u_sex # univ sex


## Combine both dataframes  ####
combined_data <- rbind(a_m_bin_outc_u, a_m_bin_outc_u_sex,a_m_bin_outc_multiv)
combined_data <- combined_data %>%
  filter(`flu exp`=="yes") %>%
  mutate(Category=factor(sex, levels = c( "females", "males","all")),
         outcome=factor(outcome, levels=c("LBW (<2,500g)", "PTB (<37 weeks)", "stillbirth","early neonatal mortality")),
         model=factor(model, levels=c("multivariable","univariable"))
         )  


## Plot big  ####
plot_bin_diff_big <- ggplot(combined_data, aes(x=d, y=Category, colour=model, shape=model)) +
  facet_wrap(~outcome, ncol=3, nrow=2) +
  geom_pointrange(aes(xmin=lower_diff, xmax=upper_diff), position=position_dodge(width=0.7), size=lwd_size) +
  geom_vline(aes(xintercept=0), lwd=0.2, col="grey30") +
  scale_color_manual(values = c("univariable" = "#999999", "multivariable" = "#E69F00")) +
   scale_shape_manual(values = c("univariable" = 18, "multivariable" = 15)) +
  # scale_x_log10() +
  xlab("marginally adjusted proportion difference (%)") +
  ylab("") + 
  theme_bw() +
  ggtitle("B") +
  theme(
    plot.title = element_text(size=size_title),           # Title size
    axis.title.x = element_text(size=size_axis_title),    # X-axis title size
    axis.title.y = element_text(size=size_axis_title),    # Y-axis title size
    axis.text = element_text(size=size_axis),             # Axis text size
    legend.position = "none",
    strip.text = element_text(size=14)                    # Facet subtitle size
  )
plot_bin_diff_big

# Forest plot continuous outcomes ####
a_m_cont_outc_u #univ 
a_m_cont_outc_u_multiv #multiv
a_m_cont_outc_u_sex #univ sex

## Combine both dataframes  ####
combined_data_cont <- rbind(a_m_cont_outc_u,a_m_cont_outc_u_sex,a_m_cont_outc_u_multiv)
combined_data <- combined_data_cont %>%
  filter(`flu exp`=="yes") %>%
  mutate(Category=factor(sex, levels = c( "females", "males","all")),
         outcome=factor(outcome, levels=c("birth weight (g)", "head circumference (cm)", 
                                          "placenta weight (g)","birth length (cm)",
                                          "ponderal index", "gestational age (weeks)")),
         model=factor(model, levels=c("multivariable","univariable")),
         across(c(mean, lci, uci), as.numeric))  


## Plot big  ####
plot_cont_diff_big <- ggplot(combined_data, aes(x=d, y=Category,colour=model,  shape=model)) +
  facet_wrap(~outcome, scales = "free_x") +
  geom_pointrange(aes(xmin=lower_diff, xmax=upper_diff), position=position_dodge(width=0.7), size=lwd_size) +
  geom_vline(aes(xintercept=0), lwd=0.2, col="grey30") +
  scale_shape_manual(values = c("univariable" = 18, "multivariable" = 15)) +
  scale_color_manual(values = c("univariable" = "#999999", "multivariable" = "#E69F00")) +
  # scale_x_log10() +
  xlab("marginally adjusted mean difference") +
  ylab("sex") + 
  theme_bw() +
  ggtitle("A") +
  theme(
    plot.title = element_text(size=size_title),           # Title size
    axis.title.x = element_text(size=size_axis_title),    # X-axis title size
    axis.title.y = element_text(size=size_axis_title),    # Y-axis title size
    axis.text = element_text(size=size_axis),             # Axis text size
    legend.title = element_text(size=size_legend_title),  # Legend title size
    legend.text = element_text(size=size_legend),         # Legend text size
    strip.text = element_text(size=14)                    # Facet subtitle size
  )
plot_cont_diff_big

## Binary and continuous outcomes together ####
main_models_forest <- egg::ggarrange(plot_cont_diff_big,plot_bin_diff_big,
                              nrow=2)
ggsave(here("output/graphs", "forest_plot_outcomes_main_RLMs_GLMs_and_stratif_sex_mean_dif.pdf"), main_models_forest , height = 12, width=11)

# TRIMESTER EFFECT ####
a_m_bin_outc_univ_trim #univ 
a_m_bin_outc_multiv_trim #multiv
a_m_bin_outc_univ_trim_sex #univ sex


## Combine dataframes  ####
combined_data_trim <- rbind(a_m_bin_outc_univ_trim, a_m_bin_outc_multiv_trim, a_m_bin_outc_univ_trim_sex)
combined_data_trim <- combined_data_trim %>%
  filter(trimester!="no") %>%
  mutate(sex=factor(sex, levels = c( "females", "males","all")),
         trimester=factor(trimester, levels=c("first", "second", "third")),
         outcome=factor(outcome, levels=c("LBW (<2,500g)", "PTB (<37 weeks)", "stillbirth")),
         model=factor(model, levels=c("univariable", "multivariable")),
         across(c(d, lower_diff, upper_diff), as.numeric))  


## Plot big ####
plot_bin_diff_trim_big <- ggplot(combined_data_trim, aes(x=d, y=sex, colour=trimester, shape=model)) +
  facet_wrap(~outcome, ncol=3, nrow=2, drop = FALSE) +
  geom_pointrange(aes(xmin=lower_diff, xmax=upper_diff), position=position_dodgev(height=0.8), size=lwd_size) +
  geom_vline(aes(xintercept=0), lwd=0.2, col="grey30") +
  scale_color_manual(values = c("first" = "#CC79A7", "second" = "#009E73", 
                                "third"="#D55E00")) +  
  scale_shape_manual(values = c("univariable"=18, "multivariable" = 15)) +
  # scale_x_log10() +
  xlab("marginally adjusted proportion difference (%)") +
  ylab("") + 
  theme_bw() +
  ggtitle("B") +
  theme(
    plot.title = element_text(size=size_title),           # Title size
    axis.title.x = element_text(size=size_axis_title),    # X-axis title size
    axis.title.y = element_text(size=size_axis_title),    # Y-axis title size
    axis.text = element_text(size=size_axis),             # Axis text size
    legend.position = "none", 
    strip.text = element_text(size=14)                    # Facet subtitle size
  )
plot_bin_diff_trim_big
ggsave(here("output/graphs", "forest_plot_outcomes_trimester_diff_and_stratif_sex_BIN.pdf"), plot_bin_diff_trim_big , height = 3, width=9)

# Forest plot continuous outcomes ####
a_m_cont_outc_univ_trim # univ
a_m_cont_outc_multiv_trim # multiv
a_m_cont_outc_univ_trim_sex # sex univ

## Combine both dataframes  ####
combined_data_trim_cont <- rbind(a_m_cont_outc_univ_trim, a_m_cont_outc_multiv_trim, a_m_cont_outc_univ_trim_sex)
combined_data_trim_cont <- combined_data_trim_cont %>%
  filter(trimester!="no") %>%
  mutate(sex=factor(sex, levels = c( "females", "males","all")),
         trimester=factor(trimester, levels=c("first", "second", "third")),
         outcome=factor(outcome, levels=c("birth weight (g)", "head circumference (cm)", 
                                          "placenta weight (g)","birth length (cm)",
                                          "ponderal index", "gestational age (weeks)")),
         model=factor(model, levels=c("multivariable", "univariable")),
         across(c(d, lower_diff, upper_diff), as.numeric))  


## Plot big ####
plot_cont_diff_trim_big <-  ggplot(combined_data_trim_cont , aes(x=d,y=sex,colour=trimester, shape=model))+
  facet_wrap(~outcome, scales = "free_x") +  # Free x-axis scales for each facet
  geom_pointrange(aes(xmin=lower_diff,xmax=upper_diff),position=position_dodgev(height=0.8),size=lwd_size) +
  geom_vline(aes(xintercept=0), lwd=0.2, col="grey30") +
  scale_color_manual(values = c("first" = "#CC79A7", "second" = "#009E73", 
                                "third"="#D55E00")) +
  scale_shape_manual(values = c("univariable" = 18, "multivariable"=15)) +
  # scale_x_log10() +
  xlab("marginally adjusted mean difference") +
  ylab("sex")+
  theme_bw()+
  ggtitle("A")+
  theme(
    plot.title = element_text(size=size_title),           # Title size
    axis.title.x = element_text(size=size_axis_title),    # X-axis title size
    axis.title.y = element_text(size=size_axis_title),    # Y-axis title size
    axis.text = element_text(size=size_axis),             # Axis text size
    legend.title = element_text(size=size_legend_title),  # Legend title size
    legend.text = element_text(size=size_legend),         # Legend text size
    strip.text = element_text(size=14)                    # Facet subtitle size
  )
plot_cont_diff_trim_big
ggsave(here("output/graphs", "forest_plot_outcomes_trimester_diff_and_stratif_sex_CONT.pdf"), plot_cont_diff_trim_big , height = 6, width=11)

## Binary and continuous outcomes together ####
# trimester_forest <- ggarrange(plot_cont_diff_trim_big,plot_bin_diff_trim_big,
#                              nrow=2)
# ggsave(here("output/graphs", "forest_plot_outcomes_trimester_GLMs_and_stratif_sex.pdf"), trimester_forest , height = 12, width=11)


# SEVERITY EFFECT ####
# Forest plot binary outcomes ####
a_m_bin_outc_sev_u #univ
a_m_bin_outc_multiv_sev # multiv
a_m_bin_outc_u_sev_sex # univ sex


## Combine both dataframes  ####
combined_data_sev <- rbind(a_m_bin_outc_sev_u, a_m_bin_outc_multiv_sev,
                           a_m_bin_outc_u_sev_sex)
combined_data_sev <- combined_data_sev %>%
  filter(severity!="no") %>%
  mutate(sex=factor(sex, levels = c( "females", "males","all")),
         severity=factor(severity, levels=c("mild", "severe")),
         outcome=factor(outcome, levels=c("LBW (<2,500g)", "PTB (<37 weeks)", "stillbirth","early neonatal mortality")),
         model=factor(model, levels=c("multivariable", "univariable")),
         across(c(d, lower_diff, upper_diff), as.numeric))
  

## Plot big ####
plot_bin_diff_sev <-  ggplot(combined_data_sev , aes(x=d,y=sex,colour=severity, shape=model
))+
  facet_wrap(~outcome, ncol=3, nrow=2)+
  geom_pointrange(aes(xmin=lower_diff,xmax=upper_diff),position=position_dodgev(height=0.8),size=lwd_size) +
  geom_vline(aes(xintercept=0), lwd=0.2, col="grey30") +
  scale_color_manual(values = c("mild" = "#CC79A7", "severe"="#0072B2")) +
  scale_shape_manual(values = c("multivariable" = 15, "univariable"=18)) +
  # scale_x_log10() +
  xlab("marginally adjusted proportion difference (%)") +
  ylab("") + 
  theme_bw() +
  ggtitle("B")+
  theme(
    plot.title = element_text(size=size_title),           # Title size
    axis.title.x = element_text(size=size_axis_title),    # X-axis title size
    axis.title.y = element_text(size=size_axis_title),    # Y-axis title size
    axis.text = element_text(size=size_axis),             # Axis text size
    legend.position = "none", 
    strip.text = element_text(size=14)                    # Facet subtitle size
  )
plot_bin_diff_sev

ggsave(here("output/graphs", "forest_plot_outcomes_severity_diff_and_stratif_sex_BIN.pdf"), plot_bin_diff_sev , height = 3, width=9)

# Forest plot continuous outcomes ####
a_m_cont_outc_sev_u # univ
a_m_cont_outc_u_multiv_sev # multiv
a_m_cont_outc_u_sev_sex # sex univ

## Combine both dataframes  ####
combined_data_sev <- rbind(a_m_cont_outc_sev_u, a_m_cont_outc_u_multiv_sev, 
                           a_m_cont_outc_u_sev_sex)
combined_data_sev <- combined_data_sev %>%
  filter(severity!="no") %>%
  mutate(sex=factor(sex, levels = c( "females", "males","all")),
         severity=factor(severity, levels=c("mild", "severe")),
         outcome=factor(outcome, levels=c("birth weight (g)", "head circumference (cm)", 
                                          "placenta weight (g)","birth length (cm)",
                                          "ponderal index", "gestational age (weeks)")),
         model=factor(model, levels=c("multivariable", "univariable")),
         across(c(d, lower_diff, upper_diff), as.numeric))


## Plot big ####
plot_cont_diff_sev_big <-  ggplot(combined_data_sev , aes(x=d,y=sex,colour=severity, shape=model))+
  facet_wrap(~outcome, scales = "free_x") +  # Free x-axis scales for each facet
  geom_pointrange(aes(xmin=lower_diff,xmax=upper_diff),position=position_dodgev(height=0.8),size=lwd_size) +
  geom_vline(aes(xintercept=0), lwd=0.2, col="grey30") +
  scale_color_manual(values = c("mild" = "#CC79A7", "severe"="#0072B2")) +
  scale_shape_manual(values = c("univariable" = 18, "multivariable"=15)) +
  # scale_x_log10() +
  xlab("marginally adjusted mean difference") +
  ylab("sex")+
  theme_bw()+
  ggtitle("A")+
  theme(
    plot.title = element_text(size=size_title),           # Title size
    axis.title.x = element_text(size=size_axis_title),    # X-axis title size
    axis.title.y = element_text(size=size_axis_title),    # Y-axis title size
    axis.text = element_text(size=size_axis),             # Axis text size
    legend.title = element_text(size=size_legend_title),  # Legend title size
    legend.text = element_text(size=size_legend),         # Legend text size
    strip.text = element_text(size=14)                    # Facet subtitle size
  )
plot_cont_diff_sev_big

ggsave(here("output/graphs", "forest_plot_outcomes_severity_diff_and_stratif_sex_CONT.pdf"), plot_cont_diff_sev_big , height = 6, width=11)

## Binary and continuous outcomes together ####
# severity_forest <- ggarrange(plot_beta_sev,plot_OR_sev,
#                      nrow=2)
# ggsave(here("output/graphs", "forest_plot_outcomes_severity_GLMs_and_stratif_sex.pdf"), severity_forest, height = 12, width=11)
