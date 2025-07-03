lausgraph1 <- lausgraph

# Flu during pregnancy and during the pandemic ####
 flu_counts <- lausgraph1 %>%
   filter(Flu_in_pregn_and_in_pandemic=="yes") %>%
   group_by(Grippe_short_date_year_month) %>%
   mutate(Grippe_short_date_year_month=as.Date(Grippe_short_date_year_month))%>%
   summarise(count = n()) %>%
   ungroup()
 
 # bar plot
 ggplot_flu_timing <- ggplot(flu_counts, aes(x = Grippe_short_date_year_month, y=count)) +
   geom_bar(stat = "identity") +
   labs(x = "time", y = "count", title = "C) Maternal ILI - cases during the pandemic") +
   scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_y_continuous(limits=c(0,50)) +
   coord_cartesian(xlim = c(ymd("1918-07-01"), ymd("1920-12-01"))) +
   theme_minimal()+
   theme(axis.text.y = element_text(size=text_size),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),      
         legend.position = c(.08, .8),
         legend.text=element_text(size=legend_size),
         axis.title.y  = element_text(size=axis_legend_size),,
         axis.text.x = element_text(angle=45,size=14, hjust = 1),
         axis.title.x = element_text(size=16),
         title =element_text(size=title_size))
 ggplot_flu_timing

 ggsave("mat_flu_cases_timing_during_pandemic.pdf", ggplot_flu_timing, path=here("output/graphs/suppl_data"))
 
 
 # Flu during pregnancy and during or outside of the pandemic ####
 flu_counts2 <- lausgraph1 %>%
   group_by(Grippe_short_date_year_month) %>%
   mutate(Grippe_short_date_year_month=as.Date(Grippe_short_date_year_month))%>%
   summarise(count = n()) %>%
   ungroup()
 
 # bar plot
 ggplot_flu_timing_pregn_pandemic_during_or_not <- ggplot(flu_counts2, aes(x = Grippe_short_date_year_month, y=count)) +
   geom_bar(stat = "identity") +
   labs(x = "", y = "count", title = "B) Maternal ILI - all cases") +
   scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") + 
   scale_y_continuous(limits=c(0,50)) +
   coord_cartesian(xlim = c(ymd("1918-07-01"), ymd("1920-12-01"))) +
   theme_minimal()  +
   theme(axis.text.y = element_text(size=text_size),
                           panel.grid.major.x = element_blank(),
                           panel.grid.minor.x = element_blank(),
                           legend.position = c(.08, .8),
                           legend.text=element_text(size=legend_size),
                           axis.title.y  = element_text(size=axis_legend_size),
         axis.text.x = element_text(angle=45,size=14, hjust = 1),
         title =element_text(size=title_size))
 
 
 ggplot_flu_timing_pregn_pandemic_during_or_not
 

 # Hosp incidence in Vaud ####
 data_b     <- flu_waves %>%
   mutate(Tag = substr(Wochenende,1,2),
          Monat = substr(Wochenende, 4,5),
          Woche = ymd(paste0(Year,"-",Monat, "-", Tag)),
          KW = isoweek(Woche),
          month=ymd(paste0(Year,"-",Monat, "-", 1)))%>%
   filter(month>="1918-07-01" & month<="1920-12-01") %>%
   mutate(Namex= paste0(Year,"/W",KW),
          AndereInc = Andere_Infekt/PopCanton*1000) 
 
 data_b <- data_b %>%
   group_by(month) %>%
   summarise(AndereInc_month = mean(AndereInc)) %>%
   ungroup()
 
 FigureHospital <- ggplot(data_b,aes(y=AndereInc_month,x=month)) +
                    geom_bar(stat = "identity") +
   labs(x = "", y="per 1,000 inhabitants",title= " A) Hospitalisations incidence in the canton of Vaud, due to infections (incl. Influenza)") +
   scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") + 
   scale_y_continuous(breaks=seq(0,0.5, by=0.2))+
  coord_cartesian(ylim=c(0,0.5))+
   coord_cartesian(xlim = c(ymd("1918-07-01"), ymd("1920-12-01"))) +
   theme_minimal()+
   theme(axis.text.y = element_text(size=text_size),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         legend.position = c(.08, .8),
                         legend.text=element_text(size=legend_size),
                         axis.title.y  = element_text(size=axis_legend_size),
                        axis.text.x = element_text(angle=45,size=14, hjust = 1),
                         title =element_text(size=title_size))
 FigureHospital
                            
 
 ggplot_flu_incid_maternity_hosp_vaud <- cowplot::plot_grid(FigureHospital, 
                                                          ggplot_flu_timing_pregn_pandemic_during_or_not, 
                                                          ggplot_flu_timing,
                                     ncol=1, nrow=4, align="hv",
                                     rel_heights = c(1,1,1))
 ggplot_flu_incid_maternity_hosp_vaud
 
 ggsave("plot_flu_incid_maternity_hosp_vaud.pdf", ggplot_flu_incid_maternity_hosp_vaud, path=here("output/graphs/suppl_data"),
        height=14, width=12)
