df_table1 <- 
  df_all %>% 
  select(journal_abbr, if_2022_jcr, quartile_2022, area_2022) %>%
  group_by(journal_abbr, if_2022_jcr, quartile_2022, area_2022) %>%
  summarize(papers = n()) %>% 
  arrange(desc(if_2022_jcr), quartile_2022) %>%
  select(journal_abbr, papers, if_2022_jcr, quartile_2022, area_2022) %>%
  rename(Journal = journal_abbr, Documents = papers, IF = if_2022_jcr, 
         Quartile = quartile_2022, 'Scimago Area' = area_2022)  

table1_theme <- 
  ttheme(tbody.style = tbody_style(color = "black", fill = c("#f7fbff", "#deebf7"),
                                   hjust = as.vector(matrix(c(0, 1, 1, 1, 0), ncol = 5, nrow = nrow(df_table1), byrow = TRUE)),
                                   x = as.vector(matrix(c(.02, .55, .9,.65, .02), ncol = 5, nrow = nrow(df_table1), byrow = TRUE))),
         colnames.style = colnames_style(color = "gray95", fill = "#08519c"))

tb_table1 <- ggtexttable(df_table1, rows = NULL, theme = table1_theme)

tb_main_title <- "Publications in International Peer-Reviewed Journal or Books"

tb_subtitle <- paste0("Period: ", period_min,"-", period_max,
                      " (updated on ", last_updated, ")")

tb_table1 <- tb_table1 %>%
  tab_add_title(text = tb_subtitle, face = "plain", size = 10) %>%
  tab_add_title(text = tb_main_title, face = "bold", padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = paste0("SCImago Q1: ", round((sum(df_all$quartile_2022 == "Q1", na.rm = T)/nrow(df_all))*100, digits = 2), "%",
                                 "; Q2: ", round((sum(df_all$quartile_2022 == "Q2", na.rm = T)/nrow(df_all))*100, digits = 2), "%",  
                                 "; Q3: ", round((sum(df_all$quartile_2022 == "Q3", na.rm = T)/nrow(df_all))*100, digits = 2), "%",
                                 "; Q4: ", round((sum(df_all$quartile_2022 == "Q4", na.rm = T)/nrow(df_all))*100, digits = 2), "%",
                                 "\nPublications: ", nrow(df_all)),
                   size = 10)

tb_table1 <- tb_table1 %>% 
  tab_add_hline(at.row = nrow(df_table1) + 3, row.side = "bottom", 
                linetype = 1,
                linewidth = 2,
                linecolor = "#08519c")

if (!dir.exists('table')) {
  dir.create('table')
}

ggsave(file = here('table', paste(format(Sys.Date(), "%Y.%m.%d"), "sum_tb_scimago_quartile.png", sep = '_')), 
       tb_table1, bg = "white", 
       height = 27, width = 23, units = "cm", dpi = 600)
