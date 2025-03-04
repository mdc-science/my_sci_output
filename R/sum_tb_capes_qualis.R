#### Table with Qualis information ####
df_table2 <-
  df_all %>%
  select(journal_abbr, if_2023_jcr, qualis_2017_2020) %>%
  group_by(journal_abbr, if_2023_jcr, qualis_2017_2020) %>%
  summarize(papers = n()) %>%
  arrange(desc(if_2023_jcr), qualis_2017_2020) %>%
  select(journal_abbr, papers, if_2023_jcr, qualis_2017_2020) %>%
  rename(
    Journal = journal_abbr, Documents = papers, "IF (2023)" = if_2023_jcr,
    "Qualis (2017-2020)" = qualis_2017_2020
  )

table2_theme <-
  ttheme(
    tbody.style = tbody_style(
      color = "black", fill = c("#fff5f0", "#fee0d2"),
      hjust = as.vector(matrix(c(0, 1, 1, 1), ncol = 4, nrow = nrow(df_table2), byrow = TRUE)),
      x = as.vector(matrix(c(.02, .55, .65, .55), ncol = 4, nrow = nrow(df_table2), byrow = TRUE))
    ),
    colnames.style = colnames_style(color = "gray95", fill = "#a50f15")
  )

tb_table2 <- ggtexttable(df_table2, rows = NULL, theme = table2_theme)

tb_main.title <- "Publications in International Peer-Reviewed Journal or Books"

tb_subtitle <- paste0(
  "Period: ", period_min, "-", period_max,
  " (updated on ", last_updated, ")"
)

tb_table2 <- tb_table2 %>%
  tab_add_title(text = tb_subtitle, face = "plain", size = 10) %>%
  tab_add_title(text = tb_main.title, face = "bold", padding = unit(0.1, "line")) %>%
  tab_add_footnote(
    text = paste0(
      "Qualis A1: ", round((sum(df_all$qualis_2017_2020 == "A1", na.rm = T) / nrow(df_all)) * 100, digits = 2), "%",
      "; A2: ", round((sum(df_all$qualis_2017_2020 == "A2", na.rm = T) / nrow(df_all)) * 100, digits = 2), "%",
      "; A3: ", round((sum(df_all$qualis_2017_2020 == "A3", na.rm = T) / nrow(df_all)) * 100, digits = 2), "%",
      "; A4: ", round((sum(df_all$qualis_2017_2020 == "A4", na.rm = T) / nrow(df_all)) * 100, digits = 2), "%",
      ";\nB1: ", round((sum(df_all$qualis_2017_2020 == "B1", na.rm = T) / nrow(df_all)) * 100, digits = 2), "%",
      "; B2: ", round((sum(df_all$qualis_2017_2020 == "B2", na.rm = T) / nrow(df_all)) * 100, digits = 2), "%",
      "; B3: ", round((sum(df_all$qualis_2017_2020 == "B3", na.rm = T) / nrow(df_all)) * 100, digits = 2), "%",
      "; B4: ", round((sum(df_all$qualis_2017_2020 == "B4", na.rm = T) / nrow(df_all)) * 100, digits = 2), "%",
      "\nPublications: ", nrow(df_all)
    ),
    size = 10
  )

tb_table2 <- tb_table2 %>%
  tab_add_hline(
    at.row = nrow(df_table2) + 3, row.side = "bottom",
    linetype = 1,
    linewidth = 2,
    linecolor = "#a50f15"
  )

if (!dir.exists("table")) {
  dir.create("table")
}

write_csv(df_table2, here("table", paste0(format(Sys.Date(), "%Y.%m.%d"), "_", period_min, "-", period_max, "_sum_tb_capes_qualis.csv")))

ggsave(
  file = here("table", paste0(format(Sys.Date(), "%Y.%m.%d"), "_", period_min, "-", period_max, "_sum_tb_capes_qualis.png")),
  tb_table2, bg = "white",
  height = 32, width = 16, units = "cm", dpi = 600
)
