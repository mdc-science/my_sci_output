#### Final Summary Table ####
total_publications <- df_all %>%
  filter(year %in% desired_period) %>%
  nrow()
total_papers <- df_all %>%
  filter(type != "chapter") %>%
  nrow()
total_chapters <- df_all %>%
  filter(type == "chapter") %>%
  nrow()
papers_first <- df_all %>%
  filter(type != "chapter") %>%
  filter(first_auth == "First") %>%
  nrow()
papers_shared <- df_all %>%
  filter(type != "chapter") %>%
  filter(first_auth == "First_shared") %>%
  nrow()
papers_corresponding <- df_all %>%
  filter(type != "chapter") %>%
  filter(corresp_auth == "Moreira, D.C.") %>%
  nrow()
chapters_first <- df_all %>%
  filter(type == "chapter") %>%
  filter(first_auth == "First") %>%
  nrow()
chapters_shared <- df_all %>%
  filter(type == "chapter") %>%
  filter(first_auth == "First_shared") %>%
  nrow()
chapters_corresponding <- df_all %>%
  filter(type == "chapter") %>%
  filter(corresp_auth == "Moreira, D.C.") %>%
  nrow()

df_sum <- data.frame(Role = character(4), Chapter = numeric(4), Paper = numeric(4))

df_sum$Role <- c("First", "Shared 1st", "Corresponding", "All")
df_sum$Chapter <- c(chapters_first, chapters_shared, chapters_corresponding, total_chapters)
df_sum$Paper <- c(papers_first, papers_shared, papers_corresponding, total_papers)
df_sum$Total <- rowSums(df_sum[sapply(df_sum, is.numeric)])

tb_sumtable <- ggtexttable(df_sum,
  rows = NULL,
  theme = ttheme("mBlue")
)

# Saving summary table
if (!dir.exists("table")) {
  dir.create("table")
}

write_csv(df_sum, here("table", paste0(format(Sys.Date(), "%Y.%m.%d"), "_", period_min, "-", period_max, "_sum_tb.csv")))

ggsave(
  file = here("table", paste0(format(Sys.Date(), "%Y.%m.%d"), "_", period_min, "-", period_max, "_sum_tb.png")),
  tb_sumtable, bg = "white",
  height = 5, width = 10, units = "cm", dpi = 600
)
