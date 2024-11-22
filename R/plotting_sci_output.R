# Custom theme function
my_theme <- function() {
  theme_few() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.title.y = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.line = element_line(linewidth = 0.1),
      panel.border = element_rect(color = "black", linewidth = 0.5),
      axis.ticks.length.x = unit(0.2, "mm"),
      axis.ticks.length.y = unit(0.2, "mm"),
      axis.text.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.ticks.y.right = element_blank()
    )
}

#### General view number of publications in each year ####
# Plot
fig_alldocs <-
  ggplot(df_all, aes(x = year)) +
  geom_bar(stat = "count", color = "gray25", fill = "ivory") +
  geom_text(stat = "count", aes(label = after_stat(count)), nudge_y = 0.5) +
  scale_x_continuous(
    limits = c(period_min - 0.5, period_max + 0.5),
    breaks = seq(period_min, period_max, by = 1)
  ) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, length.out = 5)) +
  labs(
    title = "Published documents - All types",
    subtitle = paste0(
      "Period: ", period_min, "-", period_max,
      " (updated on ", last_updated, ")"
    )
  ) +
  xlab("Year") +
  ylab("Documents") +
  my_theme()

#### General view cumulative number of publications per year ####
# Plot
fig_cumdocs <-
  ggplot(df_all, aes(x = year)) +
  geom_bar(stat = "count", aes(y = cumsum(after_stat(count))), color = "gray25", fill = "ivory") +
  geom_text(stat = "count", nudge_y = 2, aes(label = cumsum(after_stat(count)), y = cumsum(after_stat(count)))) +
  scale_x_continuous(
    limits = c(period_min - 0.5, period_max + 0.5),
    breaks = seq(period_min, period_max, by = 1)
  ) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, length.out = 5)) +
  labs(
    title = "Published documents, cumulative - All types",
    subtitle = paste0("Period: ", period_min, "-", period_max, " (updated on ", last_updated, ")")
  ) +
  xlab("Year") +
  ylab("Documents") +
  my_theme()

#### Authorship position ####
# Plot
fig_authpos <-
  ggplot(df_all, aes(x = my_position, fill = first_auth)) +
  geom_hline(yintercept = seq(0, 18, by = 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_bar(color = "black") +
  scale_fill_manual(
    labels = c("First author", "Shared 1st authorship", "Other position"),
    values = c("First_shared" = "#4292c6", "First" = "#ef3b2c"),
    na.value = "ivory"
  ) +
  labs(
    title = "Published documents - Author position",
    subtitle = paste0("Period: ", period_min, "-", period_max, " (updated on ", last_updated, ")"),
    fill = ""
  ) +
  scale_x_continuous(breaks = seq(1, 18, by = 1)) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, by = 3)) +
  xlab("Authorship position (Moreira, DC)") +
  ylab("Documents") +
  my_theme() +
  theme(
    legend.position = c(0.8, 0.9),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
    )

#### Impact factor over the years - Colored by authorship####
# Plot
fig_if <-
  ggplot(df_all, aes(x = year, y = if_2023_jcr)) +
  geom_hline(yintercept = mean(df_all$if_2023_jcr, na.rm = T), linetype = "dashed") +
  geom_bar(aes(group = year), stat = "summary", fun = mean, fill = "ivory", color = "black") +
  geom_text(
    aes(
      label = paste0("Mean = ", round(mean(if_2023_jcr, na.rm = T), digits = 2)),
      x = min(year), y = mean(if_2023_jcr, na.rm = T) * 1.1
    ),
    check_overlap = T
  ) +
  geom_jitter(aes(color = first_auth), alpha = 0.75) +
  scale_color_manual(
    labels = c("First author", "Shared 1st authorship", "3"),
    values = c("First_shared" = "#4292c6", "First" = "#ef3b2c"),
    na.value = "gray15"
  ) +
  geom_text_repel(
    data = subset(df_all, if_2023_jcr > 5.9 | if_2023_jcr < 1),
    aes(label = journal_abbr)
  ) +
  scale_x_continuous(limits = c(period_min - 0.5, period_max + 0.5), breaks = seq(period_min, period_max, by = 1)) +
  scale_y_continuous(
    limits = c(0, round_any(max(df_all$if_2023_jcr, na.rm = T), 2, f = ceiling)),
    breaks = seq(0, round_any(max(df_all$if_2023_jcr, na.rm = T), 2, f = ceiling), length.out = 5)
  ) +
  labs(
    title = "Published documents - Journal Impact Factor",
    subtitle = paste0("Period: ", period_min, "-", period_max, " (updated on ", last_updated, ")"),
    fill = ""
  ) +
  xlab("Year") +
  ylab("Journal Impact Factor (JCR, 2023)") +
  my_theme()

#### Number of publications in per publisher ####
# Tidying data
dt_publisher <- count(df_all, publisher) %>% 
  arrange(desc(n)) %>% 
  mutate(fill_map = as.factor(row_number()))

# Plot
# Use the code below to (i) count the number of grouping vars, 
# (ii) select the range from which to draw colors, and
# (iii) get the number of colors necessary within the range of colors
color_count <-  length(unique(na.omit(dt_publisher$publisher))) # Counts the number of grouping vars
get_palette <- colorRampPalette(brewer.pal(9, "Spectral")) # Defines the range from which to generate colors
get_palette(color_count) # Generates the colors 

fig_alldocs_publisher <-
  ggplot(dt_publisher, aes(x = reorder(publisher, 1 / n), y = n, fill = fill_map)) +
  geom_col(color = "gray25") +
  geom_text(aes(label = n), nudge_y = 0.75) +
  scale_fill_manual(values = get_palette(color_count)) +
  labs(
    title = "Published documents - Publisher",
    subtitle = paste0(
      "Period: ", period_min, "-", period_max,
      " (updated on ", last_updated, ")"
    )
  ) +
  xlab("Publisher") +
  ylab("Documents") +
  my_theme() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### Number of publications in per issue type ####
# Tidying data
dt_issue_type <- count(df_all, issue_type) %>% 
  arrange(n) %>% 
  mutate(fill_map = as.factor(row_number()))

issue_type_label <- c(
  "special_issue" = "Special Issue", "regular_issue" = "Regular Issue",
  "book" = "Book"
)

# Plot
fig_alldocs_issue_type <-
  ggplot(dt_issue_type, aes(x = reorder(issue_type, 1 / n), y = n, fill = fill_map)) +
  geom_col(color = "gray25") +
  geom_text(aes(label = n), nudge_y = 2) +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Published documents - Issue type",
    subtitle = paste0(
      "Period: ", period_min, "-", period_max,
      " (updated on ", last_updated, ")"
    )
  ) +
  scale_x_discrete(labels = issue_type_label) +
  xlab("Issue type") +
  ylab("Documents") +
  my_theme()

#### Number of publications in per type of publication ####
# Tidying data
dt_type <- count(df_all, type) %>%
  arrange(desc(n)) %>% 
  mutate(fill_map = as.factor(row_number()))

doc_type_label <- c(
  "article" = "Original\nResearch", "editorial" = "Editorial",
  "review" = "Review", "chapter" = "Book\nChapter",
  "commentary" = "Commentary", "opinion" = "Opinion",
  "perspective" = "Perspective"
)

# Plot
fig_alldocs_type <-
  ggplot(dt_type, aes(x = reorder(type, 1 / n), y = n, fill = fill_map)) +
  geom_col(color = "gray25") +
  geom_text(aes(label = n), nudge_y = 1.5) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_discrete(labels = doc_type_label) +
  labs(
    title = "Published documents - Article type",
    subtitle = paste0("Period: ", period_min, "-", period_max, " (updated on ", last_updated, ")")
  ) +
  xlab("Document type") +
  ylab("Documents") +
  my_theme()

#### Number of publications in per Qualis classification ####
# Tidying data
dt_qualis <- count(df_all, qualis_2017_2020) %>% 
  arrange(qualis_2017_2020) %>%
  mutate(fill_map = as.factor(row_number()))

# Plot
fig_qualis <-
  ggplot(dt_qualis, aes(x = qualis_2017_2020, y = n, fill = fill_map)) +
  geom_col(color = "gray25") +
  geom_text(aes(label = n), nudge_y = 1.5) +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Published documents - Qualis CAPES",
    subtitle = paste0("Period: ", period_min, "-", period_max, " (updated on ", last_updated, ")")
  ) +
  xlab("Qualis CAPES (2017-2020 evaluation)") +
  ylab("Documents") +
  my_theme()

#### Number of publications in per Scimago Quartile ####
# Tidying data
dt_quartile <- count(df_all, quartile_2023) %>%
  arrange(quartile_2023) %>% 
  mutate(fill_map = as.factor(row_number()))

# Plot
fig_quartile <-
  ggplot(dt_quartile, aes(x = quartile_2023, y = n, fill = fill_map)) +
  geom_col(color = "gray25") +
  geom_text(aes(label = n), nudge_y = 1.5) +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Published documents - SJR, SCImago",
    subtitle = paste0("Period: ", period_min, "-", period_max, " (updated on ", last_updated, ")")
  ) +
  xlab("Quartile (2023 evaluation)") +
  ylab("Documents") +
  my_theme()

#### Number of publications per Journal with IF_2023 ####
# Tidying data
dt_journal <- count(df_all, journal_abbr) %>% 
  arrange(n) %>%
  left_join(df_journals, by = "journal_abbr")

# Plot
fig_alldocs_journal <-
  ggplot(dt_journal, aes(y = fct_reorder(journal_abbr, 1 / n, .desc = T) %>% fct_inorder(), 
                         x = n, fill = n)) +
  geom_vline(xintercept = seq(1, max(dt_journal$n, na.rm = T) + 2, by = 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_col(color = "gray25") +
  scale_fill_gradient(low = "ivory", high = "#4292c6") +
  geom_text(aes(label = format(if_2023_jcr, nsmall = 2)), nudge_x = 0.5) +
  labs(
    title = "Published documents - Journal",
    subtitle = paste0("Period: ", period_min, "-", period_max, " (updated on ", last_updated, ")")
  ) +
  xlab("Documents") +
  ylab("") +
  scale_x_continuous(
    limits = c(0, max(dt_journal$n, na.rm = T) + 2),
    breaks = seq(0, max(dt_journal$n, na.rm = T) + 2, by = 2)
  ) +
  my_theme()

#### Number of publications in per Corresponding author ####
# Splitting multiple corresponding authors
df_corrauth <- df_all %>%
  select(year, title, corresp_auth) %>%
  separate_wider_delim(corresp_auth,
    delim = ";", too_few = "align_start",
    names = c(sprintf("corresp_author_%d", seq(1:4)))
  )

df_corrauth <- df_corrauth %>%
  gather(key = "name", value = "corresp_auth", -year, -title)

# Tidying data
dt_corrauth <- df_corrauth %>%
  group_by(corresp_auth) %>%
  summarize(n = n()) %>%
  filter(!is.na(corresp_auth))

# Plot
fig_corrauth <-
  ggplot(dt_corrauth, aes(y = reorder(corresp_auth, 1 / n), x = n, fill = n)) +
  geom_vline(xintercept = seq(1, round_any(max(dt_corrauth$n), 5, f = ceiling), by = 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_col(color = "gray25") +
  scale_fill_gradient(low = "ivory", high = "#4292c6") +
  geom_text(aes(label = n), nudge_x = 0.75) +
  labs(
    title = "Published documents - Corresponding author",
    subtitle = paste0(
      "Period: ", period_min, "-", period_max,
      " (updated on ", last_updated, ")"
    )
  ) +
  xlab("Documents") +
  ylab("") +
  scale_x_continuous(limits = c(0, round_any(max(dt_corrauth$n), 5, f = ceiling)), breaks = seq(0, round_any(max(dt_corrauth$n), 5, f = ceiling), by = 5)) +
  my_theme()

#### Times Cited, All Databases Web of Science ####
# Plot
fig_citations_wos <-
  ggplot(subset(df_all, citations_wos > 0), aes(x = citations_wos, y = reorder(title_trunc, 1 / citations_wos))) +
  geom_vline(xintercept = seq(0, round_any(max(df_all$citations_wos, na.rm = T), 100, f = ceiling), by = 20), linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_col(aes(fill = citations_wos), color = "gray25") +
  scale_fill_gradient(low = "ivory", high = "#6a51a3") +
  scale_x_continuous(
    limits = c(0, round_any(max(df_all$citations_wos, na.rm = T), 100, f = ceiling)),
    breaks = seq(0, round_any(max(df_all$citations_wos, na.rm = T), 100, f = ceiling), length.out = 5)
  ) +
  geom_text(aes(label = citations_wos), nudge_x = max(df_all$citations_wos, na.rm = T) / 100, hjust = 0) +
  geom_vline(xintercept = h_index_wos, color = "#3f007d", linetype = "dashed", linewidth = 0.25) +
  geom_text_npc(aes(npcx = 0.5, npcy = 0.95, label = paste("Current h-index (WoS):", h_index_wos)), check_overlap = T, color = "#3f007d", hjust = 0.5) +
  labs(
    title = "Citations, all bases - Web of Science",
    subtitle = paste0("Updated on ", last_updated)
  ) +
  ylab("") +
  xlab("Citations") +
  my_theme()

#### Times Cited per Year since publication, All Databases Web of Science ####
# Calculating average citations per year since publication
df_all <- df_all %>%
  mutate(cites_per_yr_wos = if_else(year == as.numeric(format(Sys.Date(), "%Y")),
    citations_wos,
    citations_wos / (as.numeric(format(Sys.Date(), "%Y")) - year)
  ))

# Plot
fig_citationsyr_wos <-
  ggplot(subset(df_all, cites_per_yr_wos > 0), aes(x = cites_per_yr_wos, y = reorder(title_trunc, 1 / cites_per_yr_wos))) +
  geom_vline(
    xintercept = seq(0, round_any(max(df_all$cites_per_yr_wos, na.rm = T), 25, f = ceiling), by = 2),
    linetype = "dashed", color = "gray", alpha = 0.5
  ) +
  geom_col(aes(fill = cites_per_yr_wos), color = "gray25") +
  scale_fill_gradient(low = "ivory", high = "#6a51a3") +
  scale_x_continuous(
    limits = c(0, round_any(max(df_all$cites_per_yr_wos, na.rm = T), 25, f = ceiling)),
    breaks = seq(0, round_any(max(df_all$cites_per_yr_wos, na.rm = T), 25, f = ceiling), length.out = 5)
  ) +
  geom_text(aes(label = round(cites_per_yr_wos, digits = 1)), nudge_x = 1) +
  labs(
    title = "Average citations per year - Web of Science",
    subtitle = paste0("Updated on ", last_updated)
  ) +
  ylab("") +
  xlab("Citations (citations/yr)") +
  my_theme()

#### Times Cited, All Databases Scopus ####
fig_citations_scopus <-
  ggplot(subset(df_all, citations_scopus > 0), aes(x = citations_scopus, y = reorder(title_trunc, 1 / citations_scopus))) +
  geom_vline(
    xintercept = seq(0, round_any(max(df_all$citations_scopus, na.rm = T), 100, f = ceiling), by = 20),
    linetype = "dashed", color = "gray", alpha = 0.5
  ) +
  geom_col(aes(fill = citations_scopus), color = "gray25") +
  scale_fill_gradient(low = "ivory", high = "#f16913") +
  scale_x_continuous(
    limits = c(0, round_any(max(df_all$citations_scopus, na.rm = T), 100, f = ceiling)),
    breaks = seq(0, round_any(max(df_all$citations_scopus, na.rm = T), 100, f = ceiling), length.out = 5)
  ) +
  geom_text(aes(label = citations_scopus), nudge_x = max(df_all$citations_scopus, na.rm = T) / 100, hjust = 0) +
  geom_vline(xintercept = h_index_scopus, color = "#7f2704", linetype = "dashed", linewidth = 0.25) +
  geom_text_npc(aes(npcx = 0.5, npcy = 0.95, label = paste("Current h-index (Scopus):", h_index_scopus)), check_overlap = T, color = "#7f2704", hjust = 0.5) +
  labs(
    title = "Citations - Scopus",
    subtitle = paste0("Updated on ", last_updated)
  ) +
  ylab("") +
  xlab("Citations") +
  my_theme()

#### Times Cited per Year since publication, All Databases Scopus ####
# Calculating average citations per year since publication
df_all <- df_all %>%
  mutate(cites_per_yr_scopus = if_else(year == as.numeric(format(Sys.Date(), "%Y")),
    citations_scopus,
    citations_scopus / (as.numeric(format(Sys.Date(), "%Y")) - year)
  ))

# Plot
fig_citationsyr_scopus <-
  ggplot(subset(df_all, cites_per_yr_scopus > 0), aes(x = cites_per_yr_scopus, y = reorder(title_trunc, 1 / cites_per_yr_scopus))) +
  geom_vline(
    xintercept = seq(0, round_any(max(df_all$cites_per_yr_scopus, na.rm = T), 25, f = ceiling), by = 2),
    linetype = "dashed", color = "gray", alpha = 0.5
  ) +
  geom_col(aes(fill = cites_per_yr_scopus), color = "gray25") +
  scale_fill_gradient(low = "ivory", high = "#f16913") +
  scale_x_continuous(
    limits = c(0, round_any(max(df_all$cites_per_yr_scopus, na.rm = T), 25, f = ceiling)),
    breaks = seq(0, round_any(max(df_all$cites_per_yr_scopus, na.rm = T), 25, f = ceiling), length.out = 5)
  ) +
  geom_text(aes(label = round(cites_per_yr_scopus, digits = 1)), nudge_x = 1) +
  labs(
    title = "Average citations per year - Scopus",
    subtitle = paste0("Updated on ", last_updated)
  ) +
  ylab("") +
  xlab("Citations (citations/yr)") +
  my_theme()

#### Arranging Figures ####
all_figs_a <- ggarrange(fig_alldocs_journal, fig_corrauth,
  fig_citations_wos, fig_citations_scopus,
  nrow = 2, ncol = 2, align = "hv"
)

all_figs_b <- ggarrange(fig_alldocs, fig_cumdocs,
  fig_quartile, fig_alldocs_type,
  fig_authpos, fig_if,
  nrow = 3, ncol = 2, align = "hv"
)

all_figs_c <- ggarrange(fig_qualis, fig_alldocs_publisher, fig_alldocs_issue_type,
  nrow = 1, ncol = 3, align = "hv"
)

#### Saving figures ####
if (!dir.exists("plot")) {
  dir.create("plot")
}

save_plot(all_figs_a, "sci_output_a", width = 40, height = 35)
save_plot(all_figs_b, "sci_output_b", width = 30, height = 30)
save_plot(all_figs_c, "sci_output_c", width = 30, height = 12)

#### Saving all plots generated in the environment ####
# Save individual plots (if necessary)
plot_names <- ls(pattern = "^fig_")

for (plot_name in plot_names) {
  plot <- get(plot_name)
  save_plot(plot, plot_name)
}
