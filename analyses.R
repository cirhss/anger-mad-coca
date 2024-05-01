library(readxl)
library(tidyverse)
library(ggthemes)
library(ggtext)
library(ggrepel)
# library(udpipe)

# ud_eng <- udpipe::udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

genre <- c("BLOG", "WEB", "TV/M", "SPOK", "FIC", "MAG", "NEWS", "ACAD")
genre <- factor(genre, levels = c("BLOG", "WEB", "TV/M", "SPOK", "FIC", "MAG", "NEWS", "ACAD"))

gram <- c("verb.BASE", "verb.3SG", "verb.ED", "verb.ING")
gram <- factor(gram, levels = c("verb.BASE", "verb.3SG", "verb.ED", "verb.ING"))

# frequency ====
mad <- c(BLOG = 31.12, WEB = 31.68, `TV/M` = 101.92, SPOK = 34.56, FIC = 59.63, MAG = 28.92, NEWS = 23.03, ACAD = 8.41)
angry <- c(BLOG = 52.05, WEB = 50.07, `TV/M` = 62.77, SPOK = 69.66, FIC = 91.89, MAG = 37.28, NEWS = 40.04, ACAD = 13.98)
emofreq <- data.frame(ANGRY = angry, MAD = mad) |> 
  rownames_to_column(var = "genre") |> 
  mutate(genre = factor(genre, levels = c("BLOG", "WEB", "TV/M", "SPOK", "FIC", "MAG", "NEWS", "ACAD"))) |> 
  as_tibble() |> 
  pivot_longer(cols = -genre,
               names_to = "word",
               values_to = "relfreq") |> 
  mutate(word = factor(word, levels = c("ANGRY", "MAD")))

emofreq |> 
  ggplot(aes(x = genre, y = relfreq, fill = word)) +
  geom_col(position = "dodge") +
  ggthemes::scale_fill_excel_new() +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(y = "Relative frequency\n(per million words)",
       fill = "Words",
       x = "Genres in COCA",
       subtitle = "Relative frequency of “angry” and “mad” across genres")
# ggsave("figs/01-fig3-1-relfreq-angry-mad.png",
#        units = "in",
#        width = 7,
#        height = 4,
#        dpi = 300)

# grammatical profiles ====

# grm <- read_xlsx("../COCA_Dayu Saskara/COCA-INF-V-MADDEN-primahadi.xlsx",
#                  range = "A1:I9")
# write_tsv(grm, "grammatical_profile.tsv")
grm <- read_tsv("grammatical_profile.tsv")

grm
grmlong <- grm |> 
  pivot_longer(cols = -WORD,
               names_to = "genre",
               values_to = "relfreq") |> 
  rename(word = WORD) |> 
  separate(word, into = c("word", "gram"), sep = "_") |> 
  mutate(gram = factor(gram, levels = c("verb.BASE", "verb.3SG", "verb.ED", "verb.ING")),
         word = factor(word, levels = c("ANGER", "MADDEN")),
         genre = factor(genre, levels = c("BLOG", "WEB", "TV/M", "SPOK", "FIC", "MAG", "NEWS", "ACAD")))

## ANGER only across genres =====
grmlong |> 
  filter(word == "ANGER") |> 
  ggplot(aes(x = genre, y = relfreq, fill = gram)) + 
  geom_col(position = "dodge") +
  ggthemes::scale_fill_excel_new(theme = "Atlas") +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(y = "Relative frequency\n(per million words)",
       fill = "Inflectional forms",
       x = "Genres in COCA",
       subtitle = "Inflectional forms of the verbal lemma ANGER")
# ggsave("figs/02-fig3-2-anger-gram-profile.png",
#        units = "in",
#        width = 8,
#        height = 5,
#        dpi = 300)

## MAD only across genres =====
grmlong |> 
  filter(word == "MADDEN") |> 
  ggplot(aes(x = genre, y = relfreq, fill = gram)) + 
  geom_col(position = "dodge") +
  ggthemes::scale_fill_excel_new(theme = "Atlas") +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(y = "Relative frequency\n(per million words)",
       fill = "Inflectional forms",
       x = "Genres in COCA",
       subtitle = "Inflectional forms of the verbal lemma MADDEN")
# ggsave("figs/03-fig3-3-madden-gram-profile.png",
#        units = "in",
#        width = 8,
#        height = 5,
#        dpi = 300)

## BASE form of anger and mad =====

grmlong |> 
  filter(str_detect(gram, "BASE")) |> 
  ggplot(aes(x = genre, y = relfreq, fill = word)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(y = "Relative frequency\n(per million words)",
       fill = "Verb lemmas",
       x = "Genres in COCA",
       subtitle = "Distribution of the base verbal forms of “anger” and “madden”")
# ggsave("figs/04-fig3-4-BASE-form-comparison.png",
#        units = "in",
#        width = 8,
#        height = 5.5,
#        dpi = 300)


## -ING form of anger and mad =====

grmlong |> 
  filter(str_detect(gram, "ING")) |> 
  ggplot(aes(x = genre, y = relfreq, fill = word)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_light() +
  labs(y = "Relative frequency\n(per million words)",
       fill = "Verb lemmas",
       x = "Genres in COCA",
       subtitle = "Distribution of the -*ing* verbal forms of “anger” and “madden”") +
  theme(legend.position = "bottom", plot.subtitle = ggtext::element_markdown())
# ggsave("figs/05-fig3-5-ING-form-comparison.png",
#        units = "in",
#        width = 8,
#        height = 5.5,
#        dpi = 300)

## -ED form of anger and mad =====

grmlong |> 
  filter(str_detect(gram, "ED")) |> 
  ggplot(aes(x = genre, y = relfreq, fill = word)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_light() +
  labs(y = "Relative frequency\n(per million words)",
       fill = "Verb lemmas",
       x = "Genres in COCA",
       subtitle = "Distribution of the -*ed* verbal forms of “anger” and “madden”") +
  theme(legend.position = "bottom", plot.subtitle = ggtext::element_markdown())
# ggsave("figs/06-fig3-6-ED-form-comparison.png",
#        units = "in",
#        width = 8,
#        height = 5.5,
#        dpi = 300)

## The -S form of anger and mad =====

grmlong |> 
  filter(str_detect(gram, "3SG")) |> 
  ggplot(aes(x = genre, y = relfreq, fill = word)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_light() +
  labs(y = "Relative frequency\n(per million words)",
       fill = "Verb lemmas",
       x = "Genres in COCA",
       subtitle = "Distribution of the -*s* verbal forms of “anger” and “madden”") +
  theme(legend.position = "bottom", plot.subtitle = ggtext::element_markdown())
# ggsave("figs/07-fig3-7-S-form-comparison.png",
#        units = "in",
#        width = 8,
#        height = 5.5,
#        dpi = 300)

# collocates =====

# angry <- read_xlsx("right-collocates-analysis.xlsx", sheet = "angry") |>
#   filter(sem != "-",
#          str_detect(attraction, "green"))
# write_tsv(angry, "right-collocates-analysis-angry.tsv")
angry <- read_tsv("right-collocates-analysis-angry.tsv")

# mad <- read_xlsx("right-collocates-analysis.xlsx", sheet = "mad") |>
#   filter(sem != "-",
#          str_detect(attraction, "green"))
# write_tsv(mad, "right-collocates-analysis-mad.tsv")
mad <- read_tsv("right-collocates-analysis-mad.tsv")

# stims <- read_xlsx("stimulus-analyses.xlsx")
# write_tsv(stims, "stimulus-analysis.tsv")
stims <- read_tsv("stimulus-analysis.tsv")

x <- angry |> 
  group_by(sem) |> 
  summarise(score = sum(score)) |> 
  arrange(desc(score))
x
y <- mad |> 
  group_by(sem) |> 
  summarise(score = sum(score)) |> 
  arrange(desc(score))
y

angry <- angry |> 
  mutate(sem_fct = factor(sem, levels = x$sem))
mad <- mad |> 
  mutate(sem_fct = factor(sem, levels = y$sem))

angry |> 
  group_by(sem, sem2) |> 
  summarise(score = sum(score)) |> 
  arrange(desc(score))

angry |> 
  filter(sem_orig != "-") |> 
  group_by(sem) |> 
  summarise(score = sum(score)) |> 
  arrange(desc(score))

angry |> 
  filter(sem_orig == "-") |> 
  group_by(sem) |> 
  summarise(score = sum(score)) |> 
  arrange(desc(score))

## Experiencer only =====
zz <- angry |> 
  filter(experiencer == "y") |> 
  select(lemma, sem, sem2, angry = w1, mad = w2, word, score) |> 
  bind_rows(mad |> filter(experiencer == "y") |> select(lemma, sem, sem2, angry = w2, mad = w1, word, score)) |> 
  group_by(lemma, sem, word) |> 
  summarise(angry = sum(angry), mad = sum(mad), score = sum(score))
zz <- zz |> 
  mutate(angryLog10 = log10(angry + .005),
         madLog10 = log10(mad + .005))
sem_score_df <- zz |> 
  group_by(sem) |> 
  summarise(sem_score = sum(score)) |> 
  arrange(desc(sem_score))
zz <- zz |> 
  left_join(sem_score_df) |> 
  mutate(sem = factor(sem, levels = sem_score_df$sem))

zz |> 
  # mutate(lemma = if_else(attraction == "green" & score < 30, "", lemma)) |> 
  ggplot(aes(x = angryLog10, y = madLog10, colour = sem)) +
  geom_point() +
  geom_text_repel(aes(label = lemma), max.overlaps = 100, show.legend = FALSE) +
  # scale_colour_brewer(type = "div", palette = "Spectral") +
  theme_bw() +
  labs(y = "Log10 frequency with “mad”\n(y-axis)",
       x = "Log10 frequency with “angry”\n(x-axis)",
       colour = "Semantic fields",
       caption = "The x- and y-axes show the log (base 10) frequency co-occurrences\nof the collocates with “mad” and “angry” respectively.") +
  # scale_colour_manual(values = c("#7c260b", "#01a2d9", "#76c0c1", "#ee8f71")) +
  scale_colour_excel_new(theme = "Badge") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11)) +
  geom_segment(aes(x = -0.5, y = 0, xend = 0.5, yend = 0), colour = "red", linetype = 2) +
  geom_segment(aes(y = -0.5, x = 0, yend = 0.5, xend = 0), colour = "red", linetype = 2) +
  annotate("rect", xmin = 0.2, xmax = 3, ymin = -3, ymax = 1,
           alpha = .08, fill = "#7ad2f6") +
  annotate("rect", xmin = -2.5, xmax = 1.5, ymin = 1.1, ymax = 3,
           alpha = .07, fill = "firebrick") +
  annotate("text", x = 1.6, y = -1.15, label = "Typical Experiencer collocates for “angry”", colour = "#014d64", fontface = "bold") +
  annotate("text", x = -0.6, y = 2.5, label = "Typical Experiencer collocates for “mad”", colour = "firebrick", fontface = "bold") +
  guides(color = guide_legend(override.aes = list(size = 6.5)))
# ggsave("figs/08-fig3-8-Experiencer-collocates.png",
#        units = "in",
#        width = 10,
#        height = 6.5,
#        dpi = 600)

## Stimulus ====

# mad_with <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WIEaGIIwTPeWYXsy_co3TFRuA0QZcTUPmT6VhybMNOc/edit?usp=sharing", sheet = "mad with NOUN", range = "E1:G75")
# write_tsv(mad_with, "mad_with.tsv")
mad_with <- read_tsv("mad_with.tsv")

# mad_at <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WIEaGIIwTPeWYXsy_co3TFRuA0QZcTUPmT6VhybMNOc/edit?usp=sharing", sheet = "mad at NOUN", range = "E1:G101")
# write_tsv(mad_at, "mad_at.tsv")
mad_at <- read_tsv("mad_at.tsv")

# mad_over <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WIEaGIIwTPeWYXsy_co3TFRuA0QZcTUPmT6VhybMNOc/edit?usp=sharing", sheet = "mad over NOUN", range = "G1:I3")
# write_tsv(mad_over, "mad_over.tsv")
mad_over <- read_tsv("mad_over.tsv")

# mad_about <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WIEaGIIwTPeWYXsy_co3TFRuA0QZcTUPmT6VhybMNOc/edit?usp=sharing", sheet = "mad about NOUN", range = "D1:F62")
# write_tsv(mad_about, "mad_about.tsv")
mad_about <- read_tsv("mad_about.tsv")

# angry_with <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WIEaGIIwTPeWYXsy_co3TFRuA0QZcTUPmT6VhybMNOc/edit?usp=sharing", sheet = "angry with NOUN", range = "H1:J94")
# write_tsv(angry_with, "angry_with.tsv")
angry_with <- read_tsv("angry_with.tsv")

# angry_at <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WIEaGIIwTPeWYXsy_co3TFRuA0QZcTUPmT6VhybMNOc/edit?usp=sharing", sheet = "angry at NOUN", range = "E1:G101")
# write_tsv(angry_at, "angry_at.tsv")
angry_at <- read_tsv("angry_at.tsv")

# angry_over <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WIEaGIIwTPeWYXsy_co3TFRuA0QZcTUPmT6VhybMNOc/edit?usp=sharing", sheet = "angry over NOUN", range = "H1:J24")
# write_tsv(angry_over, "angry_over.tsv")
angry_over <- read_tsv("angry_over.tsv")

# angry_about <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WIEaGIIwTPeWYXsy_co3TFRuA0QZcTUPmT6VhybMNOc/edit?usp=sharing", sheet = "angry about NOUN", range = "A2:C86")
# write_tsv(angry_about, "angry_about.tsv")
angry_about <- read_tsv("angry_about.tsv")

stimulus <- bind_rows(mad_about, mad_at, mad_over, mad_with, angry_about, angry_at, angry_over, angry_with) |> 
  rename(no = NO,
         pattern = PATTERNS,
         freq = FREQ) |> 
  mutate(word = if_else(str_detect(pattern, "^MAD"), "mad", "angry"),
         nouns = str_replace(pattern, "^(MAD|ANGRY)\\s(AT|ABOUT|OVER|WITH)\\s", ""),
         nouns = tolower(nouns),
         cxn = str_c(tolower(str_extract(pattern, "^(MAD|ANGRY)\\s(AT|ABOUT|OVER|WITH)\\s")), "NOUN", sep = ""),
         preps = str_replace(cxn, "(angry|mad)", "adj"),
         preps = factor(preps, levels = c("adj at NOUN", "adj with NOUN", "adj about NOUN", "adj over NOUN")))

### Frequency of the constructional patterns per adjective ====
stim_cxn_count <- stimulus |> 
  group_by(word, cxn, preps) |> 
  summarise(n = sum(freq)) |> 
  arrange(word, cxn) |> 
  mutate(perc = round(n/sum(n) * 100, 1)) |> 
  arrange(word, desc(perc))

stim_cxn_count |> 
  ggplot(aes(x = word, y = n, fill = preps)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = ifelse(n < 10, "", paste("n=", n, sep = ""))), colour = ifelse(stim_cxn_count$preps == "adj at NOUN", "white", "black"), position = position_fill(vjust = .5)) +
  theme_bw() +
  scale_fill_tableau() +
  labs(y = "Proportion",
       x = "Words",
       fill = "Constructional patterns")
# ggsave("figs/10-fig3-10-construction-for-stimulus.png",
#        units = "in",
#        width = 6.5,
#        height = 4,
#        dpi = 600)

# stims_lemma <- udpipe::udpipe(stimulus$nouns, "english")
# stims_lemma |>
#   write_tsv("stims_lemma.tsv")
stims_lemma <- read_tsv("stims_lemma.tsv") |> 
  as_tibble() |> 
  mutate(lemma = replace(lemma, lemma == "bannks", "bank")) |> 
  mutate(lemma = if_else(lemma %in% c("cars", "pets", "brows",
                                      "italians", "dicks",
                                      "mexicans", "members",
                                      "parks", "adults"),
                         str_replace(lemma, "s$", ""),
                         lemma),
         lemma = if_else(token %in% c("drawing", "cycling",
                                      "longing", "feeling", "polling"),
                         token,
                         lemma))

stims_lemma_over3 <- stims |> 
  group_by(word, lemma, sem) |> 
  summarise(n = sum(freq)) |> 
  arrange(desc(word), desc(n))
stims_lemma_over3 |> 
  pivot_wider(names_from = "word", values_from = "n", values_fill = 0) |> 
  mutate(madLog10 = log10(mad + .005),
         angryLog10 = log10(angry + .005)) |> 
  ggplot(aes(x = angryLog10, y = madLog10, colour = sem)) +
  geom_point() +
  geom_text_repel(aes(label = lemma), max.overlaps = 100, show.legend = FALSE, fontface = "bold") +
  # scale_colour_brewer(type = "qual", palette = "Paired") +
  scale_colour_tableau(palette = "Red-Blue-Brown") +
  theme_bw() +
  labs(y = "Log10 frequency with “mad”\n(y-axis)",
       x = "Log10 frequency with “angry”\n(x-axis)",
       colour = "Semantic fields",
       title = "Distribution of the NOUN collocates of the Stimulus and their semantic fields",
       subtitle = "(Data from the combined patterns [angry/mad *at*/*with*/*about*/*over* + NOUN])",
       caption = "The x- and y-axes show the log (base 10) frequency co-occurrences\nof the collocates with “mad” and “angry” respectively.") +
  # scale_colour_manual(values = c("#7c260b", "#01a2d9", "#76c0c1", "#ee8f71")) +
  # scale_colour_excel_new(theme = "Badge") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11),
        # panel.background = element_rect(fill = "gray97"),
        plot.subtitle = element_markdown()) +
  guides(color = guide_legend(override.aes = list(size = 6.5))) +
  geom_segment(aes(x = -0.25, y = 0, xend = 0.25, yend = 0), colour = "red", linetype = 2) +
  geom_segment(aes(y = -0.25, x = 0, yend = 0.25, xend = 0), colour = "red", linetype = 2) +
  geom_curve(aes(y = -0.5, x = -2.175, yend = 1.7, xend = 1.4), alpha = .2, curvature = .185, linewidth = .35, colour = "firebrick") +
  geom_curve(aes(y = -2.1, x = -0.5, yend = 1.7, xend = 1.6), alpha = .2, curvature = -.1, linewidth = .35, colour = "#014d64") +
  annotate("text", x = 0.6, y = -1.15, label = "More frequent Stimulus collocates for “angry”", colour = "#014d64", fontface = "bold", alpha = .5) +
  annotate("text", x = -0.5, y = 1.1, label = "More frequent Stimulus collocates for “mad”", colour = "firebrick", fontface = "bold", alpha = .5)
# ggsave("figs/09-fig3-9-Stimulus-collocates.png",
#        units = "in",
#        width = 10,
#        height = 6.5,
#        dpi = 600)

stim_lemma_count <- stims |> 
  group_by(lemma) |> 
  summarise(freq = sum(freq)) |> 
  arrange(desc(freq)) |> 
  ungroup()

stims |> 
  mutate(lemma = factor(lemma, levels = stim_lemma_count$lemma),
         lemma = fct_rev(lemma)) |> 
  mutate(pattern = str_replace(pattern, "^(mad|angry)\\s", "adj "),
         pattern = factor(pattern, levels = c("adj at NOUN", "adj with NOUN", "adj about NOUN", "adj over NOUN"))) |> 
  ggplot(aes(x = lemma, y = freq, fill = pattern)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~word) +
  theme_bw() +
  scale_fill_tableau(palette = "Red-Blue-Brown") +
  labs(fill = "Constructions",
       x = "Noun collocates",
       y = "Raw frequency in the construction",
       caption = "The collocates are ordered by their combined frequency across the two words.\nOnly nouns appearing at least three times are included in this graph.") +
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11),
        strip.text.x.top = element_text(size = 14),
        plot.caption = element_text(size = 11))
# ggsave("figs/11-fig3-11-Stimulus-collocates-and-construction.png",
#        units = "in",
#        width = 9,
#        height = 7.25,
#        dpi = 600)


# 
# 
# 
# stim_cxn_count <- stims |> 
#   group_by(word, pattern) |> 
#   summarise(n = sum(freq)) |> 
#   arrange(word, pattern) |> 
#   mutate(perc = round(n/sum(n) * 100, 1)) |> 
#   arrange(word, desc(perc))
# stim_cxn_count
