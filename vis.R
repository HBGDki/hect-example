library(dplyr)
library(ggplot2)
library(extrafont)

extrafont::font_import("font", prompt = FALSE)

# load("data/sdat.Rdata")
load("data/sdat2.Rdata")

lookup <- c(
  "0.002" = "worst",
  "0.02"  = "worst",
  "0.09"  = "worst",
  "0.01"  = "base",
  "0.05"  = "base",
  "0.12"  = "base",
  "0.015" = "best",
  "0.08"  = "best",
  "0.15"  = "best"
)

plookup <- c(
  "pow1" = "s",
  "pow2"  = "p1",
  "pow3"  = "p2"
)

## prepare data
##---------------------------------------------------------

## sample size summaries
mnt <- sdat %>%
  group_by(s, p1, p2, RRR, sup, fut) %>%
  summarise(
    nt = mean(Nt),
    nt_m = median(Nt),
    nt_q1 = quantile(Nt, 0.25),
    nt_q3 = quantile(Nt, 0.75)
  ) %>%
  mutate(
    CER = unname(lookup[as.character(s)]),
    CER = factor(CER, levels = c("worst", "base", "best")))

# stack by pow variables
sdat2 <- sdat %>%
  tidyr::gather(pow, val, pow1:pow3) %>%
  mutate(
    outcome = unname(plookup[pow]),
    outcome = factor(outcome, levels = c("s", "p1", "p2")))

## power summaries
mpow <- sdat2 %>%
  filter(RRR != 0) %>%
  group_by(s, p1, p2, RRR, sup, fut, outcome) %>%
  tidyr::nest() %>%
  mutate(d = purrr::map(data, function(x) {
    bn <- binom::binom.bayes(sum(x$val), length(x$val))
    data_frame(
      pow = mean(x$val),
      pow_q1 = bn$lower,
      pow_q3 = bn$upper
    )
  })) %>%
  select(-data) %>%
  tidyr::unnest() %>%
  mutate(
    CER = unname(lookup[as.character(s)]),
    CER = factor(CER, levels = c("worst", "base", "best")))

## type I error summaries
mpv <- sdat2 %>%
  filter(RRR == 0) %>%
  group_by(s, p1, p2, sup, fut, outcome) %>%
  tidyr::nest() %>%
  mutate(d = purrr::map(data, function(x) {
    message(as.character(x$sim_num[1]))
    bn <- binom::binom.bayes(sum(x$val), length(x$val))
    data_frame(
      pval = mean(x$val),
      pval_q1 = bn$lower,
      pval_q3 = bn$upper
    )
  })) %>%
  select(-data) %>%
  tidyr::unnest() %>%
  mutate(
    CER = unname(lookup[as.character(p2)]),
    CER = factor(CER, levels = c("worst", "base", "best")))

## make plots
##---------------------------------------------------------

doplot <- FALSE

thm <- theme(
  text = element_text(family = "Open Sans"), #, color = "grey20")
  plot.title = element_text(family = "Open Sans Semibold", size = 12),
  # text = element_text(family = "DINOT-Regular"), #, color = "grey20")
  # plot.title = element_text(family = "DINOT-Bold", size = 12),
  # title = element_text(family = "Open Sans Semibold"), #, color = "grey20")
  # text = element_text(family = "Alte DIN 1451 Mittelschrift"), #, color = "grey20")
  # text = element_text(family = "Lato"), #, color = "grey20")
  # text = element_text(family = "Gidole"), #, color = "grey20")
  strip.background = element_rect(fill = "#999999"),
  strip.text = element_text(color = "white")
)

# dev size: 6.25x4.5
p1 <- ggplot(mnt, aes(factor(sup), nt_m, color = factor(fut), ymin = nt_q1, ymax = nt_q3)) +
  geom_pointrange(position = position_dodge(width = 0.6), size = 0.35) +
  facet_grid(CER ~ RRR, labeller = label_both) +
  labs(
    title = "1. Expected sample size at trial termination",
    x = "superiority threshold",
    y = "sample size",
    color = "futility\nthreshold") +
  ylim(0, NA) +
  theme_bw() +
  ggthemes::scale_colour_tableau() +
  theme(panel.grid.major.x = element_line(size = rel(15)), legend.position = "none") +
  thm

if (doplot)
  print(p1)

# dev size: 5.75x4.5
p2 <- ggplot(mpv, aes(factor(sup), pval, color = factor(fut), ymin = pval_q1, ymax = pval_q3)) +
  # geom_point(position = position_dodge(width = 0.6)) +
  geom_pointrange(position = position_dodge(width = 0.6), size = 0.35) +
  geom_hline(yintercept = 0.05, linetype = 2) +
  facet_grid(CER ~ outcome, labeller = label_both) +
  labs(
    title = "2. Type I error rate based on highly permissive outcome",
    x = "superiority threshold",
    y = "type I error rate",
    color = "futility\nthreshold") +
  ylim(0, NA) +
  theme_bw() +
  ggthemes::scale_colour_tableau() +
  theme(panel.grid.major.x = element_line(size = rel(15))) +
  thm

if (doplot)
  print(p2)

# looks good with dev size 12x4.5
p3 <- ggplot(mpow, aes(factor(sup), pow, color = factor(fut), ymin = pow_q1, ymax = pow_q3)) +
  # geom_point(position = position_dodge(width = 0.6)) +
  geom_pointrange(position = position_dodge(width = 0.6), size = 0.35) +
  geom_hline(yintercept = 0.8, linetype = 2) +
  facet_grid(CER ~ RRR + outcome, labeller = label_both) +
  labs(
    title = "3. Power to detect a difference when monitoring for the highly permissive outcome",
    x = "superiority threshold",
    y = "power",
    color = "futility\nthreshold") +
  ylim(0, NA) +
  theme_bw() +
  ggthemes::scale_colour_tableau() +
  theme(panel.grid.major.x = element_line(size = rel(15)), legend.position = "none") +
  thm

height <- 1 # Vertical spacing
aux <- 1e-5 # Auxiliary number to identify 'height' among other heights
width <- 0.1 # Desirable horizontal spacing

p3 <- p3 + theme(panel.spacing = unit(height + aux, "lines"))
gt <- ggplot_gtable(ggplot_build(p3))
idx <- sapply(gt$widths, function(x) is.numeric(x) && as.numeric(x) == (height + aux))
idx2 <- which(idx)[c(1, 2, 4, 5, 7, 8)]
gt$widths[idx2] <- unit(0.1, "lines")
idx3 <- sapply(gt$heights, function(x) is.numeric(x) && as.numeric(x) == (height + aux))
gt$heights[idx3] <- unit(0.1, "lines")

if (doplot)
  grid::grid.draw(gt)

## plot them all together
##---------------------------------------------------------

dev.new(width = 12, height = 9)

top_row <- cowplot::plot_grid(p1, p2, rel_widths = c(12 - 5.75, 5.75))
cowplot::plot_grid(top_row, gt, ncol = 1)

ggsave(file = "plots/detail.pdf")

## visualize time
##---------------------------------------------------------

tb <- sdat %>%
  group_by(Nt) %>%
  summarise(n = n()) %>%
  arrange(Nt) %>%
  mutate(
    cn = cumsum(n),
    pct = 100 * cn / max(cn),
    idx = seq_along(Nt)
  )

dev.new(width = 4.77, height = 2.2)

ggplot(tb, aes(idx, pct)) +
  geom_col(width = 1, fill = ggthemes::tableau_color_pal()(1)) +
  theme_bw() +
  scale_x_continuous(breaks = tb$idx) +
  labs(
    title = "Probability of ending at time t across all scenarios",
    x = "time (arbitrary units)",
    y = "probability of ending"
  ) +
  theme(
    panel.grid.minor.x = element_blank()
  ) +
  thm

ggsave(file = "plots/time_pct.pdf")

dev.new(width = 4.77, height = 2.2)

ggplot(tb, aes(idx, n)) +
  geom_col(width = 1, fill = ggthemes::tableau_color_pal()(1)) +
  theme_bw() +
  scale_x_continuous(breaks = tb$idx) +
  labs(
    title = "Number of scenarios ending at time t",
    x = "time (arbitrary units)",
    y = "number of scenarios"
  ) +
  theme(
    panel.grid.minor.x = element_blank()
  )

ggsave(file = "plots/time_n.pdf")

# CERs

cers <- tibble::tribble(
  ~outcome, ~cer, ~case,
  "s" , 0.002 , "worst",
  "p1", 0.02  , "worst",
  "p2", 0.09  , "worst",
  "s" , 0.01  , "base" ,
  "p1", 0.05  , "base" ,
  "p2", 0.12  , "base" ,
  "s" , 0.015 , "best" ,
  "p1", 0.08  , "best" ,
  "p2", 0.15  , "best"
)

cers$case <- factor(cers$case, levels = c("worst", "base", "best"))
cers$outcome <- factor(cers$outcome, levels = c("p2", "p1", "s"))

cers2 <- cers %>%
  filter(case %in% c("worst", "best")) %>%
  tidyr::spread(case, cer)

cls <- ggthemes::tableau_color_pal("tableau10")(4)
cls <- cls[c(4, 2, 3)]

ggplot(cers, aes(cer * 100, outcome, color = case)) +
  geom_segment(data = cers2, aes(x = worst * 100, xend = best * 100, y = outcome, yend = outcome),
    color = "darkgray", size = 1) +
  geom_point(size = 3) +
  labs(
    x = "Control Event Rate (CER)"
  ) +
  scale_color_manual(values = cls) +
  theme_bw() +
  thm

dev.new(width = 5.8, height = 1.7)
ggsave(file = "plots/cer.pdf")

## plots to match writeup
##---------------------------------------------------------

# ggplot(mnt, aes(factor(sup), nt, fill = factor(fut))) +
#   geom_col(position = "dodge") +
#   facet_grid(CER ~ RRR, labeller = label_both) +
#   labs(
#     x = "superiority threshold",
#     y = "expected sample size at trial termination",
#     fill = "futility\nthreshold")

# ggplot(mpv, aes(factor(fut), pval, fill = outcome)) +
#   geom_col(position = "dodge") +
#   facet_grid(factor(CER) ~ factor(sup))

# ggplot(mpow, aes(factor(sup), val, fill = factor(fut))) +
#   geom_col(position = "dodge") +
#   facet_grid(CER ~ RRR + outcome, labeller = label_both)


