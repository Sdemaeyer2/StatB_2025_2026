# Laad de benodigde libraries
library(ggplot2)
library(ggdist)
library(dplyr)
library(metRology) 

# Maak een dataframe aan met de verschillende verdelingen

theme_set(theme_linedraw() +
            theme(text = element_text(family = "Times", size = 8))
)

Tverdeling1 <- ggplot() +
  stat_function(
    fun = dt.scaled,
    args = list(df = 5, mean = 0, sd = 1),
    xlim = c(-3, 3),
    aes(color = "df = 5")
  ) +
  stat_function(
    fun = dt.scaled,
    args = list(df = 50, mean = 0, sd = 1),
    xlim = c(-3, 3),
    aes(color = "df = 50")
  ) +
  scale_color_manual(
    name   = "Vrijheidsgraden",
    values = c("df = 5"  = "#D55E00",  # orange (Okabe–Ito)
               "df = 50" = "#0072B2")  # blue   (Okabe–Ito)
  ) +
  scale_y_continuous(name = "dichtheid", labels = NULL) +
  labs(title = "t-verdelingen")


Tverdeling1

library(ggplot2)
library(metRology)

df_val <- 5
cutoff <- 2

# Raster + dichtheid
d <- data.frame(x = seq(-3, 3, length.out = 2000))
d$y <- dt.scaled(d$x, df = df_val, mean = 0, sd = 1)
d$tail <- abs(d$x) > cutoff

# Twee-zijdige kans P(|T| > cutoff)
p_tail <- 1 - (pt.scaled(cutoff, df = df_val, mean = 0, sd = 1) -
               pt.scaled(-cutoff, df = df_val, mean = 0, sd = 1))

label_text <- paste0(
  "P(|T| > ", cutoff, ") = ",
  round(p_tail, 3)
)

#### MET SHADE

library(ggplot2)
library(metRology)


df_val <- 50
cutoff <- 2

curve_lab <- paste0("t-verdeling (df = ", df_val, ")")
tail_lab  <- paste0("|t| > ", cutoff)

d <- data.frame(x = seq(-3, 3, length.out = 2000))
d$y <- dt.scaled(d$x, df = df_val, mean = 0, sd = 1)

d_left  <- subset(d, x <= -cutoff)
d_right <- subset(d, x >=  cutoff)

# Twee-zijdige kans
p_tail <- 1 - (pt.scaled(cutoff, df = df_val, mean = 0, sd = 1) -
               pt.scaled(-cutoff, df = df_val, mean = 0, sd = 1))
label_text <- paste0("P(|t| > ", cutoff, ") = ", round(p_tail, 3))

Tverdeling_tail <- ggplot(d, aes(x, y)) +
  # staarten (2 aparte geoms)
  geom_area(
    data = d_left,
    aes(fill = tail_lab),
    alpha = 0.35
  ) +
  geom_area(
    data = d_right,
    aes(fill = tail_lab),
    alpha = 0.35
  ) +
  # curve
  geom_line(
    aes(color = curve_lab),
    linewidth = 1
  ) +
  geom_vline(xintercept = c(-cutoff, cutoff), linetype = "dashed") +
  annotate(
    "label",
    x = 0,
    y = max(d$y) * 0.85,
    label = label_text,
    size = 4,
    fill = "white"
  ) +
  coord_cartesian(xlim = c(-3, 3)) +
  scale_color_manual(
    name = NULL,
    values = setNames(c("#0072B2"), curve_lab)
  ) +
  scale_fill_manual(
    name = NULL,
    values = setNames(c("#D55E00"), tail_lab)
  ) +
  labs(
    title = "t-verdeling met p-waarde ingekleurd",
    x = "t",
    y = "density"
  ) +
  scale_y_continuous(name = "dichtheid", labels = NULL)

Tverdeling_tail

### AFVAL VB...

df_val <- 198
cutoff <- 5.251

curve_lab <- paste0("t-verdeling (df = ", df_val, ")")
tail_lab  <- paste0("|t| > ", cutoff)

d <- data.frame(x = seq(-6, 6, length.out = 3000))
d$y <- dt.scaled(d$x, df = df_val, mean = 0, sd = 1)

d_left  <- subset(d, x <= -cutoff)
d_right <- subset(d, x >=  cutoff)

# Twee-zijdige kans
p_tail <- 1 - (pt.scaled(cutoff, df = df_val, mean = 0, sd = 1) -
               pt.scaled(-cutoff, df = df_val, mean = 0, sd = 1))
label_text <- paste0("P(|t| > ", cutoff, ") < ", 0.05)

Tverdeling_tail <- ggplot(d, aes(x, y)) +
  # staarten (2 aparte geoms)
  geom_area(
    data = d_left,
    aes(fill = tail_lab),
    alpha = 0.35
  ) +
  geom_area(
    data = d_right,
    aes(fill = tail_lab),
    alpha = 0.35
  ) +
  # curve
  geom_line(
    aes(color = curve_lab),
    linewidth = 1
  ) +
  geom_vline(xintercept = c(-cutoff, cutoff), linetype = "dashed") +
  annotate(
    "label",
    x = 0,
    y = max(d$y) * 0.85,
    label = label_text,
    size = 4,
    fill = "white"
  ) +
  coord_cartesian(xlim = c(-6, 6)) +
  scale_color_manual(
    name = NULL,
    values = setNames(c("#0072B2"), curve_lab)
  ) +
  scale_fill_manual(
    name = NULL,
    values = setNames(c("#D55E00"), tail_lab)
  ) +
  labs(
    title = "t-verdeling voorbeeld afval experiment",
    x = "t",
    y = "density"
  ) +
  scale_y_continuous(name = "dichtheid", labels = NULL)

Tverdeling_tail

#### Extra voorbeeld deel 1


# ---- input: pas dit aan aan je eigen voorbeeld ----
n_t   <- 22; mean_t   <- 68; sd_t   <- 9
n_t1  <- 20; mean_t1  <- 73; sd_t1  <- 10

xlim <- c(-4, 4)      # plot range
grid_n <- 3000        # resolutie curve

# ---- Welch t-statistiek + df ----
se  <- sqrt(sd_t1^2 / n_t1 + sd_t^2 / n_t)
t0  <- (mean_t1 - mean_t) / se

df_welch <- (sd_t1^2 / n_t1 + sd_t^2 / n_t)^2 /
  ((sd_t1^2 / n_t1)^2 / (n_t1 - 1) + (sd_t^2 / n_t)^2 / (n_t - 1))

# Twee-zijdige p-waarde (met geschaalde pt)
p_two <- 2 * (1 - pt.scaled(abs(t0), df = 40, mean = 0, sd = 1))

# ---- data voor curve + staarten ----
d <- data.frame(x = seq(xlim[1], xlim[2], length.out = grid_n))
d$y <- dt.scaled(d$x, df = 40, mean = 0, sd = 1)

d_left  <- subset(d, x <= -abs(t0))
d_right <- subset(d, x >=  abs(t0))

# ---- labels (dynamisch) ----
curve_lab <- paste0("t-verdeling (df = ", round(40, 1), ")")
tail_lab  <- paste0("|t| ≥ ", round(abs(t0), 2))

label_text <- paste0(
  "t = ", round(t0, 2),
  "   df = ", round(40, 1),
  "\np(twee-zijdig) = ", signif(p_two, 3)
)

# Dynamische positie annotatie (rechtsboven, mee met x-range)
ann_x <- min(xlim[2] - 0.05 * diff(xlim), abs(t0) + 0.10 * diff(xlim))
ann_y <- max(d$y) * 0.92

# ---- plot ----
p <- ggplot(d, aes(x, y)) +
  geom_area(data = d_left,  aes(fill = tail_lab), alpha = 0.35) +
  geom_area(data = d_right, aes(fill = tail_lab), alpha = 0.35) +
  geom_line(aes(color = curve_lab), linewidth = 1) +
  geom_vline(xintercept = c(-abs(t0), abs(t0)), linetype = "dashed") +
  annotate(
    "label",
    x = 0,
    y = max(d$y) * 0.85,
    label = label_text,
    size = 4,
    fill = "white"
  ) +
  coord_cartesian(xlim = xlim) +
  scale_color_manual(name = NULL, values = setNames(c("#0072B2"), curve_lab)) +  # Okabe–Ito blauw
  scale_fill_manual(name = NULL, values = setNames(c("#D55E00"), tail_lab)) +    # Okabe–Ito oranje
  labs(
    title = "Interventie (2025) vs vorig jaar (2024): toets op verschil in gemiddelden",
    x = "t",
    y = "dichtheid"
  ) +
  scale_y_continuous(name = "dichtheid", labels = NULL)

p

#### Extra voorbeeld deel 2


# ---- input: pas dit aan aan je eigen voorbeeld ----
n_t   <- 88; mean_t   <- 68; sd_t   <- 9
n_t1  <- 80; mean_t1  <- 73; sd_t1  <- 10

xlim <- c(-4, 4)      # plot range
grid_n <- 3000        # resolutie curve

# ---- Welch t-statistiek + df ----
se  <- sqrt(sd_t1^2 / n_t1 + sd_t^2 / n_t)
t0  <- (mean_t1 - mean_t) / se

df_welch <- (sd_t1^2 / n_t1 + sd_t^2 / n_t)^2 /
  ((sd_t1^2 / n_t1)^2 / (n_t1 - 1) + (sd_t^2 / n_t)^2 / (n_t - 1))

# Twee-zijdige p-waarde (met geschaalde pt)
p_two <- 2 * (1 - pt.scaled(abs(t0), df = 166, mean = 0, sd = 1))

# ---- data voor curve + staarten ----
d <- data.frame(x = seq(xlim[1], xlim[2], length.out = grid_n))
d$y <- dt.scaled(d$x, df = 166, mean = 0, sd = 1)

d_left  <- subset(d, x <= -abs(t0))
d_right <- subset(d, x >=  abs(t0))

# ---- labels (dynamisch) ----
curve_lab <- paste0("t-verdeling (df = ", round(, 1), ")")
tail_lab  <- paste0("|t| ≥ ", round(abs(t0), 2))

label_text <- paste0(
  "t = ", round(t0, 2),
  "   df = ", round(166, 1),
  "\np(twee-zijdig) = ", signif(p_two, 3)
)

# Dynamische positie annotatie (rechtsboven, mee met x-range)
ann_x <- min(xlim[2] - 0.05 * diff(xlim), abs(t0) + 0.10 * diff(xlim))
ann_y <- max(d$y) * 0.92

# ---- plot ----
p <- ggplot(d, aes(x, y)) +
  geom_area(data = d_left,  aes(fill = tail_lab), alpha = 0.35) +
  geom_area(data = d_right, aes(fill = tail_lab), alpha = 0.35) +
  geom_line(aes(color = curve_lab), linewidth = 1) +
  geom_vline(xintercept = c(-abs(t0), abs(t0)), linetype = "dashed") +
  annotate(
    "label",
    x = 0,
    y = max(d$y) * 0.85,
    label = label_text,
    size = 4,
    fill = "white"
  ) +
  coord_cartesian(xlim = xlim) +
  scale_color_manual(name = NULL, values = setNames(c("#0072B2"), curve_lab)) +  # Okabe–Ito blauw
  scale_fill_manual(name = NULL, values = setNames(c("#D55E00"), tail_lab)) +    # Okabe–Ito oranje
  labs(
    title = "Interventie (2025) vs vorig jaar (2024): toets op verschil in gemiddelden",
    x = "t",
    y = "dichtheid"
  ) +
  scale_y_continuous(name = "dichtheid", labels = NULL)

p
