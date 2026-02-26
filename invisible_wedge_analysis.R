# =============================================================================
# Invisible Wedge: Measles-Related Permanent Hearing Loss Analysis
# Clean reproducible script — February 2026
# =============================================================================

library(tidyverse)
library(nhanesA)
library(ggplot2)
library(gridExtra)
library(scales)

# Output directory
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)

# -----------------------------------------------------------------------------
# 1. Hard-coded U.S. measles reported cases (CDC historical)
# -----------------------------------------------------------------------------

measles_cases <- tribble(
  ~year, ~reported_cases,
  1944, 650291,
  1945, 146013,
  1946, 659843,
  1947, 222375,
  1948, 615104,
  1949, 625281,
  1950, 319124,
  1951, 530118,
  1952, 683077,
  1953, 449146,
  1954, 682720,
  1955, 555156,
  1956, 611936,
  1957, 486799,
  1958, 763094,
  1959, 406162,
  1960, 441703,
  1961, 423919,
  1962, 481530,
  1963, 385156,
  1964, 458083,
  1965, 261904,
  1966, 204136,
  1967, 62705,
  1968, 22231,
  1969, 25826,
  1989, 18193,
  2019, 1274,
  2024, 285,
  2025, 2281
)

# -----------------------------------------------------------------------------
# 2. Risk bounds for permanent profound bilateral SNHL after measles
# -----------------------------------------------------------------------------

risk_low  <- 0.001    # 0.1%
risk_mid  <- 0.0175   # midpoint ~1.75%
risk_high <- 0.034    # 3.4%

expected_hl <- measles_cases %>%
  mutate(
    exp_low  = reported_cases * risk_low,
    exp_mid  = reported_cases * risk_mid,
    exp_high = reported_cases * risk_high
  )

# -----------------------------------------------------------------------------
# 3. Sensitivity analysis — cumulative pre-vaccine (1944–1969)
# -----------------------------------------------------------------------------

risk_scenarios <- tribble(
  ~scenario,          ~risk,
  "Ultra-conservative", 0.0005,
  "Conservative",       0.001,
  "Midpoint",           0.0175,
  "High",               0.034
)

sensitivity_cumulative <- measles_cases %>%
  filter(year <= 1969) %>%
  crossing(risk_scenarios) %>%
  mutate(expected_cases = reported_cases * risk) %>%
  group_by(scenario, risk) %>%
  summarise(
    total_expected = sum(expected_cases, na.rm = TRUE),
    total_rounded  = comma(round(total_expected)),
    .groups = "drop"
  )

write_csv(sensitivity_cumulative,
          file.path(output_dir, "sensitivity_cumulative.csv"))

# -----------------------------------------------------------------------------
# 4. NHANES 2015–2016 audiometry
# -----------------------------------------------------------------------------

demo <- nhanes("DEMO_I")
aux  <- nhanes("AUX_I")

nhanes_hl <- demo %>%
  inner_join(aux, by = "SEQN") %>%
  mutate(
    age = RIDAGEYR,
    birth_year_approx = 2015.5 - age,
    pta_left  = rowMeans(select(., AUXU500L, AUXU1K1L, AUXU2KL, AUXU4KL), na.rm = TRUE),
    pta_right = rowMeans(select(., AUXU500R, AUXU1K1R, AUXU2KR, AUXU4KR), na.rm = TRUE),
    pta_better = pmin(pta_left, pta_right, na.rm = TRUE),
    has_hl = pta_better >= 26 & !is.na(pta_better)
  ) %>%
  filter(age >= 20, age < 80, !is.na(pta_better))

nhanes_hl_summary <- nhanes_hl %>%
  mutate(
    birth_cohort = cut(
      birth_year_approx,
      breaks = c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000),
      labels = c("1930s", "1940s", "1950s", "1960s", "1970s", "1980s", "1990s")
    )
  ) %>%
  group_by(birth_cohort) %>%
  summarise(
    n = n(),
    percent_hl = mean(has_hl, na.rm = TRUE) * 100
  )

write_csv(nhanes_hl_summary,
          file.path(output_dir, "nhanes_hl_by_cohort.csv"))

# -----------------------------------------------------------------------------
# 5. Figures
# -----------------------------------------------------------------------------

p_top <- ggplot(
  expected_hl %>% filter(year >= 1940 & year <= 1970),
  aes(x = year)
) +
  geom_ribbon(aes(ymin = exp_low, ymax = exp_high),
              fill = "pink", alpha = 0.5) +
  geom_line(aes(y = exp_mid), linewidth = 1, color = "red") +
  labs(
    title = "Expected Permanent Hearing Loss from Measles (1940–1970)",
    y = "Expected cases",
    x = "Year"
  ) +
  theme_minimal()

p_bottom <- ggplot(nhanes_hl_summary,
                   aes(x = birth_cohort, y = percent_hl)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Observed Hearing Loss by Birth Cohort (NHANES 2015–2016)",
    y = "% with HL (better ear ≥ 26 dB)",
    x = "Birth cohort"
  ) +
  theme_minimal()

g <- grid.arrange(p_top, p_bottom, ncol = 1)

ggsave(file.path(output_dir, "invisible_wedge_figures.png"),
       g, width = 10, height = 12, dpi = 300)

