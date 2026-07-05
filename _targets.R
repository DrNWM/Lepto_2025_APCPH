# =============================================================================
# _targets.R — Reproducible Analysis Pipeline
# Run: targets::tar_make()
#
# Death is the single outcome throughout. Generates 6 publication-ready
# figures (5 descriptive by outcome + ROC), a univariate test table, and
# crude/adjusted logistic regression tables from clean data
# =============================================================================

library(targets)
library(here)
library(tidyverse)
library(conflicted)
library(glue)
library(ggrepel)

# Handle tidyverse conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

# =============================================================================
# THEME & COLOURS
# =============================================================================

ur_colours <- c(
  "Rural" = "#1b9e77",
  "Urban-Rural" = "#d95f02",
  "Urban" = "#7570b3",
  "City Centre" = "#e7298a"
)

outcome_colours <- c("Alive" = "#2196F3", "Died" = "#E57373")

theme_lepto <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, colour = "grey35", hjust = 0, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, colour = "grey50", hjust = 1),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(margin = margin(t = 3)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.4),
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "grey95", colour = "grey80")
  )

theme_set(theme_lepto)

# =============================================================================
# PIPELINE CONFIGURATION
# =============================================================================

tar_option_set(
  packages = c("tidyverse", "here", "ggplot2", "glue", "ggrepel", "rstatix",
               "generalhoslem", "pROC", "broom", "gtsummary", "gt", "cli"),
  format = "rds"
)

# =============================================================================
# TARGETS PIPELINE
# =============================================================================

list(
  # Load clean data
  tar_target(
    lepto_data,
    read_rds(here("data", "clean", "lepto_2025_clean.rds")),
    format = "rds"
  ),

  # =========================================================================
  # SECTION 1: SUMMARY STATISTICS
  # =========================================================================

  tar_target(
    summary_stats,
    tibble(
      Metric = c("Total Cases", "Deaths", "Case Fatality Rate (%)",
                 "Mean Age (SD), years", "% Male"),
      Value = c(
        nrow(lepto_data),
        sum(lepto_data$death == 1, na.rm = TRUE),
        round(mean(lepto_data$death == 1, na.rm = TRUE) * 100, 2),
        paste0(round(mean(lepto_data$age_years, na.rm = TRUE), 1), " (",
               round(sd(lepto_data$age_years, na.rm = TRUE), 1), ")"),
        round(mean(lepto_data$sex == "Male", na.rm = TRUE) * 100, 1)
      )
    )
  ),

  tar_target(
    summary_stats_csv,
    {
      write_csv(summary_stats, here("outputs", "01_summary_stats.csv"))
      here("outputs", "01_summary_stats.csv")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 2: FIGURE 1 - CASES BY DISTRICT, SPLIT BY OUTCOME
  # Districts ranked by Case Fatality Rate (highest at top)
  # =========================================================================

  tar_target(
    fig1_district_outcome,
    {
      district_cfr <- lepto_data |>
        group_by(district) |>
        summarise(
          n_cases = n(),
          n_deaths = sum(death == 1, na.rm = TRUE),
          cfr = (n_deaths / n_cases) * 100,
          .groups = "drop"
        )

      overall_cfr <- (sum(lepto_data$death == 1, na.rm = TRUE) / nrow(lepto_data)) * 100

      plot_data <- lepto_data |>
        filter(!is.na(death)) |>
        mutate(
          outcome = factor(
            case_when(
              death == 1 ~ "Died",
              death == 0 ~ "Alive",
              .default = NA_character_
            ),
            levels = c("Alive", "Died")
          )
        ) |>
        filter(!is.na(outcome)) |>
        count(district, outcome, name = "n_cases_outcome") |>
        left_join(district_cfr, by = "district")

      cfr_labels <- district_cfr |>
        distinct(district, cfr) |>
        mutate(
          district = fct_reorder(district, cfr),
          label_text = paste0("CFR: ", round(cfr, 1), "%")
        )

      # Get max cases per district for label positioning
      max_cases_per_district <- plot_data |>
        group_by(district) |>
        summarise(
          max_cases = sum(n_cases_outcome),
          n_deaths = sum(n_cases_outcome[which(outcome == "Died")]),
          .groups = "drop"
        ) |>
        left_join(cfr_labels, by = "district") |>
        mutate(
          case_death_label = paste0("(", max_cases, " cases, ", n_deaths, " death",
                                    if_else(n_deaths != 1, "s", ""), ")")
        )

      # Map each district to its urbanisation category (Fig 4 palette)
      district_ur <- lepto_data |>
        filter(!is.na(ur_category)) |>
        distinct(district, ur_category) |>
        mutate(ur_category = factor(ur_category,
                                    levels = c("Rural", "Urban-Rural", "Urban", "City Centre")))

      # District name labels: black text on an urbanisation-coloured pill.
      # Pill colour is passed as a per-row vector (not a mapped fill scale),
      # so it never collides with the Alive/Died fill legend.
      district_labels <- district_cfr |>
        left_join(district_ur, by = "district") |>
        mutate(district = fct_reorder(district, cfr))
      label_fill <- unname(ur_colours[as.character(district_labels$ur_category)])

      plot_data |>
        ggplot(aes(x = fct_reorder(.data$district, .data$cfr), y = .data$n_cases_outcome,
                   fill = .data$outcome)) +
        geom_col(position = "fill", alpha = 0.85, width = 0.7) +
        geom_text(data = max_cases_per_district,
                  aes(x = district, y = 1.08, label = label_text),
                  hjust = 0, vjust = 0.5, size = 3.8, fontface = "bold",
                  colour = "#1b1b1b", inherit.aes = FALSE) +
        geom_text(data = max_cases_per_district,
                  aes(x = district, y = 0.50, label = case_death_label),
                  hjust = 0, vjust = 0.5, size = 3.8,
                  colour = "#1b1b1b", inherit.aes = FALSE) +
        # Invisible layer: carries the urbanisation colour scale so a second
        # legend is drawn without touching the Alive/Died fill legend
        geom_point(data = district_labels,
                   aes(x = district, y = 0.5, colour = ur_category),
                   alpha = 0, inherit.aes = FALSE) +
        # District name pills: black bold text on urbanisation-coloured badge
        geom_label(data = district_labels,
                   aes(x = district, y = -0.015, label = district),
                   fill = label_fill, colour = "white", fontface = "bold",
                   size = 3.8, hjust = 1, label.r = unit(0.5, "lines"),
                   label.padding = unit(0.28, "lines"), label.size = 0,
                   inherit.aes = FALSE, show.legend = FALSE) +
        scale_fill_manual(values = outcome_colours, drop = FALSE) +
        scale_colour_manual(values = ur_colours, name = "Urbanisation",
                            drop = FALSE, na.translate = FALSE) +
        scale_y_continuous(labels = \(x) paste0(x * 100, "%"),
                           expand = expansion(mult = c(0.02, 0.12))) +
        labs(
          title = "Cases by District, Split by Outcome (Standardized)",
          subtitle = "Districts ranked by Case Fatality Rate (highest at top) | 100% stacked | District labels coloured by urbanisation",
          x = NULL,
          y = "Proportion of Cases (%)",
          fill = "Outcome",
          caption = "Source: CDCIS e-Notifikasi"
        ) +
        coord_flip(clip = "off") +
        guides(
          fill = guide_legend(order = 1),
          colour = guide_legend(order = 2, override.aes = list(alpha = 1, size = 4))
        ) +
        theme(
          legend.position = "top",
          legend.justification = "center",
          legend.background = element_rect(fill = "white", colour = "grey80"),
          legend.direction = "horizontal",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(5.5, 5.5, 5.5, 95)
        )
    }
  ),

  tar_target(
    fig1_file,
    {
      ggsave(here("outputs", "fig1_cases_by_district_outcome.png"), fig1_district_outcome,
             width = 11, height = 6, dpi = 300)
      here("outputs", "fig1_cases_by_district_outcome.png")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 3: FIGURE 2 - AGE DISTRIBUTION BY OUTCOME
  # =========================================================================

  tar_target(
    fig2_age_outcome,
    {
      plot_data <- lepto_data |>
        filter(!is.na(age_years), !is.na(death)) |>
        mutate(
          outcome = factor(
            case_when(
              death == 1 ~ "Died",
              death == 0 ~ "Alive",
              .default = NA_character_
            ),
            levels = c("Alive", "Died")
          )
        ) |>
        filter(!is.na(outcome))

      summary_stats <- plot_data |>
        group_by(outcome) |>
        summarise(
          median_age = median(age_years, na.rm = TRUE),
          n_samples = n(),
          .groups = "drop"
        ) |>
        mutate(
          label = paste0("Median: ", median_age, " years\n(n = ", n_samples, ")")
        )

      plot_data |>
        ggplot(aes(x = .data$outcome, y = .data$age_years, fill = .data$outcome)) +
        geom_violin(alpha = 0.6, trim = FALSE) +
        geom_boxplot(width = 0.15, alpha = 0.8, outlier.shape = NA) +
        geom_text(data = summary_stats,
                  aes(x = outcome, y = 97, label = label),
                  inherit.aes = FALSE, size = 3.2, fontface = "bold",
                  colour = "#1b1b1b", vjust = 1, lineheight = 1) +
        scale_fill_manual(values = outcome_colours, drop = FALSE, na.translate = FALSE) +
        scale_y_continuous(breaks = seq(0, 80, 10)) +
        labs(
          title = "Age Distribution by Outcome",
          subtitle = "Violin = distribution shape | Box = median and IQR",
          x = "Outcome",
          y = "Age (years)",
          fill = "Outcome",
          caption = "Source: CDCIS e-Notifikasi"
        ) +
        theme(
          legend.position = "none",
          plot.title = element_text(face = "bold", size = 26, hjust = 0, margin = margin(b = 5))
        )
    }
  ),

  tar_target(
    fig2_file,
    {
      ggsave(here("outputs", "fig2_age_by_outcome.png"), fig2_age_outcome,
             width = 8, height = 6, dpi = 300)
      here("outputs", "fig2_age_by_outcome.png")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 4: FIGURE 3 - SEX DISTRIBUTION BY OUTCOME
  # =========================================================================

  tar_target(
    fig3_sex_outcome,
    {
      sex_cfr_data <- tibble(
        sex_label = c("Male", "Female"),
        cfr_pct = c(5.67, 2.55)
      )

      lepto_data |>
        filter(!is.na(sex), !is.na(death)) |>
        mutate(
          sex_label = factor(
            case_when(
              sex == "Male" ~ "Male",
              sex == "Female" ~ "Female",
              .default = sex
            ),
            levels = c("Male", "Female")
          ),
          outcome = factor(
            case_when(
              death == 1 ~ "Died",
              death == 0 ~ "Alive",
              .default = NA_character_
            ),
            levels = c("Alive", "Died")
          )
        ) |>
        filter(!is.na(outcome)) |>
        count(sex_label, outcome) |>
        ggplot(aes(x = .data$sex_label, y = .data$n, fill = .data$outcome)) +
        geom_col(position = "stack", alpha = 0.85, width = 0.7) +
        geom_text(aes(label = .data$n), position = position_stack(vjust = 0.5),
                  size = 3.2, fontface = "bold") +
        geom_text(data = sex_cfr_data,
                  aes(x = sex_label, y = 600, label = paste0("CFR: ", cfr_pct, "%")),
                  inherit.aes = FALSE, size = 4, fontface = "bold",
                  colour = "#1b1b1b", vjust = 0) +
        scale_fill_manual(values = outcome_colours, drop = FALSE) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = "Sex Distribution by Outcome",
          x = "Sex",
          y = "Number of Cases",
          fill = "Outcome",
          caption = "Source: CDCIS e-Notifikasi"
        ) +
        theme(
          legend.position = "top",
          plot.title = element_text(face = "bold", size = 26, hjust = 0, margin = margin(b = 5))
        )
    }
  ),

  tar_target(
    fig3_file,
    {
      ggsave(here("outputs", "fig3_sex_by_outcome.png"), fig3_sex_outcome,
             width = 8, height = 6, dpi = 300)
      here("outputs", "fig3_sex_by_outcome.png")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 5: URBANISATION & DISTRICT SUMMARY TABLES
  # =========================================================================

  tar_target(
    urbanisation_summary,
    {
      lepto_data |>
        filter(!is.na(ur_category)) |>
        group_by(ur_category) |>
        summarise(
          Cases = n(),
          Deaths = sum(death == 1, na.rm = TRUE),
          CFR_pct = round((Deaths / Cases) * 100, 1),
          Median_Age = median(age_years, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(ur_category = factor(ur_category,
                                    levels = c("Rural", "Urban-Rural", "Urban", "City Centre"))) |>
        arrange(ur_category)
    }
  ),

  tar_target(
    urbanisation_summary_csv,
    {
      write_csv(urbanisation_summary, here("outputs", "02_urbanisation_summary.csv"))
      here("outputs", "02_urbanisation_summary.csv")
    },
    format = "file"
  ),

  tar_target(
    district_summary,
    {
      lepto_data |>
        group_by(district, ur_category) |>
        summarise(
          Cases = n(),
          Deaths = sum(death == 1, na.rm = TRUE),
          CFR_pct = round((Deaths / Cases) * 100, 1),
          .groups = "drop"
        ) |>
        arrange(desc(Cases))
    }
  ),

  tar_target(
    district_summary_csv,
    {
      write_csv(district_summary, here("outputs", "03_district_summary.csv"))
      here("outputs", "03_district_summary.csv")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 6: ECOLOGICAL CORRELATIONS (DISTRICT-LEVEL)
  # =========================================================================

  tar_target(
    ecological_data,
    {
      lepto_data |>
        group_by(district, ur_category) |>
        summarise(
          n_cases = n(),
          n_deaths = sum(death == 1, na.rm = TRUE),
          cfr = (n_deaths / n_cases) * 100,
          population = first(population_2025),
          urbanisation_rate = first(urbanisation_rate),
          .groups = "drop"
        ) |>
        mutate(
          incidence_rate = (n_cases / population) * 100000
        ) |>
        arrange(district)
    }
  ),

  # =========================================================================
  # SECTION 7: FIGURE 4 - INCIDENCE RATE BY URBANISATION CATEGORY (descriptive)
  # =========================================================================

  tar_target(
    fig4_ir_urcategory,
    {
      plot_data <- ecological_data |>
        mutate(ur_category = factor(ur_category,
                                    levels = c("Rural", "Urban-Rural", "Urban", "City Centre")))

      summary_stats <- plot_data |>
        group_by(ur_category) |>
        summarise(
          mean_ir = mean(incidence_rate, na.rm = TRUE),
          n_districts = n(),
          .groups = "drop"
        )

      plot_data |>
        ggplot(aes(x = .data$ur_category, y = .data$incidence_rate, colour = .data$ur_category)) +
        geom_col(data = summary_stats, aes(x = ur_category, y = mean_ir, fill = ur_category),
                 alpha = 0.4, width = 0.6, inherit.aes = FALSE) +
        geom_jitter(width = 0.12, height = 0, size = 4, alpha = 0.85, show.legend = FALSE) +
        ggrepel::geom_text_repel(aes(label = .data$district), size = 2.8, fontface = "bold",
                                 show.legend = FALSE, max.overlaps = Inf) +
        geom_text(data = summary_stats,
                  aes(x = ur_category, y = 50, label = paste0("Mean:\n", round(mean_ir, 1))),
                  inherit.aes = FALSE, size = 4.2, fontface = "bold",
                  colour = "#1b1b1b", vjust = 0.5) +
        scale_colour_manual(values = ur_colours) +
        scale_fill_manual(values = ur_colours) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = "Incidence Rate by Urbanisation Category",
          subtitle = "Bar = mean incidence rate | Points = individual districts (n = 10); descriptive only",
          x = "Urbanisation Category",
          y = "Incidence Rate (per 100,000)",
          caption = "Source: CDCIS e-Notifikasi; Population: DOSM"
        ) +
        theme(legend.position = "none")
    }
  ),

  tar_target(
    fig4_file,
    {
      ggsave(here("outputs", "fig4_ir_by_urcategory.png"), fig4_ir_urcategory,
             width = 8, height = 6, dpi = 300)
      here("outputs", "fig4_ir_by_urcategory.png")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 9C: FIGURE 10 - URBANISATION CATEGORY VS OUTCOME WITH CFR
  # =========================================================================

  tar_target(
    fig10_urcategory_cfr,
    {
      cfr_by_ur <- lepto_data |>
        filter(!is.na(ur_category), !is.na(death)) |>
        group_by(ur_category) |>
        summarise(
          n_cases = n(),
          n_deaths = sum(death == 1, na.rm = TRUE),
          cfr = (n_deaths / n_cases) * 100,
          .groups = "drop"
        ) |>
        mutate(ur_category = factor(ur_category,
                                    levels = c("Rural", "Urban-Rural", "Urban", "City Centre")))

      plot_data <- lepto_data |>
        filter(!is.na(ur_category), !is.na(death)) |>
        mutate(
          ur_category = factor(ur_category,
                               levels = c("Rural", "Urban-Rural", "Urban", "City Centre")),
          outcome = factor(
            case_when(
              death == 1 ~ "Died",
              death == 0 ~ "Alive",
              .default = NA_character_
            ),
            levels = c("Alive", "Died")
          )
        ) |>
        filter(!is.na(outcome)) |>
        count(ur_category, outcome)

      plot_data |>
        ggplot(aes(x = .data$ur_category, y = .data$n, fill = .data$outcome)) +
        geom_col(position = "stack", alpha = 0.85, width = 0.7) +
        geom_text(aes(label = .data$n), position = position_stack(vjust = 0.5),
                  size = 3.2, fontface = "bold") +
        geom_text(data = cfr_by_ur,
                  aes(x = ur_category, y = n_cases + 30, label = paste0("CFR: ", round(cfr, 1), "%")),
                  inherit.aes = FALSE, size = 3.2, fontface = "bold",
                  colour = "#1b1b1b", vjust = 0) +
        scale_fill_manual(values = outcome_colours, drop = FALSE) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = "Outcome Distribution by Urbanisation Category",
          subtitle = "Stacked case counts with case fatality rates per category",
          x = "Urbanisation Category",
          y = "Number of Cases",
          fill = "Outcome",
          caption = "Source: CDCIS e-Notifikasi"
        ) +
        theme(legend.position = "top")
    }
  ),

  tar_target(
    fig10_file,
    {
      ggsave(here("outputs", "fig10_urcategory_vs_outcome.png"), fig10_urcategory_cfr,
             width = 8, height = 6, dpi = 300)
      here("outputs", "fig10_urcategory_vs_outcome.png")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 8: FIGURE 5 - URBANISATION CATEGORY BY OUTCOME
  # =========================================================================

  tar_target(
    fig5_urcategory_outcome,
    {
      cfr_by_ur <- lepto_data |>
        filter(!is.na(ur_category), !is.na(death)) |>
        group_by(ur_category) |>
        summarise(
          n_cases = n(),
          n_deaths = sum(death == 1, na.rm = TRUE),
          cfr = (n_deaths / n_cases) * 100,
          .groups = "drop"
        ) |>
        mutate(ur_category = factor(ur_category,
                                    levels = c("Rural", "Urban-Rural", "Urban", "City Centre")))

      plot_data <- lepto_data |>
        filter(!is.na(ur_category), !is.na(death)) |>
        mutate(
          ur_category = factor(ur_category,
                               levels = c("Rural", "Urban-Rural", "Urban", "City Centre")),
          outcome = factor(
            case_when(
              death == 1 ~ "Died",
              death == 0 ~ "Alive",
              .default = NA_character_
            ),
            levels = c("Alive", "Died")
          )
        ) |>
        filter(!is.na(outcome)) |>
        count(ur_category, outcome)

      plot_data |>
        ggplot(aes(x = .data$ur_category, y = .data$n, fill = .data$outcome)) +
        geom_col(position = "stack", alpha = 0.85, width = 0.7) +
        geom_text(data = filter(plot_data, outcome == "Alive"),
                  aes(label = .data$n), position = position_stack(vjust = 0.5),
                  size = 4.2, fontface = "bold") +
        geom_text(data = filter(plot_data, outcome == "Died"),
                  aes(label = .data$n), position = position_stack(vjust = 0.5),
                  size = 2.5, fontface = "bold") +
        geom_text(data = cfr_by_ur,
                  aes(x = ur_category, y = 500, label = paste0("CFR: ", round(cfr, 1), "%")),
                  inherit.aes = FALSE, size = 5, fontface = "bold",
                  colour = "#1b1b1b", vjust = 0) +
        scale_fill_manual(values = outcome_colours, drop = FALSE) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(
          title = "Number of Cases by Outcome and Urbanisation Category",
          x = "Urbanisation Category",
          y = "Number of Cases",
          fill = "Outcome",
          caption = "Source: CDCIS e-Notifikasi"
        ) +
        theme(
          legend.position = "top",
          plot.title = element_text(face = "bold", size = 13, hjust = 0, margin = margin(b = 5))
        )
    }
  ),

  tar_target(
    fig5_file,
    {
      ggsave(here("outputs", "fig5_urcategory_by_outcome.png"), fig5_urcategory_outcome,
             width = 8, height = 6, dpi = 300)
      here("outputs", "fig5_urcategory_by_outcome.png")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 9: BINARY LOGISTIC REGRESSION
  # =========================================================================

  tar_target(
    logistic_data,
    {
      lepto_data |>
        group_by(district) |>
        mutate(incidence_rate = (n() / population_2025) * 100000) |>
        ungroup() |>
        filter(!is.na(death), !is.na(age_years), !is.na(sex),
               !is.na(urbanisation_rate)) |>
        mutate(
          death = as.integer(death == 1),
          sex = factor(sex, levels = c("Female", "Male")),
          ur_category = factor(ur_category,
                               levels = c("City Centre", "Urban", "Urban-Rural", "Rural"))
        )
    }
  ),

  tar_target(
    model_crude,
    {
      glm(death ~ ur_category,
          data = logistic_data,
          family = binomial(link = "logit"))
    }
  ),

  tar_target(
    model_adjusted,
    {
      glm(death ~ ur_category + incidence_rate + age_years + sex,
          data = logistic_data,
          family = binomial(link = "logit"))
    }
  ),

  # =========================================================================
  # SECTION 10: UNIVARIATE TESTS (crude screens feeding the adjusted model)
  # Sex vs Outcome: chi-square. Age/IR vs Outcome: univariate logistic
  # regression. Urbanisation Category vs Outcome: model_crude above.
  # =========================================================================

  tar_target(
    test_sex_outcome,
    {
      model <- glm(death ~ sex, data = logistic_data, family = binomial(link = "logit"))
      broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term == "sexMale") |>
        transmute(
          Predictor = "Sex (Male vs Female)",
          Test = "Univariate Logistic Regression",
          Crude_OR = round(estimate, 3),
          CI_Low = round(conf.low, 3),
          CI_High = round(conf.high, 3),
          P_value = round(p.value, 4)
        )
    }
  ),

  tar_target(
    test_age_outcome,
    {
      model <- glm(death ~ age_years, data = logistic_data, family = binomial(link = "logit"))
      broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term == "age_years") |>
        transmute(
          Predictor = "Age (per year)",
          Test = "Univariate Logistic Regression",
          Crude_OR = round(estimate, 3),
          CI_Low = round(conf.low, 3),
          CI_High = round(conf.high, 3),
          P_value = round(p.value, 4)
        )
    }
  ),

  tar_target(
    test_ir_outcome,
    {
      model <- glm(death ~ incidence_rate, data = logistic_data, family = binomial(link = "logit"))
      broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term == "incidence_rate") |>
        transmute(
          Predictor = "District Incidence Rate (per 100,000)",
          Test = "Univariate Logistic Regression",
          Crude_OR = round(estimate, 3),
          CI_Low = round(conf.low, 3),
          CI_High = round(conf.high, 3),
          P_value = round(p.value, 4)
        )
    }
  ),

  tar_target(
    univariate_tests_table,
    {
      ur_category_rows <- broom::tidy(model_crude, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term != "(Intercept)") |>
        transmute(
          Predictor = case_when(
            term == "ur_categoryRural" ~ "Urbanisation: Rural (vs City Centre)",
            term == "ur_categoryUrban-Rural" ~ "Urbanisation: Urban-Rural (vs City Centre)",
            term == "ur_categoryUrban" ~ "Urbanisation: Urban (vs City Centre)",
            .default = term
          ),
          Test = "Univariate Logistic Regression",
          Crude_OR = round(estimate, 3),
          CI_Low = round(conf.low, 3),
          CI_High = round(conf.high, 3),
          P_value = round(p.value, 4)
        )

      bind_rows(
        test_sex_outcome,
        test_age_outcome,
        test_ir_outcome,
        ur_category_rows
      )
    }
  ),

  tar_target(
    univariate_tests_csv,
    {
      write_csv(univariate_tests_table, here("outputs", "04_univariate_tests.csv"))
      here("outputs", "04_univariate_tests.csv")
    },
    format = "file"
  ),

  tar_target(
    model_comparison_table,
    {
      get_model_stats <- \(model, model_name, is_crude = FALSE) {
        n_params <- length(coef(model)) - 1
        y_obs <- logistic_data$death
        y_pred <- fitted(model)

        hl_test <- tryCatch(
          generalhoslem::logitgof(obs = y_obs, exp = y_pred, g = 3),
          error = \(e) list(statistic = NA_real_, p.value = NA_real_)
        )

        auc_obj <- pROC::auc(y_obs, y_pred, quiet = TRUE)

        aic_crude <- AIC(model_crude)
        aic_adj <- AIC(model_adjusted)
        aic_diff <- aic_adj - aic_crude

        recommendation <- if (is_crude) {
          "Simpler"
        } else {
          if (aic_diff < 2) {
            "Preferred ✓"
          } else {
            "Alternative"
          }
        }

        tibble(
          Model = model_name,
          `N Events` = sum(y_obs),
          `N Total` = nrow(logistic_data),
          `Parameters` = n_params,
          `EPV` = round(sum(y_obs) / n_params, 2),
          `AIC` = round(AIC(model), 2),
          `Calibration (HL p)` = round(hl_test$p.value, 4),
          `AUC` = round(as.numeric(auc_obj), 3),
          `Recommendation` = recommendation
        )
      }

      bind_rows(
        get_model_stats(model_crude, "A: Crude (UR only)", is_crude = TRUE),
        get_model_stats(model_adjusted, "B: Adjusted (UR + IR + Age + Sex)", is_crude = FALSE)
      )
    }
  ),

  tar_target(
    model_comparison_csv,
    {
      write_csv(model_comparison_table, here("outputs", "05_model_comparison.csv"))
      here("outputs", "05_model_comparison.csv")
    },
    format = "file"
  ),

  tar_target(
    odds_ratios_table,
    {
      or_crude <- broom::tidy(model_crude, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term != "(Intercept)") |>
        transmute(
          Model = "Crude",
          Predictor = case_when(
            term == "ur_categoryRural" ~ "Urbanisation: Rural (vs City Centre)",
            term == "ur_categoryUrban-Rural" ~ "Urbanisation: Urban-Rural (vs City Centre)",
            term == "ur_categoryUrban" ~ "Urbanisation: Urban (vs City Centre)",
            .default = term
          ),
          OR = round(estimate, 3),
          CI_Low = round(conf.low, 3),
          CI_High = round(conf.high, 3),
          p_value = round(p.value, 4),
          Significance = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01 ~ "**",
            p.value < 0.05 ~ "*",
            p.value < 0.10 ~ "†",
            .default = "ns"
          )
        )

      or_adj <- broom::tidy(model_adjusted, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term != "(Intercept)") |>
        transmute(
          Model = "Adjusted",
          Predictor = case_when(
            term == "ur_categoryRural" ~ "Urbanisation: Rural (vs City Centre)",
            term == "ur_categoryUrban-Rural" ~ "Urbanisation: Urban-Rural (vs City Centre)",
            term == "ur_categoryUrban" ~ "Urbanisation: Urban (vs City Centre)",
            term == "incidence_rate" ~ "District Incidence Rate (per 100,000)",
            term == "age_years" ~ "Age (per year)",
            term == "sexMale" ~ "Sex (Male vs Female)",
            .default = term
          ),
          OR = round(estimate, 3),
          CI_Low = round(conf.low, 3),
          CI_High = round(conf.high, 3),
          p_value = round(p.value, 4),
          Significance = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01 ~ "**",
            p.value < 0.05 ~ "*",
            p.value < 0.10 ~ "†",
            .default = "ns"
          )
        )

      bind_rows(or_crude, or_adj)
    }
  ),

  tar_target(
    odds_ratios_csv,
    {
      write_csv(odds_ratios_table, here("outputs", "06_odds_ratios.csv"))
      here("outputs", "06_odds_ratios.csv")
    },
    format = "file"
  ),

  tar_target(
    logistic_summary,
    {
      tibble(Summary = "Binary logistic regression results")
    }
  ),

  tar_target(
    logistic_summary_txt,
    {
      writeLines("Results saved to outputs/", here("outputs", "07_logistic_summary.txt"))
      here("outputs", "07_logistic_summary.txt")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 10B: FOREST PLOT - UNIVARIATE RESULTS
  # =========================================================================

  tar_target(
    forest_plot_univariate,
    {
      forest_data <- univariate_tests_table |>
        filter(Test == "Univariate Logistic Regression") |>
        mutate(
          Predictor = factor(Predictor, levels = rev(unique(Predictor))),
          sig_colour = case_when(
            P_value < 0.05 ~ "#E74C3C",
            .default = "#95A5A6"
          ),
          p_display = pmax(P_value, 0.001),
          label_text = paste0(
            round(Crude_OR, 2), " [", round(CI_Low, 2), "-", round(CI_High, 2), "]\n",
            "p=", format(p_display, digits = 3)
          )
        )

      forest_data |>
        ggplot(aes(x = .data$Crude_OR, y = .data$Predictor, colour = .data$sig_colour)) +
        geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
        geom_errorbarh(aes(xmin = .data$CI_Low, xmax = .data$CI_High),
                       height = 0.2, linewidth = 1, alpha = 0.8) +
        geom_point(size = 4, alpha = 0.9) +
        geom_text(aes(label = .data$label_text),
                  hjust = -0.1, vjust = 0.5, size = 3.2, fontface = "bold",
                  colour = "#1b1b1b") +
        scale_x_log10(expand = expansion(mult = c(0.1, 0.4))) +
        scale_colour_identity() +
        labs(
          title = "Univariate Logistic Regression: Odds Ratios",
          subtitle = "Effect sizes with 95% confidence intervals | Red = significant at p<0.05",
          x = "Odds Ratio (log scale)",
          y = NULL,
          caption = "Source: Univariate analysis; Reference: Female sex, City Centre urbanisation"
        ) +
        theme(
          axis.text.y = element_text(size = 10, face = "bold"),
          panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.5)
        )
    }
  ),

  tar_target(
    forest_plot_file,
    {
      ggsave(here("outputs", "fig7_forest_plot_univariate.png"), forest_plot_univariate,
             width = 10, height = 6, dpi = 300)
      here("outputs", "fig7_forest_plot_univariate.png")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 10C: FOREST PLOT - ADJUSTED MULTIVARIATE MODEL
  # =========================================================================

  tar_target(
    adjusted_model_results,
    {
      broom::tidy(model_adjusted, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term != "(Intercept)") |>
        transmute(
          Predictor = case_when(
            term == "ur_categoryRural" ~ "Urbanisation: Rural (vs City Centre)",
            term == "ur_categoryUrban-Rural" ~ "Urbanisation: Urban-Rural (vs City Centre)",
            term == "ur_categoryUrban" ~ "Urbanisation: Urban (vs City Centre)",
            term == "incidence_rate" ~ "District Incidence Rate (per 100,000)",
            term == "age_years" ~ "Age (per year)",
            term == "sexMale" ~ "Sex (Male vs Female)",
            .default = term
          ),
          Adjusted_OR = round(estimate, 3),
          CI_Low = round(conf.low, 3),
          CI_High = round(conf.high, 3),
          P_value = round(p.value, 4)
        )
    }
  ),

  tar_target(
    forest_plot_adjusted,
    {
      forest_data <- adjusted_model_results |>
        mutate(
          Predictor = factor(Predictor, levels = rev(unique(Predictor))),
          sig_colour = case_when(
            P_value < 0.05 ~ "#E74C3C",
            .default = "#95A5A6"
          ),
          p_display = pmax(P_value, 0.001),
          label_text = paste0(
            round(Adjusted_OR, 2), " [", round(CI_Low, 2), "-", round(CI_High, 2), "]\n",
            "p=", format(p_display, digits = 3)
          )
        )

      forest_data |>
        ggplot(aes(x = .data$Adjusted_OR, y = .data$Predictor, colour = .data$sig_colour)) +
        geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
        geom_errorbarh(aes(xmin = .data$CI_Low, xmax = .data$CI_High),
                       height = 0.2, linewidth = 1, alpha = 0.8) +
        geom_point(size = 4, alpha = 0.9) +
        geom_text(aes(label = .data$label_text),
                  hjust = -0.1, vjust = 0.5, size = 3.2, fontface = "bold",
                  colour = "#1b1b1b") +
        scale_x_log10(expand = expansion(mult = c(0.1, 0.4))) +
        scale_colour_identity() +
        labs(
          title = "Adjusted Logistic Regression: Odds Ratios",
          subtitle = "Effect sizes adjusted for urbanisation, sex, age, and incidence rate | Red = significant at p<0.05",
          x = "Odds Ratio (log scale)",
          y = NULL,
          caption = "Source: Multivariate analysis; Reference: Female sex, City Centre urbanisation"
        ) +
        theme(
          axis.text.y = element_text(size = 10, face = "bold"),
          panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.5)
        )
    }
  ),

  tar_target(
    forest_plot_adjusted_file,
    {
      ggsave(here("outputs", "fig8_forest_plot_adjusted.png"), forest_plot_adjusted,
             width = 10, height = 6, dpi = 300)
      here("outputs", "fig8_forest_plot_adjusted.png")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 11: FORMATTED TABLES (GT)
  # =========================================================================

  tar_target(
    model_comparison_gt,
    {
      model_comparison_table |>
        gt::gt() |>
        gt::fmt_number(columns = c(`AIC`, `AUC`), decimals = 2) |>
        gt::fmt_number(columns = c(`Calibration (HL p)`), decimals = 4) |>
        gt::tab_header(
          title = "Binary Logistic Regression: Model Comparison",
          subtitle = "Johor, Malaysia"
        ) |>
        gt::opt_table_font(font = "Arial") |>
        gt::tab_options(
          table.font.size = "13px",
          heading.title.font.size = "16px",
          heading.subtitle.font.size = "12px",
          column_labels.background.color = "#2C3E50",
          column_labels.font.weight = "bold",
          table.border.top.style = "solid",
          table.border.bottom.style = "solid"
        )
    }
  ),

  tar_target(
    model_comparison_html,
    {
      gt::gtsave(model_comparison_gt,
                 filename = here("outputs", "Table_01_Model_Comparison.html"))
      here("outputs", "Table_01_Model_Comparison.html")
    },
    format = "file"
  ),

  tar_target(
    odds_ratios_gt,
    {
      odds_ratios_table |>
        gt::gt() |>
        gt::fmt_number(columns = c(`OR`, `CI_Low`, `CI_High`, `p_value`), decimals = 3) |>
        gt::tab_header(
          title = "Binary Logistic Regression: Odds Ratios",
          subtitle = "Crude and Adjusted Models"
        ) |>
        gt::opt_table_font(font = "Arial") |>
        gt::tab_options(
          table.font.size = "13px",
          heading.title.font.size = "16px",
          heading.subtitle.font.size = "12px",
          column_labels.background.color = "#2C3E50",
          column_labels.font.weight = "bold",
          table.border.top.style = "solid",
          table.border.bottom.style = "solid"
        )
    }
  ),

  tar_target(
    odds_ratios_html,
    {
      gt::gtsave(odds_ratios_gt,
                 filename = here("outputs", "Table_02_Odds_Ratios.html"))
      here("outputs", "Table_02_Odds_Ratios.html")
    },
    format = "file"
  ),

  # =========================================================================
  # SECTION 12: FIGURE 6 - ROC CURVES
  # =========================================================================

  tar_target(
    roc_curves_plot,
    {
      y_obs <- logistic_data$death
      y_pred_crude <- fitted(model_crude)
      y_pred_adj <- fitted(model_adjusted)

      roc_crude <- pROC::roc(y_obs, y_pred_crude, quiet = TRUE)
      roc_adj <- pROC::roc(y_obs, y_pred_adj, quiet = TRUE)

      roc_crude_df <- data.frame(
        sensitivity = roc_crude$sensitivities,
        specificity = roc_crude$specificities,
        model = "A: Crude (UR only)"
      )

      roc_adj_df <- data.frame(
        sensitivity = roc_adj$sensitivities,
        specificity = roc_adj$specificities,
        model = "B: Adjusted (UR + IR + Age + Sex)"
      )

      roc_data <- bind_rows(roc_crude_df, roc_adj_df)

      ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = model)) +
        geom_line(linewidth = 1.2) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
        scale_color_manual(
          values = c(
            "A: Crude (UR only)" = "#E74C3C",
            "B: Adjusted (UR + IR + Age + Sex)" = "#3498DB"
          )
        ) +
        labs(
          title = "ROC Curves: Model Discrimination Ability",
          subtitle = glue("Model A AUC = {round(roc_crude$auc, 3)} | Model B AUC = {round(roc_adj$auc, 3)}"),
          x = "1 - Specificity (False Positive Rate)",
          y = "Sensitivity (True Positive Rate)",
          color = "Model"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 2)),
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
          legend.position = c(0.98, 0.02),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          legend.margin = margin(5, 5, 5, 5),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          plot.margin = margin(5, 5, 5, 5)
        ) +
        coord_fixed()
    }
  ),

  tar_target(
    roc_curves_file,
    {
      ggsave(here("outputs", "fig6_roc_curves_2025.png"), roc_curves_plot,
             width = 10, height = 8, dpi = 300)
      here("outputs", "fig6_roc_curves_2025.png")
    },
    format = "file"
  )
)

# =============================================================================
# HOW TO USE THIS PIPELINE:
#
# Run the pipeline:
#   targets::tar_make()
#
# Check status:
#   targets::tar_status()
#
# View pipeline diagram:
#   targets::tar_visnetwork()
#
# Run specific target:
#   targets::tar_make(names = "fig1_district_outcome")
# =============================================================================
