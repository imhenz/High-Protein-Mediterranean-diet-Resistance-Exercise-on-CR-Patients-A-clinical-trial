library(dplyr)
library(rstatix)
library(tidyr)

# Mark p value as significant
mark_sig <- function(p) {
  case_when(
    p < 0.0001 ~ "****",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    .default = ""
  )
}

# Return the variable and pvalues for group, period and interaction
mixed_model <- function(df, col) {
  result <- df |> 
    anova_test(dv = col, between = Group,
               within = Period, wid = Sample.id) |>
    get_anova_table() |> 
    adjust_pvalue(method = "bonferroni") |>
    as_tibble() |>
    mutate(
      variable = col, 
      p = paste0(p, mark_sig(p))
    )
  
  pvalues <- result |>
    select(variable, Effect, p) |>
    pivot_wider(names_from = "Effect", values_from = "p") |> 
    rename(p_g = Group, 
           p_t = Period, 
           p_i = `Group:Period`)
  
  fvalues <- result |>
    select(variable, Effect, F) |>
    pivot_wider(names_from = "Effect", values_from = "F") |> 
    rename(stat_g = Group, 
           stat_t = Period, 
           stat_i = `Group:Period`)
  
  inner_join(fvalues, pvalues)
}

# helper function
# Runs Mann-Whitney by default
run_tests <- function(df, variables, fct) {
  # Make sure fct is either Group or Period
  check <- fct %in% c("Group", "Period")
  
  if(!check) {
    stop("fct must be either Group or Period")
  }
  
  results <- lapply(variables, function(col) {
    # formula e.g HbA1c ~ Group (mwu)
    # formula e.g HbA1c ~ Period (Wilcoxon)
    formula <- paste(col, "~", fct) |> as.formula()
    
    if (fct == "Group") {
      # run Mann-Whitney
      result <- df |> wilcox_test(formula, paired = FALSE)
    } else {
      # Run Wilcoxon
      result <- df |> wilcox_test(formula, paired = TRUE)
    }
    
    result
  }) |> 
    bind_rows() |> 
    mutate(
      p = p.adjust(p, method = "bonferroni"),
      p = paste0(p, mark_sig(p)),
      variable = .y.
    ) |> 
    select(variable, statistic, p)
  
  if (fct == "Group") {
    results <- results |> 
      mutate(
        stat_g = statistic,
        p_g = p
      )
  } else {
    results <- results |> 
      mutate(
        stat_t = statistic,
        p_t = p
      )
  }
  
  results |> mutate(statistic = NULL, p = NULL)
}
