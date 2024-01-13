library(readr)
teaching_data2 <- read_csv("/Users/sebastiansaueruser/github-repos/_other/teachertools/data-raw/teaching_df.csv")


library(dplyr)
all_dfs <- teachertools::teaching_df()

d <- teachertools::teaching_df("random", canonicalize = FALSE)

d_name <- attr(d, "df_name")

d_path <- all_dfs |>
  dplyr::filter(datasets_names == d_name) |>
  dplyr::pull(source)



preds_chosen <- attr(d, "preds_chosen")
focus_pred <- attr(d, "focus_pred")
output_var <- attr(d, "output_var")

preds_k <- sample(c(-2, -1, 0, 1, 2), 1)

d_nom_vars <- teaching_data2 |>
  dplyr::filter(datasets_names == d_name) |>
  dplyr::pull(nom_vars)

# d_nom_vars_vec <- stringr::str_split(d_nom_vars,
#                                      pattern = ",",
#                                      simplify = TRUE)

d_nom_vars_vec <- strsplit(d_nom_vars, ",") |> unlist() |>
  sample()

focus_nom_var <- d_nom_vars_vec[1]

output_var <- teaching_data2 |>
  dplyr::filter(datasets_names == d_name) |>
  dplyr::pull(main_var)



new_name <- paste0(output_var,"_bin")


d2 <-
  d |>
  select(all_of(c(focus_nom_var, output_var)))
  mutate(across(all_of(focus_nom_var), as.factor)) |>
  mutate(var2 = ifelse(!!sym(output_var) >= mean(!!sym(output_var), na.rm = TRUE), "high", "low")) |>
  mutate(var2 = factor(var2)) |>
  rename(!!new_name := var2) |>
  select(all_of(c(focus_nom_var, new_name)))

#names(d2) <- c("var1", "var2")

library(ggplot2)
ggplot2::ggplot(d2) +
  aes(x = !!sym(names(d2[1])), fill = !!sym(names(d2)[[2]])) +
  geom_bar(position = "fill") +
  #coord_flip()+
  scale_fill_viridis_d()

