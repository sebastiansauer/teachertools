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

d2 <-
  d |>
  mutate(across(all_of(d_nom_vars_vec), as.factor)) |>
  select(all_of(d_nom_vars_vec[1:2]))

#names(d2) <- c("var1", "var2")

library(ggplot2)
ggplot2::ggplot(d2) +
  aes(x = !!sym(d_nom_vars_vec[1]), fill = !!sym(d_nom_vars_vec[2])) +
  geom_bar(position = "fill") +
  #coord_flip()+
  scale_fill_viridis_d()
