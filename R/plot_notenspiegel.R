

plot_notenspiegel <- function(data, plot_title = ""){

  grades <- c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1, .7)

  notenschnitt <- mean(d$grades, na.rm = TRUE)

  data %>%
    count(grades) %>%
    mutate(bestanden = ifelse(grades > 4, FALSE, TRUE)) %>%
    ggplot(aes(x = grades, y = n)) +
    geom_col(aes(fill = bestanden)) +
    scale_x_continuous(breaks = grades[grades != 0.7]) +
    geom_vline(xintercept = notenschnitt, linetype = "dashed") +
    geom_text(aes(label = n)) +
    theme_ipsum_rc() +
    labs(x = "Noten",
         y = "Anzahl",
         title = plot_title,
         subtitle = paste0("Notenschnitt: ", notenschnitt),
         caption = paste0("n = ", nrow(data)))

}
