

two_by_two_data <- teachertools:::two_by_two_association("random")

library(ggplot2)
ggplot2::ggplot(two_by_two_data) +
  aes(x = av_bin, fill = uv_bin) +
  geom_bar(position = "fill") +
  #coord_flip()+
  scale_fill_viridis_d()

