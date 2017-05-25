
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class)


summary(mpg)
