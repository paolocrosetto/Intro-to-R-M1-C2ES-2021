# CM5 - Alexandra Lugova - GGplot avancee

library(tidyverse)

## A base plot to work with
base <- iris
base

base <- iris %>% 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, 
             color = Species, size = Petal.Width))+
  geom_point()

# Coordinates

## Coord_functions 
## Invert axes - coord_flip
base + coord_flip()

## Cartesian linear coordinates (limits): coord_cartesian - change the limits of the plot to 'zoom' on a part of it
base + coord_cartesian(xlim = c(5, 6), ylim = c(3,4))

## Cartesian linear coordinates (fixed):two scales on the two axes to be comparable
ggplot(mpg, aes(cty, hwy)) + geom_point() + labs(title = "It looks like mileage in City and Highway is the same!")

ggplot(mpg, aes(cty, hwy)) + geom_point() + 
  coord_fixed() + labs(title = "But mileage is higher in highways!")

ggplot(mpg, aes(cty, hwy)) + geom_point() + 
  coord_fixed(ratio = 1/5) + labs(title = "City is 5 times as big as highway here")

## coordinate transformations

##log scales
ggplot(mpg, aes(cty, hwy)) + geom_point() + coord_trans(x = "log10", y = "log10")+ labs(title = "log axes")

## polar coordinates -transform a (x,y) coordinate into a (distance, angle)
base + coord_polar()

## Polar coordinates: pie charts
ggplot(mpg, aes(x = "", y = class, fill = class)) + geom_col()

ggplot(mpg, aes(x = "", y = class, fill = class)) + geom_col() + coord_polar("y", start = 0)

# Scales

## Scale functions
base + scale_color_grey() + scale_size_continuous(range = c(1, 10))

## Changing colors: `scale_color_*`
## Discrete color scales
## Brewer palettes
base + scale_color_brewer()
base + scale_color_brewer(type = "qual")
base + scale_color_brewer(palette = "Set1")

## Viridis palette
base + scale_color_viridis_d()

## Info on color palettes: brewer
RColorBrewer::display.brewer.all()

## Colors manually
base + scale_color_manual(values = c('pink','yellow','blue'))

## Continuous color
## a simple plot to showcase stuff
base2 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Width))+
  geom_point()
base2

## Gradients: change the gradient
base2 + scale_color_gradient(low = "green", high = "red")

## a gradient with an intermediate step
base2 + scale_color_gradient2(low = "red", mid = "grey", high = "green", midpoint = mean(iris$Petal.Width))

## In-between continuous and discrete: binned scales
## you have continuous data but you want to treat it as discrete: `binned` scales
base2 + scale_color_binned()

## change the gradient to be colorblind proof
base2 + scale_color_viridis_c()

## Continuous size
base + scale_size_continuous(range = c(1,12))

## binned size
base + scale_size_binned(n.breaks = 3, range = c(1,12))

## Controlling barplots: fill
ggplot(mpg, aes(x = manufacturer, color = manufacturer)) + geom_bar()

prod <- ggplot(mpg, aes(x = manufacturer, fill = manufacturer)) + geom_bar()
prod

prod + scale_fill_viridis_d()

## Not-mapped appearance - if appearance **does not vary** 
ggplot(mpg, aes(x = manufacturer)) + geom_bar(fill = "yellow") + geom_hline(yintercept = 15, color = 'red')

# Theme

## Theming the plot
## pre-installed themes gallery
base + theme_classic()
base + theme_dark()
base + theme_linedraw()
base + theme_void()

## install new themes and use theme
install.packages("ggthemes")
library(ggthemes)
base + theme_excel_new()
base + theme_economist()
base + theme_light()

## redaction de theme
base + theme_light() + theme(legend.position = "left") +
  theme(panel.background = element_rect(fill = "yellow"))

base + labs(title = "title", subtitle = "subtitle", caption = "CAPTION", tag = "tag")

## saving a plot - `ggsave(filename, width, height, dpi)` - by default it saves the last plot made

# Annotations and lines

## adding non-data driven elements
## lines
base + geom_hline(yintercept = 3, color = "red")
base + geom_vline(xintercept = 5, color = "blue")
base + geom_abline(slope = 0.8, intercept = -2, color = "orange")

## Text as a `geom`
carspeed <- mpg %>%  filter(year == 2008 & trans == "manual(m5)") %>% ggplot(aes(x = cty, y = hwy))
carspeed + geom_point()

## labeling cars by their name: text
carspeed + geom_text(aes(label = model))

carspeed + geom_label(aes(label = model))

## Text as a annotation
base + annotate(geom = "text", label = "ANNOTATION", x = 6, y = 4, hjust = 0)

