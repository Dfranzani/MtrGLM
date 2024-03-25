library(hexSticker)
library(magick)
library(sysfonts)

fonts <- font_files()
font_add("Roboto", "Roboto-Regular.ttf")

library(ggplot2)

g = ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(size = 0.5, color = "black") +
  theme_void()

s <- sticker(subplot = g,
             package = "MtrGLM",
             p_y = 1,
             s_width = 1,
             s_height = 1,
             s_x = 1,
             s_y = 0.8,
             p_size = 18,
             h_fill = "black",
             h_color = "gray",
             h_size = 0.5,
             url = "https://dfranzani.github.io/website/principal/home.html",
             u_color = "white",
             u_size = 1.7,
             p_family = "Roboto",
             filename = "./data-raw/MtrGLM.png"
)

usethis::use_logo("data-raw/MtrGLM.png")
