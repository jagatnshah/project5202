install.packages(c("raster", "ggplot2", "sf", "rnaturalearth", "ggspatial"))
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(grid)
# Load and flip image
img <- brick("BardiyaImage.png")
img <- flip(img, "y")
extent(img) <- c(81.19775, 81.71392, 28.28616, 28.67491)
crs(img) <- CRS("+init=epsg:4326")
# Convert to data frame
img_df <- as.data.frame(img, xy = TRUE)
names(img_df)[3:5] <- c("r", "g", "b")
img_df$col <- rgb(img_df$r / 255, img_df$g / 255, img_df$b / 255)
# Load Nepal
nepal <- ne_countries(scale = "medium", returnclass = "sf") |> subset(name == "Nepal")
# Main map with Bardiya overlay
main_map <- ggplot() +
  geom_sf(data = nepal, fill = "steelblue", color = "gray40") +
  geom_raster(data = img_df, aes(x = x, y = y, fill = col), show.legend = FALSE) +
  scale_fill_identity() +
  geom_segment(aes(x = 84, y = 29.5, xend = 81.5, yend = 28.5), 
               arrow = arrow(length = unit(0.1, "cm")), color = "red") +
  theme_minimal() +
  ggtitle("Nepal with Bardiya National Park")
# Inset plot
inset_map <- ggplot() +
  geom_raster(data = img_df, aes(x = x, y = y, fill = col), show.legend = FALSE) +
  scale_fill_identity() +
  theme_void() +
  ggtitle("Zoom: Bardiya NP")
# Convert inset to grob
inset_grob <- ggplotGrob(inset_map)
# Add inset to main map using annotation_custom
final_map <- main_map +
  annotation_custom(grob = inset_grob,
                    xmin = 84, xmax = 89, ymin = 29, ymax = 31)
print(final_map)
ggsave("Nepal_with_Bardiya_Inset.png", plot = final_plot, width = 10, height = 8, dpi = 600)

