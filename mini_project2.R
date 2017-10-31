#####################################################################################
# Making a map using R #
# Making a map essentially involves three steps --- getting a shape file for map,
# getting the data, and plotting. Here we make a map of state-by-state forecast of Hillary
# Clinton's chance of winning in 2016 presidential election as predicted by 
# FiveThirtyEight.com on Sep 19, 2016.
#####################################################################################

# Load the package we need
library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)# the package containing map data


# Get a shape file of states in the US

usa.df <- map_data("state")
colnames(usa.df)[5] <- "State"

# Get the data to be plotted

usa.dat <- read.table("clinton_chance_sep19_2016.csv", header = TRUE, sep = ",")
usa.dat$State <- tolower(usa.dat$State)

# Merge the data with the shape file

usa.df <- join(usa.df, usa.dat, by = "State", type = "inner")

# Abbreviations of states and where they should be plotted

states <- data.frame(state.center, state.abb) # centers of states and abbreviations
subset <- tolower(state.name) %in% usa.df$State # exclude Hawaii and Alaska
states <- states[subset, ]

# The function to plot
p <- function(data, brks, title) {
  ggp <- ggplot() + 
    #		Draw borders of states
    geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                  fill=ChanceC), color = "black", size = 0.4) +
    # 	Use shades of red to blue for plotting; trans = "reverse" option 
    # 	makes the shades go from light to dark as the percentages increases, 
    #		ensuring that darkest blue = the largest percentage of voters vote for Hillary.
    scale_fill_distiller(palette = "RdBu", breaks = brks,
                         trans = "reverse") + 
    #		Add legend
    theme_nothing(legend = TRUE) + labs(title = title, fill = "Hillary's \nChance") + 
    #		Add state abbreviations		
    geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 2)
  return(ggp)
}

# Get the break points for different shades
brks.to.use <- seq(0, 100, by = 10)
figure.title <- "The Map of State-by-State Forecast of Hillary Clinton's \nChance of Winning in 2016 Presidential Election"

# Save the map to a file to viewing 

ggsave(p(usa.df, brks.to.use, figure.title), height = 4, width = 4*1.5,
       file = "mini project2.jpg")


