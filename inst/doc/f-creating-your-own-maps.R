## ----warning=FALSE, message=FALSE---------------------------------------------
library(choroplethr)
library(choroplethrMaps)
library(R6)

# Create the class, inheriting from the base Choropleth object
CountryChoropleth = R6Class("CountryChoropleth",
  inherit = choroplethr::Choropleth,
  public = list(
    
    # Initialize with a world map
    initialize = function(user.df)
    {
      data(country.map, package="choroplethrMaps")
      super$initialize(country.map, user.df)
    }
  )
)

# Create some sample data and then render it
data(country.regions, package="choroplethrMaps")
df = data.frame(region=country.regions$region, value=sample(1:nrow(country.regions)))
c  = CountryChoropleth$new(df)
c$render()

## -----------------------------------------------------------------------------
country_choropleth

