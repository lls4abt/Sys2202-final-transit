# install dev version of ggmap
devtools::install_github("dkahle/ggmap")

library(ggmap)
#> Loading required package: ggplot2
#> Google Maps API Terms of Service: http://developers.google.com/maps/terms.
#> Please cite ggmap if you use it: see citation("ggmap") for details.

# save api key
register_google(key = "AIzaSyAAFM75kpNklj1RJrsAyGWDl4lYjD3Rn5g")

# check if key is saved
has_goog_key()
#> [1] TRUE