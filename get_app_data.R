### This script downloads the data and images that are to be updated daily.

# ## Source the functions
# source("00_app_functions.R")
# library(rsconnect)
# 
# 
# ### Raw data
# ## Pull iNaturalist and eBird data
# inat <- inat_recent("1647", "week", "Acadia National Park")
# ebird <- ebird_recent("US-ME-009", "Acadia National Park")
# 
# ## Make a df with 'groups' to add to the data
# groups <- data.frame(iconic.taxon.name = c("Plantae", "Mammalia", "Animalia", "Aves", "Insecta", 
#                                  "Reptilia", "Amphibia", "Fungi", "Protozoa", "Chromista",
#                                  "Arachnida", "Mullusca"),
#            groups = c("Plants", "Mammals", "Other animals", "Birds", "Insects", "Reptiles",
#                       "Amphibians", "Fungi and lichens", "Protozoans", "Kelp and seaweeds", 
#                       "Spiders", "Mullusks"))
# 
# ## Combine the two data frames
# final_data <- combine_citsci_data(inat, ebird, join = groups)
# 
# ## Write out the data
# write_csv(final_data, "www/datasets/the_data.csv")
# 
# 
# ### iNaturalist images
# ## Download the images to the www folder
# download_photos(final_data, "www/img/obs")


### Deploy updates to shiny app
## Set up account info:
# setAccountInfo(name = "schoodic-institute-data", 
#                token = "57B287A6E91E86DCCA29B542392AB8E2", 
#                secret = "6NZ1FD19vvDi6pKIQ+iMXgxiYyTsgOc2yEzTl4bC",
#                server = "shinyapps.io")

## Deploy
deployApp(launch.browser = F, forceUpdate = T)



## Run watchlist and new species analysis
# Get watchlist species
# watchlist_species(final_data, "app2/www/datasets")

# Get new species
# new_npspecies(final_data, "app2/www/datasets/acad_species_list.csv", "app2/www/datasets")


