# knitr::purl("vignettes/v025_geospatial_hab.Rmd", "~/ContinuLegen/n2khab_demo1.R")

# remotes::install_github("inbo/n2khab",
#                         build_vignettes = TRUE,
#                         upgrade = TRUE)

options(stringsAsFactors = FALSE)
library(n2khab)
library(sf)
library(dplyr)
library(units)
library(knitr)

## Code from vignette: reference lists and using them
#####################################################

read_types() %>% glimpse()
read_types(lang = "nl")

read_env_pressures()

schemes <- read_schemes()
schemes

read_scheme_types()


## Code from vignette: Working with geospatial data sources of habitat (sub)types and RIBs
###########################################################################################

# habitatmap <- read_habitatmap()
# habitatmap

habitatstreams <- read_habitatstreams()
habitatstreams

read_habitatstreams(source_text = TRUE) %>%
    .$sources

habitatsprings <- read_habitatsprings()
habitatsprings


hms <- read_habitatmap_stdized()

hms_pol <- hms$habitatmap_polygons
hms_pol

hms_occ <- hms$habitatmap_types
hms_occ

hms_pol %>%
    mutate(area = st_area(.)) %>%
    st_drop_geometry %>%
    inner_join(hms_occ, by = "polygon_id") %>%
    # area of type within polygon:
    mutate(area_type = area * phab / 100) %>%
    group_by(type) %>%
    summarise(area = sum(area_type) %>%
                  set_units("ha") %>%
                  round(2)
    )

hmt <- read_habitatmap_terr()

hmt$habitatmap_terr_polygons
hmt$habitatmap_terr_types

hms_occ %>%
    distinct(type) %>%
    anti_join(hmt$habitatmap_terr_types %>%
                  distinct(type),
              by = "type") %>%
    arrange(type)

hmt$habitatmap_terr_types %>%
    count(source) %>%
    mutate(pct = (n / sum(n) * 100) %>% round(0))

wsh <- read_watersurfaces_hab()
wsh$watersurfaces_polygons
wsh$watersurfaces_types

wsh$watersurfaces_polygons %>%
    mutate(area = st_area(.)) %>%
    st_drop_geometry %>%
    inner_join(wsh$watersurfaces_types,
               by = "polygon_id") %>%
    group_by(type) %>%
    summarise(nr_watersurfaces = n_distinct(polygon_id),
              total_area = sum(area),
              area_min = min(area),
              area_Q1 = quantile(area, 0.25),
              area_Q2 = quantile(area, 0.5),
              area_Q3 = quantile(area, 0.75),
              max = max(area)
    ) %>%
    mutate_at(vars(matches("area|max")),
              function(x) {set_units(x, "a") %>% round(1)})

read_watersurfaces_hab(interpreted = TRUE) %>%
    .$watersurfaces_types %>%
    filter(type == "3130") %>%
    nrow

df <-
    tribble(
        ~mycode, ~obs,
        "2130", 5,
        "2190", 45,
        "2330_bu", 8,
        "2330_dw", 8,
        "6410_mo", 78,
        "6410_ve", 4,
        "91E0_vn", 10
    )
df

df_exp <-
    expand_types(df, type_var = "mycode")
df_exp

hmt$habitatmap_terr_types %>%
    semi_join(df_exp, by = c(type = "mycode")) %>%
    nrow

hmt$habitatmap_terr_types %>%
    semi_join(df, by = c(type = "mycode")) %>%
    nrow


## Code from vignette: using the GRTS data sources
#####################################################

read_GRTSmh()
read_GRTSmh(brick = TRUE)

## ----warning = FALSE, echo = FALSE-----------------------------------
data.frame(level = 1:9,
           resolution = 32 * 2 ^ (1:9)) %>%
    kable(align = "r")

read_GRTSmh_diffres(level = 5)
read_GRTSmh_diffres(level = 5, polygon = TRUE)

oldopt <- options(scipen = 999, digits = 15)
read_GRTSmh_base4frac()
options(oldopt)








## Easy use of binary data in projects: work in progress
########################################################

# minimal setup (+ downloading habitatmap_stdized):

# installing n2khab_data directory with 2 subdirectories (in working dir)
n2khab_data_path <- fileman_folders() # you can also provide a custom root 'path' to
# fileman_folders, but then take a location
# that is a (grand-grand-...)parent
# of the current working directory

vignette("v020_datastorage", package = "n2khab")

# https://zenodo.org/communities/n2khab-data-processed

hms_path <- file.path(n2khab_data_path, "20_processed/habitatmap_stdized")
dir.create(hms_path)
download_zenodo(doi = "10.5281/zenodo.3355192",
                path = hms_path)

read_habitatmap_stdized()


