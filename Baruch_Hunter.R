library(rvest)
library(leaflet)
library(geosphere) # for distHaversine

# list of school wiki page names
schools <- c("Hunter_College", "Baruch_College")

# helper: DMS → decimal degrees
dms_to_dec <- function(x) {
  nums <- as.numeric(strsplit(x, "[^0-9]")[[1]])
  nums <- nums[!is.na(nums)]
  dec <- sum(nums * (1 / 60)^(0:(length(nums) - 1)))
  if (grepl("[SW]$", x)) dec <- -dec
  dec
}

# scrape coords for each school
get_coords <- function(school) {
  url <- paste0("https://en.wikipedia.org/wiki/", school)
  page <- read_html(url)

  lat_raw <- page |>
    html_element(".latitude") |>
    html_text2()
  lon_raw <- page |>
    html_element(".longitude") |>
    html_text2()

  lat <- dms_to_dec(lat_raw)
  lon <- dms_to_dec(lon_raw)

  data.frame(
    name = gsub("_", " ", school),
    lat = lat,
    lon = lon,
    stringsAsFactors = FALSE
  )
}

school_coords <- do.call(rbind, lapply(schools, get_coords))

# compute distance (in km)
dist_meters <- distHaversine(
  c(school_coords$lon[1], school_coords$lat[1]),
  c(school_coords$lon[2], school_coords$lat[2])
)
dist_km <- dist_meters / 1000

# build leaflet map with line between schools
leaflet(school_coords) %>%
  addTiles() %>%
  addPopups(
    lng = ~lon,
    lat = ~lat,
    popup = ~ paste0("Look! It's <b>", name, "</b>!")
  ) %>%
  addPolylines(
    lng = school_coords$lon,
    lat = school_coords$lat,
    color = "blue",
    weight = 3,
    # popup = paste0("Distance: ", round(dist_km, 2), " km")
  ) %>%
  addLabelOnlyMarkers(
    lng = mean(school_coords$lon),
    lat = mean(school_coords$lat),
    label = paste0("Distance: ", round(dist_km, 2), " km"),
    labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE)
  ) %>%
  setView(
    lng = mean(school_coords$lon),
    lat = mean(school_coords$lat),
    zoom = 13
  )
