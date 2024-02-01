# Téléchargement des données COVID depuis GitHub
url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
covid_data = utils::read.csv(url)
covid_data = covid_data %>%
  dplyr::select(-Province.State, -Lat, -Long) %>%
  dplyr::group_by(Country.Region) %>%
  dplyr::summarise(across(-1, sum, .names = "{.col}"))

# transposition de data.frame
transpose_df <- function(df){
  df %>%
    t() %>%   #Tranpose, but function is for matrices. Return Matrix
    as.data.frame() %>%   #Force to be dataframe
    tibble::rownames_to_column(var = "rowname") %>%  #Resave first column from rownames
    janitor::row_to_names(row_number = 1) %>%  #Resave column headers from first row.
    tibble() %>% # recast to tibble class
    dplyr::mutate(across(-1, stringr::str_trim, .names = "{.col}")) %>%
    dplyr::mutate(across(-1, as.numeric, .names = "{.col}"))
}

# transpose data
covid_data.transposed<- transpose_df(covid_data) %>%
  dplyr::mutate(
    Country.Region = stringr::str_replace_all(Country.Region, stringr::fixed("."), "/"),
    Country.Region = stringr::str_replace_all(Country.Region, stringr::fixed("X"), "")
  ) %>%
  dplyr::summarise(across(-1, ~sum(.x, na.rm=T), .names = "{.col}")) %>%
  tidyr::pivot_longer(1:201, names_to = "Country", values_to = "Cases")

# get world states boundaries
world_shp<- rnaturalearth::ne_countries()["name_en"]

# jointure
cases_georef<- dplyr::inner_join(world_shp, covid_data.transposed, by=c("name_en"="Country"))


# export
sf::st_write(cases_georef, "covid_by_country.gpkg")
