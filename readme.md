
# Convert WFS data from the Berlin Geodata Portal “FIS-Broker”

``` r
library(httr)
library(sf)
```

    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 7.0.0

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
get_X_Y_coordinates <- function(x) {
  
  sftype <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))

  if(sftype == "POINT") {
    
    xy <- as.data.frame(sf::st_coordinates(x))
    dplyr::bind_cols(x, xy)
    
  } else {
    x
  }
  
}

sf_fisbroker <- function(url) {
  
    typenames <- basename(url)
    
    url <- httr::parse_url(url)
    
    url$query <- list(service = "wfs",
                      version = "2.0.0",
                      request = "GetFeature",
                      srsName = "EPSG:25833",
                      TYPENAMES = typenames)
    
    request <- httr::build_url(url)
    
    print(request)
    
    out <- sf::read_sf(request)
    
    out <- sf::st_transform(out, 4326)
    
    out <- get_X_Y_coordinates(out)

    return(out)
}

export_format <- c(
          "geojson", 
          "sqlite"
   )

sf_save <- function(z, fname) {
  
  ifelse(!dir.exists(fname), dir.create(fname), "Folder exists already")
  ff <- paste(file.path(fname, fname), export_format, sep = ".")
  purrr::walk(ff, ~{ sf::st_write(z, .x, delete_dsn = TRUE)})
  saveRDS(z, paste0(file.path(fname, fname), ".rds"))
  
}
```

# Description

This repository is a proof-of-concept how to convert WFS data into
different output formats using “Simple Features”
<https://github.com/r-spatial/sf>

## LOR - Planungsräume

``` r
z <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan")
```

    ## [1] "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan?service=wfs&version=2.0.0&request=GetFeature&srsName=EPSG%3A25833&TYPENAMES=s_lor_plan"

``` r
z <- z %>%
     mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
     select(gml_id, RAUMID, everything()) %>%
     arrange(RAUMID)

dplyr::glimpse(z)
```

    ## Rows: 448
    ## Columns: 9
    ## $ gml_id                <chr> "s_lor_plan.01011101", "s_lor_plan.01011102", "…
    ## $ RAUMID                <chr> "01011101", "01011102", "01011103", "01011104",…
    ## $ BEZIRKSNAME           <chr> "Mitte", "Mitte", "Mitte", "Mitte", "Mitte", "M…
    ## $ PLANUNGSRAUM          <chr> "Stülerstraße", "Großer Tiergarten", "Lützowstr…
    ## $ BEZIRKSREGION         <chr> "Tiergarten Süd", "Tiergarten Süd", "Tiergarten…
    ## $ PROGNOSERAUM          <chr> "Zentrum", "Zentrum", "Zentrum", "Zentrum", "Ze…
    ## $ DATUM_GUELTIG_AB      <chr> "14.06.2006", "14.06.2006", "14.06.2006", "14.0…
    ## $ FLAECHENGROESSE_IN_M2 <dbl> 366755.5, 3009397.5, 522356.6, 338383.5, 934609…
    ## $ geometry              <MULTIPOLYGON [°]> MULTIPOLYGON (((13.33889 52..., MU…

``` r
sf_save(z, "LOR_Planungsraum")
```

    ## Deleting source `LOR_Planungsraum/LOR_Planungsraum.geojson' failed
    ## Writing layer `LOR_Planungsraum' to data source `LOR_Planungsraum/LOR_Planungsraum.geojson' using driver `GeoJSON'
    ## Writing 448 features with 8 fields and geometry type Multi Polygon.
    ## Deleting source `LOR_Planungsraum/LOR_Planungsraum.sqlite' failed
    ## Writing layer `LOR_Planungsraum' to data source `LOR_Planungsraum/LOR_Planungsraum.sqlite' using driver `SQLite'
    ## Writing 448 features with 8 fields and geometry type Multi Polygon.

## Bodenrichtwert 2020

``` r
z <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2020")
```

    ## [1] "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2020?service=wfs&version=2.0.0&request=GetFeature&srsName=EPSG%3A25833&TYPENAMES=s_brw_2020"

``` r
dplyr::glimpse(z)
```

    ## Rows: 1,133
    ## Columns: 12
    ## $ gml_id          <chr> "s_brw_2020.1002", "s_brw_2020.1005", "s_brw_2020.101…
    ## $ GEMEINDE        <chr> "Berlin", "Berlin", "Berlin", "Berlin", "Berlin", "Be…
    ## $ BEZIRK          <chr> "Spandau", "Mitte", "Treptow-Köpenick", "Pankow", "Ne…
    ## $ BRW             <dbl> 600, 2800, 400, 100, 750, 120, 550, 360, 9000, 300, 1…
    ## $ NUTZUNG         <chr> "W - Wohngebiet", "W - Wohngebiet", "W - Wohngebiet",…
    ## $ GFZ             <dbl> 0.7, 0.6, 0.4, NA, 0.8, NA, 0.4, 0.4, 3.5, 0.2, NA, 0…
    ## $ STICHTAG        <chr> "2020-01-01T00:00:00", "2020-01-01T00:00:00", "2020-0…
    ## $ BEITRAGSZUSTAND <chr> "Beitragsfrei nach BauGB", "Beitragsfrei nach BauGB",…
    ## $ LUMNUM          <chr> "[[url]]", "[[url]]", "[[url]]", "[[url]]", "[[url]]h…
    ## $ ANWERT          <chr> NA, NA, NA, NA, NA, "EU", NA, NA, NA, NA, "EU", NA, N…
    ## $ VERFAHRENSART   <chr> NA, NA, NA, NA, NA, "Entw", NA, NA, NA, NA, "Entw", N…
    ## $ geometry        <MULTIPOLYGON [°]> MULTIPOLYGON (((13.20129 52..., MULTIPOL…

``` r
sf_save(z, "Bodenrichtwerte_2020")
```

    ## Deleting source `Bodenrichtwerte_2020/Bodenrichtwerte_2020.geojson' failed
    ## Writing layer `Bodenrichtwerte_2020' to data source `Bodenrichtwerte_2020/Bodenrichtwerte_2020.geojson' using driver `GeoJSON'
    ## Writing 1133 features with 11 fields and geometry type Multi Polygon.
    ## Deleting source `Bodenrichtwerte_2020/Bodenrichtwerte_2020.sqlite' failed
    ## Writing layer `Bodenrichtwerte_2020' to data source `Bodenrichtwerte_2020/Bodenrichtwerte_2020.sqlite' using driver `SQLite'
    ## Writing 1133 features with 11 fields and geometry type Multi Polygon.

## Gebäudealter der Wohnbebauung 2015

``` r
z <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s06_12baualter")
```

    ## [1] "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s06_12baualter?service=wfs&version=2.0.0&request=GetFeature&srsName=EPSG%3A25833&TYPENAMES=s06_12baualter"

``` r
dplyr::glimpse(z)
```

    ## Rows: 13,091
    ## Columns: 23
    ## $ gml_id                <chr> "s06_12baualter.0700254561000300", "s06_12baual…
    ## $ UEBERW_DEKADE_WOH_NEU <chr> "1971-1980", "1951-1960", "1971-1980", "1951-19…
    ## $ FREISTEHEN            <int> 4, 7, 19, 9, 17, 12, 4, 41, 7, 14, 11, 11, 2, 2…
    ## $ GEREIHTES             <int> 7, 8, 29, 1, 5, 6, 37, NA, 2, 1, NA, NA, 1, NA,…
    ## $ ANDERERTYP            <int> 11, NA, 1, NA, 10, 2, 2, NA, NA, NA, 1, NA, NA,…
    ## $ X1921_1930            <int> 1, NA, 2, NA, 6, 2, 1, 5, 6, 4, 1, NA, 4, 1, 20…
    ## $ X1931_1940            <int> 1, NA, 1, NA, NA, 5, NA, 29, 5, 3, 3, NA, 1, 2,…
    ## $ X1951_1960            <int> 9, 9, 1, 5, 6, 1, 6, 7, 17, NA, NA, 2, NA, 9, 2…
    ## $ X1961_1970            <int> 1, NA, 21, 4, 14, 11, 33, 2, 1, 3, NA, 3, NA, N…
    ## $ X1971_1980            <int> 10, NA, 24, NA, 2, NA, 2, 1, NA, 2, 1, NA, NA, …
    ## $ EW2015                <int> 303, 129, 180, 70, 151, 111, 121, 134, 118, 51,…
    ## $ TYP                   <int> 25, 73, 25, 25, 23, 23, 11, 23, 23, 25, 23, 24,…
    ## $ TYPKLAR               <chr> "Verdichtung in Einzelhausgebieten, Mischbebauu…
    ## $ X1991_2000            <int> NA, 6, NA, 1, 4, 3, NA, NA, NA, 2, NA, NA, NA, …
    ## $ DOPPELHAUS            <int> NA, NA, 4, 3, 3, 3, 2, 6, 21, 1, NA, 4, 2, 7, 1…
    ## $ X_BIS_1900            <int> NA, NA, 2, NA, NA, NA, NA, NA, NA, NA, NA, 1, N…
    ## $ X1981_1990            <int> NA, NA, 2, 1, NA, NA, 2, 3, NA, 1, 1, 1, NA, 3,…
    ## $ X2011_2015            <chr> NA, NA, "1-3", NA, NA, NA, NA, NA, "1-3", NA, N…
    ## $ X1911_1920            <int> NA, NA, NA, 1, 1, NA, NA, NA, NA, NA, 5, 6, NA,…
    ## $ X1941_1950            <int> NA, NA, NA, 1, NA, NA, NA, NA, 1, NA, NA, NA, N…
    ## $ X2001_2010            <int> NA, NA, NA, NA, 2, 1, NA, NA, NA, 1, 1, NA, NA,…
    ## $ X1901_1910            <int> NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, 2, N…
    ## $ geometry              <MULTIPOLYGON [°]> MULTIPOLYGON (((13.21185 52..., MU…

``` r
sf_save(z, "Gebaeudealter")
```

    ## Deleting source `Gebaeudealter/Gebaeudealter.geojson' failed
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.geojson' using driver `GeoJSON'
    ## Writing 13091 features with 22 fields and geometry type Multi Polygon.
    ## Deleting source `Gebaeudealter/Gebaeudealter.sqlite' failed
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.sqlite' using driver `SQLite'
    ## Writing 13091 features with 22 fields and geometry type Multi Polygon.

## Einwohnerdichte 2018

``` r
z <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s06_06ewdichte2018")
```

    ## [1] "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s06_06ewdichte2018?service=wfs&version=2.0.0&request=GetFeature&srsName=EPSG%3A25833&TYPENAMES=s06_06ewdichte2018"

``` r
dplyr::glimpse(z)
```

    ## Rows: 14,759
    ## Columns: 7
    ## $ gml_id     <chr> "s06_06ewdichte2018.1200661171000000", "s06_06ewdichte2018…
    ## $ SCHL5      <dbl> 1.200661e+15, 1.200661e+15, 1.200661e+15, 1.200661e+15, 1.…
    ## $ EW2018     <int> 135, 220, 227, 232, 96, 136, 124, 367, 3, 287, 247, 64, 45…
    ## $ FLALLE     <dbl> 27319.4, 36515.2, 39358.5, 44434.1, 19595.1, 24868.4, 2611…
    ## $ EW_HA_2018 <dbl> 49.415434, 60.248883, 57.674962, 52.212152, 48.991840, 54.…
    ## $ TYPKLAR    <chr> "Villen und Stadtvillen mit parkartigen Gärten (überwiegen…
    ## $ geometry   <MULTIPOLYGON [°]> MULTIPOLYGON (((13.28802 52..., MULTIPOLYGON …

``` r
sf_save(z, "Einwohnerdichte_2018")
```

    ## Deleting source `Einwohnerdichte_2018/Einwohnerdichte_2018.geojson' failed
    ## Writing layer `Einwohnerdichte_2018' to data source `Einwohnerdichte_2018/Einwohnerdichte_2018.geojson' using driver `GeoJSON'
    ## Writing 14759 features with 6 fields and geometry type Multi Polygon.
    ## Deleting source `Einwohnerdichte_2018/Einwohnerdichte_2018.sqlite' failed
    ## Writing layer `Einwohnerdichte_2018' to data source `Einwohnerdichte_2018/Einwohnerdichte_2018.sqlite' using driver `SQLite'
    ## Writing 14759 features with 6 fields and geometry type Multi Polygon.

## Wohnlagenkarte nach Adressen zum Berliner Mietspiegel 2019

``` r
z <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_wohnlagenadr2019")
```

    ## [1] "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_wohnlagenadr2019?service=wfs&version=2.0.0&request=GetFeature&srsName=EPSG%3A25833&TYPENAMES=s_wohnlagenadr2019"

``` r
z  <- z %>% 
       mutate(ADR_num = as.numeric(stringr::str_extract(ADR, "[0-9]+")), 
              ADR_chr = stringr::str_extract(ADR, "[aA-zZ]+"))

dplyr::glimpse(z)
```

    ## Rows: 394,889
    ## Columns: 14
    ## $ gml_id    <chr> "s_wohnlagenadr2019.00007005", "s_wohnlagenadr2019.00007006…
    ## $ BEZNAME   <chr> "Spandau", "Spandau", "Spandau", "Spandau", "Spandau", "Spa…
    ## $ PLZ       <int> 13585, 13585, 13585, 13585, 13585, 13585, 13585, 13585, 135…
    ## $ STRASSE   <chr> "Achenbachstraße", "Achenbachstraße", "Achenbachstraße", "A…
    ## $ ADR       <chr> "005", "006", "007", "008", "009", "010", "011", "012", "01…
    ## $ WOL       <chr> "einfach", "einfach", "einfach", "einfach", "einfach", "ein…
    ## $ LAERM     <chr> "Ja", "Nein", "Nein", "Nein", "Nein", "Nein", "Nein", "Nein…
    ## $ STADTTEIL <chr> "West", "West", "West", "West", "West", "West", "West", "We…
    ## $ PLR_NAME  <chr> "Ackerstraße", "Ackerstraße", "Ackerstraße", "Ackerstraße",…
    ## $ X         <dbl> 13.20318, 13.20312, 13.20307, 13.20302, 13.20297, 13.20258,…
    ## $ Y         <dbl> 52.54141, 52.54162, 52.54178, 52.54197, 52.54214, 52.54222,…
    ## $ geometry  <POINT [°]> POINT (13.20318 52.54141), POINT (13.20312 52.54162),…
    ## $ ADR_num   <dbl> 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 5, 5, 6, 7, 8, 9, 10…
    ## $ ADR_chr   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "A", NA, NA…

``` r
sf_save(z, "Wohnlagenkarte_2019")
```

    ## Deleting source `Wohnlagenkarte_2019/Wohnlagenkarte_2019.geojson' failed
    ## Writing layer `Wohnlagenkarte_2019' to data source `Wohnlagenkarte_2019/Wohnlagenkarte_2019.geojson' using driver `GeoJSON'
    ## Writing 394889 features with 13 fields and geometry type Point.
    ## Deleting source `Wohnlagenkarte_2019/Wohnlagenkarte_2019.sqlite' failed
    ## Writing layer `Wohnlagenkarte_2019' to data source `Wohnlagenkarte_2019/Wohnlagenkarte_2019.sqlite' using driver `SQLite'
    ## Writing 394889 features with 13 fields and geometry type Point.

## Öffentliche Schulen

Data set used in the blog post from
[lxndrkp](https://twitter.com/lxndrkp):

[HOW TO: Downloading data from Berlin’s geospatial data
portal](https://lab.technologiestiftung-berlin.de/projects/fisbroker-to-qgis/index_en.html)

Note: Although the data is published as Open Data I exclude some
sensitive data in the export.

``` r
z <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_schulen") %>%
     select(-TELEFON, -FAX, -EMAIL, -INTERNET)
```

    ## [1] "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_schulen?service=wfs&version=2.0.0&request=GetFeature&srsName=EPSG%3A25833&TYPENAMES=s_schulen"

``` r
dplyr::glimpse(z)
```

    ## Rows: 688
    ## Columns: 15
    ## $ gml_id     <chr> "s_schulen.03G20", "s_schulen.03G21", "s_schulen.03G22", "…
    ## $ NAME       <chr> "Grundschule im Moselviertel", "Grundschule unter den Bäum…
    ## $ SCHULART   <chr> "Grundschule", "Grundschule", "Grundschule", "Grundschule"…
    ## $ ESB        <int> 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 332, 333…
    ## $ TRAEGER    <chr> "öffentlich", "öffentlich", "öffentlich", "öffentlich", "ö…
    ## $ SCHULZWEIG <chr> "Grundschule", "Grundschule", "Grundschule", "Grundschule"…
    ## $ SCHULTYP   <chr> "Grundschule", "Grundschule", "Grundschule", "Grundschule"…
    ## $ BEZIRK     <chr> "Pankow", "Pankow", "Pankow", "Pankow", "Pankow", "Pankow"…
    ## $ OT         <chr> "Weißensee", "Blankenburg", "Heinersdorf", "Karow", "Karow…
    ## $ PLZ        <int> 13088, 13129, 13089, 13125, 13125, 10439, 13187, 13187, 13…
    ## $ ADRESSE    <chr> "Brodenbacher Weg 31", "Alt-Blankenburg 26", "Tino-Schwier…
    ## $ FILIALE    <chr> "Hauptstandort", "Hauptstandort", "Hauptstandort", "Haupts…
    ## $ X          <dbl> 13.47694, 13.45445, 13.43683, 13.47966, 13.48625, 13.42160…
    ## $ Y          <dbl> 52.55931, 52.59214, 52.56539, 52.60959, 52.62083, 52.55084…
    ## $ geometry   <POINT [°]> POINT (13.47694 52.55931), POINT (13.45445 52.59214)…

``` r
sf_save(z, "Schulen")
```

    ## Deleting source `Schulen/Schulen.geojson' failed
    ## Writing layer `Schulen' to data source `Schulen/Schulen.geojson' using driver `GeoJSON'
    ## Writing 688 features with 14 fields and geometry type Point.
    ## Deleting source `Schulen/Schulen.sqlite' failed
    ## Writing layer `Schulen' to data source `Schulen/Schulen.sqlite' using driver `SQLite'
    ## Writing 688 features with 14 fields and geometry type Point.
