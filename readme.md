
# Convert WFS data from the Berlin Geodata Portal “FIS-Broker”

``` r
library(glue)
```

    ## Warning: package 'glue' was built under R version 3.5.3

``` r
library(httr)
```

    ## Warning: package 'httr' was built under R version 3.5.2

``` r
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:glue':
    ## 
    ##     collapse

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
get_url <- function(x) {

    u_data <- glue("http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/{x}")
    u_geom <- glue("http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/{x}")

    query <- glue("?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES={x}")

    url_data <- paste0(u_data, query)
    url_geom <- paste0(u_geom, query)

    if(!http_error(url_data)) {
       url_data
    } else {
       url_geom
    }

}

get_X_Y_coordinates <- function(s) {
  
  sftype <- unique(as.character(sf::st_geometry_type(s)))

  if(sftype == "POINT") {
    
    xy <- as.data.frame(sf::st_coordinates(s))
    dplyr::bind_cols(s, xy)
    
  } else {
    s
  }
  
}

sf_fisbroker <- function(x) {
  
  url <- get_url(x)
  print(url)
  s <- sf::read_sf(url)
  sf::st_crs(s) <- 25833
  s <- sf::st_transform(s, 4326)
  s <- get_X_Y_coordinates(s)
  s

}

export_format <- c(
          "geojson", 
          # "shp",
          "sqlite",
          "xlsx"
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
z <- sf_fisbroker("s_lor_plan") 
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s_lor_plan"

``` r
z <- z %>%
     mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
     select(gml_id, RAUMID, everything()) %>%
     arrange(RAUMID)

dplyr::glimpse(z)
```

    ## Observations: 448
    ## Variables: 8
    ## $ gml_id                <chr> "s_lor_plan.01011101", "s_lor_plan.01011...
    ## $ RAUMID                <chr> "01011101", "01011102", "01011103", "010...
    ## $ PLANUNGSRAUM          <chr> "Stülerstraße", "Großer Tiergarten", "Lü...
    ## $ PROGNOSERAUM          <chr> "Zentrum", "Zentrum", "Zentrum", "Zentru...
    ## $ BEZIRK                <chr> "Mitte", "Mitte", "Mitte", "Mitte", "Mit...
    ## $ DATUM_GUELTIG_AB      <chr> "2006-06-14 00:00:00.0", "2006-06-14 00:...
    ## $ FLAECHENGROESSE_IN_M2 <dbl> 366755.5, 3009397.5, 522356.6, 338383.5,...
    ## $ geometry              <MULTIPOLYGON [°]> MULTIPOLYGON (((13.33889 52...

``` r
sf_save(z, "LOR_Planungsraum")
```

    ## Deleting source `LOR_Planungsraum/LOR_Planungsraum.geojson' using driver `GeoJSON'
    ## Writing layer `LOR_Planungsraum' to data source `LOR_Planungsraum/LOR_Planungsraum.geojson' using driver `GeoJSON'
    ## features:       448
    ## fields:         7
    ## geometry type:  Multi Polygon
    ## Deleting source `LOR_Planungsraum/LOR_Planungsraum.sqlite' using driver `SQLite'
    ## Writing layer `LOR_Planungsraum' to data source `LOR_Planungsraum/LOR_Planungsraum.sqlite' using driver `SQLite'
    ## features:       448
    ## fields:         7
    ## geometry type:  Multi Polygon
    ## Deleting source `LOR_Planungsraum/LOR_Planungsraum.xlsx' using driver `XLSX'
    ## Writing layer `LOR_Planungsraum' to data source `LOR_Planungsraum/LOR_Planungsraum.xlsx' using driver `XLSX'
    ## features:       448
    ## fields:         7
    ## geometry type:  Multi Polygon

## Erhaltungsverordnungsgebiete - Erhaltung der Zusammensetzung der Wohnbevölkerung

<https://fbinter.stadt-berlin.de/fb/berlin/service_intern.jsp?id=s_erhaltgeb_em@senstadt&type=WFS>

``` r
z <- sf_fisbroker("s_erhaltgeb_em")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_erhaltgeb_em?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s_erhaltgeb_em"

``` r
dplyr::glimpse(z)
```

    ## Observations: 57
    ## Variables: 11
    ## $ gml_id      <chr> "s_erhaltgeb_em.EM0206", "s_erhaltgeb_em.EM0301", ...
    ## $ PDF_LINK    <chr> "[[url]]https://fbinter.stadt-berlin.de/fb_daten/b...
    ## $ BEZIRK      <chr> "Friedrichshain-Kreuzberg", "Pankow", "Pankow", "P...
    ## $ GEBIETSNAME <chr> "Boxhagener Platz", "Falkplatz", "Arnimplatz", "Os...
    ## $ F_GVBL_DAT  <chr> "15.04.1999", "18.12.1997", "03.04.1999", "02.04.2...
    ## $ F_IN_KRAFT  <chr> "16.04.1999", "23.03.1997", "04.04.1999", "03.04.2...
    ## $ AE_GVBL_DAT <chr> "03.06.1999", NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ AE_IN_KRAFT <chr> "16.04.1999", NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ GK_ABL_DAT  <chr> "18.05.2018", "03.08.2018", "03.08.2018", "03.08.2...
    ## $ FL_IN_HA    <dbl> 34.2, 28.2, 50.6, 63.5, 18.5, 62.1, 48.6, 51.3, 72...
    ## $ geometry    <MULTIPOLYGON [°]> MULTIPOLYGON (((13.45787 52..., MULTI...

``` r
sf_save(z, "erhaltgeb_em")
```

    ## Deleting source `erhaltgeb_em/erhaltgeb_em.geojson' using driver `GeoJSON'
    ## Writing layer `erhaltgeb_em' to data source `erhaltgeb_em/erhaltgeb_em.geojson' using driver `GeoJSON'
    ## features:       57
    ## fields:         10
    ## geometry type:  Multi Polygon
    ## Deleting source `erhaltgeb_em/erhaltgeb_em.sqlite' using driver `SQLite'
    ## Writing layer `erhaltgeb_em' to data source `erhaltgeb_em/erhaltgeb_em.sqlite' using driver `SQLite'
    ## features:       57
    ## fields:         10
    ## geometry type:  Multi Polygon
    ## Deleting source `erhaltgeb_em/erhaltgeb_em.xlsx' using driver `XLSX'
    ## Writing layer `erhaltgeb_em' to data source `erhaltgeb_em/erhaltgeb_em.xlsx' using driver `XLSX'
    ## features:       57
    ## fields:         10
    ## geometry type:  Multi Polygon

## Erhaltungsverordnungsgebiete - Erhaltung der städtebaulichen Eigenart

<https://fbinter.stadt-berlin.de/fb/berlin/service_intern.jsp?id=s_erhaltgeb_es@senstadt&type=WFS>

``` r
z <- sf_fisbroker("s_erhaltgeb_es")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_erhaltgeb_es?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s_erhaltgeb_es"

``` r
dplyr::glimpse(z)
```

    ## Observations: 70
    ## Variables: 9
    ## $ gml_id      <chr> "s_erhaltgeb_es.ES0909", "s_erhaltgeb_es.ES0801", ...
    ## $ BEZIRK      <chr> "Treptow-Köpenick", "Neukölln", "Tempelhof-Schöneb...
    ## $ GEBIETSNAME <chr> "Pflanzgartensiedlung", "Rixdorf", "Wolframsiedlun...
    ## $ F_GVBL_DAT  <chr> "16.11.2004", "29.11.1986", "16.09.2000", "25.02.1...
    ## $ F_IN_KRAFT  <chr> "17.11.2004", "30.11.1986", "17.09.2000", "26.02.1...
    ## $ AE_GVBLDAT  <chr> "-", "15.06.1989", "-", "-", "-", "-", "-", "-", "...
    ## $ AE_INKRAFT  <chr> "-", "16.06.1989", "-", "-", "-", "-", "-", "-", "...
    ## $ FL_IN_HA    <dbl> 14.70816, 8.44687, 6.22128, 52.13354, 34.99173, 26...
    ## $ geometry    <MULTIPOLYGON [°]> MULTIPOLYGON (((13.58897 52..., MULTI...

``` r
sf_save(z, "erhaltgeb_es")
```

    ## Deleting source `erhaltgeb_es/erhaltgeb_es.geojson' using driver `GeoJSON'
    ## Writing layer `erhaltgeb_es' to data source `erhaltgeb_es/erhaltgeb_es.geojson' using driver `GeoJSON'
    ## features:       70
    ## fields:         8
    ## geometry type:  Multi Polygon
    ## Deleting source `erhaltgeb_es/erhaltgeb_es.sqlite' using driver `SQLite'
    ## Writing layer `erhaltgeb_es' to data source `erhaltgeb_es/erhaltgeb_es.sqlite' using driver `SQLite'
    ## features:       70
    ## fields:         8
    ## geometry type:  Multi Polygon
    ## Deleting source `erhaltgeb_es/erhaltgeb_es.xlsx' using driver `XLSX'
    ## Writing layer `erhaltgeb_es' to data source `erhaltgeb_es/erhaltgeb_es.xlsx' using driver `XLSX'
    ## features:       70
    ## fields:         8
    ## geometry type:  Multi Polygon

## Bodenrichtwert 2019

``` r
z <- sf_fisbroker("s_brw_2019")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_brw_2019?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s_brw_2019"

``` r
dplyr::glimpse(z)
```

    ## Observations: 1,129
    ## Variables: 12
    ## $ gml_id          <chr> "s_brw_2019.1002", "s_brw_2019.1004", "s_brw_2...
    ## $ GEMEINDE        <chr> "Berlin", "Berlin", "Berlin", "Berlin", "Berli...
    ## $ BEZIRK          <chr> "Spandau", "Mitte", "Mitte", "Treptow-Köpenick...
    ## $ BRW             <dbl> 500, 6500, 2500, 360, 60, 650, 80, 500, 310, 7...
    ## $ NUTZUNG         <chr> "W - Wohngebiet", "M1 - Kerngebiet", "W - Wohn...
    ## $ GFZ             <dbl> 0.7, 4.5, 0.6, 0.4, NA, 0.8, NA, 0.4, 0.4, 3.5...
    ## $ STICHTAG        <chr> "2019-01-01T00:00:00", "2019-01-01T00:00:00", ...
    ## $ BEITRAGSZUSTAND <chr> "Beitragsfrei nach BauGB", "Beitragsfrei nach ...
    ## $ LUMNUM          <chr> "[[url]]", "[[url]]http://www.berlin.de/gutach...
    ## $ ANWERT          <chr> NA, NA, NA, NA, NA, NA, "EU", NA, NA, NA, NA, ...
    ## $ VERFAHRENSART   <chr> NA, NA, NA, NA, NA, NA, "Entw", NA, NA, NA, NA...
    ## $ geometry        <MULTIPOLYGON [°]> MULTIPOLYGON (((13.20129 52..., M...

``` r
sf_save(z, "Bodenrichtwerte")
```

    ## Deleting source `Bodenrichtwerte/Bodenrichtwerte.geojson' using driver `GeoJSON'
    ## Writing layer `Bodenrichtwerte' to data source `Bodenrichtwerte/Bodenrichtwerte.geojson' using driver `GeoJSON'
    ## features:       1129
    ## fields:         11
    ## geometry type:  Multi Polygon
    ## Deleting source `Bodenrichtwerte/Bodenrichtwerte.sqlite' using driver `SQLite'
    ## Writing layer `Bodenrichtwerte' to data source `Bodenrichtwerte/Bodenrichtwerte.sqlite' using driver `SQLite'
    ## features:       1129
    ## fields:         11
    ## geometry type:  Multi Polygon
    ## Deleting source `Bodenrichtwerte/Bodenrichtwerte.xlsx' using driver `XLSX'
    ## Writing layer `Bodenrichtwerte' to data source `Bodenrichtwerte/Bodenrichtwerte.xlsx' using driver `XLSX'
    ## features:       1129
    ## fields:         11
    ## geometry type:  Multi Polygon

## Gebäudealter der Wohnbebauung 2015

``` r
z <- sf_fisbroker("s06_12baualter")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s06_12baualter?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s06_12baualter"

``` r
dplyr::glimpse(z)
```

    ## Observations: 13,091
    ## Variables: 23
    ## $ gml_id                <chr> "s06_12baualter.0700254561000300", "s06_...
    ## $ UEBERW_DEKADE_WOH_NEU <chr> "1971-1980", "1951-1960", "1971-1980", "...
    ## $ FREISTEHEN            <int> 4, 7, 19, 9, 17, 12, 4, 41, 7, 14, 11, 1...
    ## $ GEREIHTES             <int> 7, 8, 29, 1, 5, 6, 37, NA, 2, 1, NA, NA,...
    ## $ ANDERERTYP            <int> 11, NA, 1, NA, 10, 2, 2, NA, NA, NA, 1, ...
    ## $ X1921_1930            <int> 1, NA, 2, NA, 6, 2, 1, 5, 6, 4, 1, NA, 4...
    ## $ X1931_1940            <int> 1, NA, 1, NA, NA, 5, NA, 29, 5, 3, 3, NA...
    ## $ X1951_1960            <int> 9, 9, 1, 5, 6, 1, 6, 7, 17, NA, NA, 2, N...
    ## $ X1961_1970            <int> 1, NA, 21, 4, 14, 11, 33, 2, 1, 3, NA, 3...
    ## $ X1971_1980            <int> 10, NA, 24, NA, 2, NA, 2, 1, NA, 2, 1, N...
    ## $ EW2015                <int> 303, 129, 180, 70, 151, 111, 121, 134, 1...
    ## $ TYP                   <int> 25, 73, 25, 25, 23, 23, 11, 23, 23, 25, ...
    ## $ TYPKLAR               <chr> "Verdichtung in Einzelhausgebieten, Misc...
    ## $ X1991_2000            <int> NA, 6, NA, 1, 4, 3, NA, NA, NA, 2, NA, N...
    ## $ DOPPELHAUS            <int> NA, NA, 4, 3, 3, 3, 2, 6, 21, 1, NA, 4, ...
    ## $ X_BIS_1900            <int> NA, NA, 2, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ X1981_1990            <int> NA, NA, 2, 1, NA, NA, 2, 3, NA, 1, 1, 1,...
    ## $ X2011_2015            <chr> NA, NA, "1-3", NA, NA, NA, NA, NA, "1-3"...
    ## $ X1911_1920            <int> NA, NA, NA, 1, 1, NA, NA, NA, NA, NA, 5,...
    ## $ X1941_1950            <int> NA, NA, NA, 1, NA, NA, NA, NA, 1, NA, NA...
    ## $ X2001_2010            <int> NA, NA, NA, NA, 2, 1, NA, NA, NA, 1, 1, ...
    ## $ X1901_1910            <int> NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, N...
    ## $ geometry              <MULTIPOLYGON [°]> MULTIPOLYGON (((13.21185 52...

``` r
sf_save(z, "Gebaeudealter")
```

    ## Deleting source `Gebaeudealter/Gebaeudealter.geojson' using driver `GeoJSON'
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.geojson' using driver `GeoJSON'
    ## features:       13091
    ## fields:         22
    ## geometry type:  Multi Polygon
    ## Deleting source `Gebaeudealter/Gebaeudealter.sqlite' using driver `SQLite'
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.sqlite' using driver `SQLite'
    ## features:       13091
    ## fields:         22
    ## geometry type:  Multi Polygon
    ## Deleting source `Gebaeudealter/Gebaeudealter.xlsx' using driver `XLSX'
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.xlsx' using driver `XLSX'
    ## features:       13091
    ## fields:         22
    ## geometry type:  Multi Polygon

## Einwohnerdichte 2017

``` r
z <- sf_fisbroker("s06_06ewdichte2017")

dplyr::glimpse(z)

sf_save(z, "Einwohnerdichte2017")
```

## Tempolimit

``` r
z <- sf_fisbroker("s_vms_tempolimits_spatial")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_vms_tempolimits_spatial?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s_vms_tempolimits_spatial"

``` r
dplyr::glimpse(z)
```

    ## Observations: 29,146
    ## Variables: 10
    ## $ gml_id     <chr> "s_vms_tempolimits_spatial.54480", "s_vms_tempolimi...
    ## $ ELEM_NR    <chr> "45530022_45530023.02", "45530023_45530024.02", "45...
    ## $ VRICHT_TXT <chr> "beide Richtungen", "beide Richtungen", "beide Rich...
    ## $ WERT_VES   <int> 30, 30, 5, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, ...
    ## $ DURCH_T    <chr> "angeordnete Verkehrseinschränkung", "angeordnete V...
    ## $ ZEIT_T     <chr> NA, NA, NA, NA, NA, NA, "07:00 - 20:00", NA, NA, NA...
    ## $ DANN_T     <chr> NA, NA, NA, NA, NA, NA, "Zeitangabe", NA, NA, NA, N...
    ## $ DAT_T      <chr> NA, NA, NA, NA, NA, NA, "2006.25.04", NA, NA, NA, N...
    ## $ TAG_T      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
    ## $ geometry   <MULTILINESTRING [°]> MULTILINESTRING ((13.38516 ..., MUL...

``` r
sf_save(z, "Tempolimit")
```

    ## Deleting source `Tempolimit/Tempolimit.geojson' using driver `GeoJSON'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.geojson' using driver `GeoJSON'
    ## features:       29146
    ## fields:         9
    ## geometry type:  Multi Line String
    ## Deleting source `Tempolimit/Tempolimit.sqlite' using driver `SQLite'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.sqlite' using driver `SQLite'
    ## features:       29146
    ## fields:         9
    ## geometry type:  Multi Line String
    ## Deleting source `Tempolimit/Tempolimit.xlsx' using driver `XLSX'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.xlsx' using driver `XLSX'
    ## features:       29146
    ## fields:         9
    ## geometry type:  Multi Line String

## Wohnlagenkarte nach Adressen zum Berliner Mietspiegel 2017

``` r
z <- sf_fisbroker("s_wohnlagenadr2017")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_wohnlagenadr2017?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s_wohnlagenadr2017"

``` r
z  <- z %>% 
       mutate(ADR_num = as.numeric(stringr::str_extract(ADR, "[0-9]+")), 
              ADR_chr = stringr::str_extract(ADR, "[aA-zZ]+"))

dplyr::glimpse(z)
```

    ## Observations: 392,138
    ## Variables: 14
    ## $ gml_id    <chr> "s_wohnlagenadr2017.00007005", "s_wohnlagenadr2017.0...
    ## $ BEZNAME   <chr> "Spandau", "Spandau", "Spandau", "Spandau", "Spandau...
    ## $ PLZ       <int> 13585, 13585, 13585, 13585, 13585, 13585, 13585, 135...
    ## $ STRASSE   <chr> "Achenbachstraße", "Achenbachstraße", "Achenbachstra...
    ## $ ADR       <chr> "005", "006", "007", "008", "009", "010", "011", "01...
    ## $ WOL       <chr> "mittel", "mittel", "mittel", "mittel", "mittel", "m...
    ## $ LAERM     <chr> "JA", NA, NA, NA, NA, NA, NA, NA, NA, NA, "JA", NA, ...
    ## $ STADTTEIL <chr> "West", "West", "West", "West", "West", "West", "Wes...
    ## $ PLR_NAME  <chr> "Ackerstraße", "Ackerstraße", "Ackerstraße", "Ackers...
    ## $ geometry  <POINT [°]> POINT (13.20318 52.54141), POINT (13.20312 52....
    ## $ X         <dbl> 13.20318, 13.20312, 13.20307, 13.20302, 13.20297, 13...
    ## $ Y         <dbl> 52.54141, 52.54162, 52.54178, 52.54197, 52.54214, 52...
    ## $ ADR_num   <dbl> 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 5, 5, 6, 7, 8...
    ## $ ADR_chr   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "A",...

``` r
sf_save(z, "wohnlagenadr2017")
```

    ## Deleting source `wohnlagenadr2017/wohnlagenadr2017.geojson' using driver `GeoJSON'
    ## Writing layer `wohnlagenadr2017' to data source `wohnlagenadr2017/wohnlagenadr2017.geojson' using driver `GeoJSON'
    ## features:       392138
    ## fields:         13
    ## geometry type:  Point
    ## Deleting source `wohnlagenadr2017/wohnlagenadr2017.sqlite' using driver `SQLite'
    ## Writing layer `wohnlagenadr2017' to data source `wohnlagenadr2017/wohnlagenadr2017.sqlite' using driver `SQLite'
    ## features:       392138
    ## fields:         13
    ## geometry type:  Point
    ## Deleting source `wohnlagenadr2017/wohnlagenadr2017.xlsx' using driver `XLSX'
    ## Writing layer `wohnlagenadr2017' to data source `wohnlagenadr2017/wohnlagenadr2017.xlsx' using driver `XLSX'
    ## features:       392138
    ## fields:         13
    ## geometry type:  Point

## Wohnlagenkarte nach Adressen zum Berliner Mietspiegel 2019

  - <https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_wohnlagenadr2019>

<!-- end list -->

``` r
z <- sf_fisbroker("s_wohnlagenadr2019")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_wohnlagenadr2019?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s_wohnlagenadr2019"

``` r
z  <- z %>% 
       mutate(ADR_num = as.numeric(stringr::str_extract(ADR, "[0-9]+")), 
              ADR_chr = stringr::str_extract(ADR, "[aA-zZ]+"))

dplyr::glimpse(z)
```

    ## Observations: 394,889
    ## Variables: 14
    ## $ gml_id    <chr> "s_wohnlagenadr2019.00007005", "s_wohnlagenadr2019.0...
    ## $ BEZNAME   <chr> "Spandau", "Spandau", "Spandau", "Spandau", "Spandau...
    ## $ PLZ       <int> 13585, 13585, 13585, 13585, 13585, 13585, 13585, 135...
    ## $ STRASSE   <chr> "Achenbachstraße", "Achenbachstraße", "Achenbachstra...
    ## $ ADR       <chr> "005", "006", "007", "008", "009", "010", "011", "01...
    ## $ WOL       <chr> "einfach", "einfach", "einfach", "einfach", "einfach...
    ## $ LAERM     <chr> "Ja", "Nein", "Nein", "Nein", "Nein", "Nein", "Nein"...
    ## $ STADTTEIL <chr> "West", "West", "West", "West", "West", "West", "Wes...
    ## $ PLR_NAME  <chr> "Ackerstraße", "Ackerstraße", "Ackerstraße", "Ackers...
    ## $ geometry  <POINT [°]> POINT (13.20318 52.54141), POINT (13.20312 52....
    ## $ X         <dbl> 13.20318, 13.20312, 13.20307, 13.20302, 13.20297, 13...
    ## $ Y         <dbl> 52.54141, 52.54162, 52.54178, 52.54197, 52.54214, 52...
    ## $ ADR_num   <dbl> 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 5, 5, 6, 7, 8...
    ## $ ADR_chr   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "A",...

``` r
sf_save(z, "wohnlagenadr2019")
```

    ## Deleting source `wohnlagenadr2019/wohnlagenadr2019.geojson' using driver `GeoJSON'
    ## Writing layer `wohnlagenadr2019' to data source `wohnlagenadr2019/wohnlagenadr2019.geojson' using driver `GeoJSON'
    ## features:       394889
    ## fields:         13
    ## geometry type:  Point
    ## Deleting source `wohnlagenadr2019/wohnlagenadr2019.sqlite' using driver `SQLite'
    ## Writing layer `wohnlagenadr2019' to data source `wohnlagenadr2019/wohnlagenadr2019.sqlite' using driver `SQLite'
    ## features:       394889
    ## fields:         13
    ## geometry type:  Point
    ## Deleting source `wohnlagenadr2019/wohnlagenadr2019.xlsx' using driver `XLSX'
    ## Writing layer `wohnlagenadr2019' to data source `wohnlagenadr2019/wohnlagenadr2019.xlsx' using driver `XLSX'
    ## features:       394889
    ## fields:         13
    ## geometry type:  Point

## Emissionen

``` r
z <- sf_fisbroker("s03_12_2emissionen")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s03_12_2emissionen?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s03_12_2emissionen"

``` r
dplyr::glimpse(z)
```

    ## Observations: 1,024
    ## Variables: 26
    ## $ gml_id     <chr> "s03_12_2emissionen.117", "s03_12_2emissionen.118",...
    ## $ IDNR_1KM   <int> 38485828, 38585828, 38685828, 38785828, 38885828, 3...
    ## $ X_MAX      <int> 385800, 386800, 387800, 388800, 389800, 390800, 391...
    ## $ X_MIN      <int> 384800, 385800, 386800, 387800, 388800, 389800, 390...
    ## $ Y_MAX      <int> 5829700, 5829700, 5829700, 5829700, 5829700, 582970...
    ## $ Y_MIN      <int> 5828700, 5828700, 5828700, 5828700, 5828700, 582870...
    ## $ AREA       <int> 1000000, 1000000, 1000000, 1000000, 1000000, 100000...
    ## $ NOX_H_15   <dbl> 2.43, 1.79, 1.73, 1.35, 0.64, 0.37, 0.00, 0.31, 2.4...
    ## $ NOX_I_15   <dbl> 0.00, 52.52, 30.99, 0.00, 0.00, 0.00, 0.00, 0.00, 0...
    ## $ NOX_V_GN15 <dbl> 4.96, 6.37, 9.51, 7.03, 10.65, 3.44, 1.41, 5.09, 4....
    ## $ NOX_V_HN15 <dbl> 3.88, 4.43, 7.57, 4.52, 9.06, 2.97, 0.99, 4.57, 3.4...
    ## $ NOX_V_NN15 <dbl> 1.08, 1.93, 1.94, 2.51, 1.58, 0.46, 0.42, 0.51, 1.2...
    ## $ NOX_GE_15  <dbl> 7.40, 60.68, 42.24, 8.39, 11.29, 3.82, 1.41, 5.41, ...
    ## $ PM10_H_15  <dbl> 0.29, 0.25, 0.37, 0.37, 0.14, 0.04, 0.00, 0.13, 0.3...
    ## $ PM10_I_15  <dbl> 0.00, 0.03, 0.03, 0.00, 0.00, 0.00, 0.00, 0.00, 0.0...
    ## $ PM10_VGN15 <dbl> 0.44, 0.57, 0.85, 0.46, 0.74, 0.26, 0.14, 0.46, 0.3...
    ## $ PM10_VHN15 <dbl> 0.37, 0.44, 0.73, 0.30, 0.63, 0.23, 0.12, 0.42, 0.3...
    ## $ PM10_VNN15 <dbl> 0.06, 0.12, 0.12, 0.16, 0.10, 0.03, 0.02, 0.03, 0.0...
    ## $ PM10_GE_15 <dbl> 0.73, 0.86, 1.26, 0.83, 0.88, 0.30, 0.14, 0.59, 0.7...
    ## $ PM2_5_H_15 <dbl> 0.27, 0.24, 0.35, 0.35, 0.14, 0.03, 0.00, 0.13, 0.3...
    ## $ PM2_5_I_15 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ PM25_VGN15 <dbl> 0.22, 0.30, 0.42, 0.29, 0.35, 0.14, 0.07, 0.25, 0.1...
    ## $ PM25_VHN15 <dbl> 0.18, 0.21, 0.34, 0.18, 0.28, 0.12, 0.05, 0.23, 0.1...
    ## $ PM25_VNN15 <dbl> 0.04, 0.08, 0.08, 0.10, 0.06, 0.02, 0.01, 0.02, 0.0...
    ## $ PM25_GE15  <int> 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ geometry   <POLYGON [°]> POLYGON ((13.29919 52.5961,..., POLYGON ((1...

``` r
sf_save(z, "Emissionen")
```

    ## Deleting source `Emissionen/Emissionen.geojson' using driver `GeoJSON'
    ## Writing layer `Emissionen' to data source `Emissionen/Emissionen.geojson' using driver `GeoJSON'
    ## features:       1024
    ## fields:         25
    ## geometry type:  Polygon
    ## Deleting source `Emissionen/Emissionen.sqlite' using driver `SQLite'
    ## Writing layer `Emissionen' to data source `Emissionen/Emissionen.sqlite' using driver `SQLite'
    ## features:       1024
    ## fields:         25
    ## geometry type:  Polygon
    ## Deleting source `Emissionen/Emissionen.xlsx' using driver `XLSX'
    ## Writing layer `Emissionen' to data source `Emissionen/Emissionen.xlsx' using driver `XLSX'
    ## features:       1024
    ## fields:         25
    ## geometry type:  Polygon

## Öffentliche Schulen

Data set used in the blog post from
[lxndrkp](https://twitter.com/lxndrkp):

[HOW TO: Downloading data from Berlin’s geospatial data
portal](https://lab.technologiestiftung-berlin.de/projects/fisbroker-to-qgis/index_en.html)

Note: Although the data is published as Open Data I exclude some
sensitive data in the export.

``` r
z <- sf_fisbroker("s_schulen") %>%
     select(-TELEFON, -FAX, -EMAIL, -INTERNET, -LEITUNG)
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_schulen?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s_schulen"

``` r
dplyr::glimpse(z)
```

    ## Observations: 709
    ## Variables: 15
    ## $ gml_id    <chr> "s_schulen.03A04", "s_schulen.11G06", "s_schulen.02G...
    ## $ SCHULNAME <chr> "Abendgymnasium Prenzlauer Berg", "Adam-Ries-Grundsc...
    ## $ SCHULART  <chr> "Abend-Gymnasium", "Grundschule", "Grundschule", "Fö...
    ## $ TRAEGER   <chr> "BWF ZV", "Bezirk", "Bezirk", "Bezirk", "Bezirk", "B...
    ## $ ZWEIG_01  <chr> "Abend-Gymnasium", "Grundschule", "Grundschule", "Fö...
    ## $ BEZIRK    <chr> "Pankow", "Lichtenberg", "Friedrichshain-Kreuzberg",...
    ## $ ORTSTEIL  <chr> "Prenzlauer Berg", "Friedrichsfelde", "Kreuzberg", "...
    ## $ PLZ       <int> 10439, 10315, 10965, 12059, 12587, 12459, 12359, 133...
    ## $ ADRESSE   <chr> "Driesener Str.22", "Alt-Friedrichsfelde 66", "Hagel...
    ## $ ZWEIG_02  <chr> NA, NA, NA, NA, "Grundschule", NA, NA, "Grundschule"...
    ## $ ZWEIG_03  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ ZWEIG_04  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ geometry  <POINT [°]> POINT (13.40567 52.55201), POINT (13.53186 52....
    ## $ X         <dbl> 13.40567, 13.53186, 13.38057, 13.45108, 13.61975, 13...
    ## $ Y         <dbl> 52.55201, 52.51146, 52.49105, 52.47593, 52.44791, 52...

``` r
sf_save(z, "Schulen")
```

    ## Deleting source `Schulen/Schulen.geojson' using driver `GeoJSON'
    ## Writing layer `Schulen' to data source `Schulen/Schulen.geojson' using driver `GeoJSON'
    ## features:       709
    ## fields:         14
    ## geometry type:  Point
    ## Deleting source `Schulen/Schulen.sqlite' using driver `SQLite'
    ## Writing layer `Schulen' to data source `Schulen/Schulen.sqlite' using driver `SQLite'
    ## features:       709
    ## fields:         14
    ## geometry type:  Point
    ## Deleting source `Schulen/Schulen.xlsx' using driver `XLSX'
    ## Writing layer `Schulen' to data source `Schulen/Schulen.xlsx' using driver `XLSX'
    ## features:       709
    ## fields:         14
    ## geometry type:  Point
