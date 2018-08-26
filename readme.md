
# Convert WFS data from the Berlin Geodata Portal “FIS-Broker”

``` r
library(glue)
library(httr)
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.2.3, proj.4 4.9.3

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


sf_fisbroker <- function(x) {
  
  url <- get_url(x)
  print(url)
  s <- sf::read_sf(url)
  sf::st_crs(s) <- 25833
  s <- sf::st_transform(s, 4326)
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
diffent output formats using “Simple Features”
<https://github.com/r-spatial/sf>

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
    ## $ geometry              <MULTIPOLYGON [Â°]> MULTIPOLYGON (((13.21185 5...

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
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s06_06ewdichte2017?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s06_06ewdichte2017"

``` r
dplyr::glimpse(z)
```

    ## Observations: 14,730
    ## Variables: 6
    ## $ gml_id    <chr> "s06_06ewdichte2017.1100542081000000", "s06_06ewdich...
    ## $ EW2017    <int> 500, 489, 727, 296, 282, 432, 268, 936, 500, 499, 46...
    ## $ FLALLE    <dbl> 10256.4, 10304.6, 11640.2, 14027.1, 7264.4, 6629.6, ...
    ## $ EW_HA2017 <dbl> 487.50049, 474.54535, 624.55972, 211.02010, 388.1944...
    ## $ TYPKLAR   <chr> "Entkernte Blockrandbebauung, Lückenschluss nach 194...
    ## $ geometry  <MULTIPOLYGON [Â°]> MULTIPOLYGON (((13.34376 52..., MULTIP...

``` r
sf_save(z, "Einwohnerdichte2017")
```

    ## Deleting source `Einwohnerdichte2017/Einwohnerdichte2017.geojson' using driver `GeoJSON'
    ## Writing layer `Einwohnerdichte2017' to data source `Einwohnerdichte2017/Einwohnerdichte2017.geojson' using driver `GeoJSON'
    ## features:       14730
    ## fields:         5
    ## geometry type:  Multi Polygon
    ## Deleting source `Einwohnerdichte2017/Einwohnerdichte2017.sqlite' using driver `SQLite'
    ## Writing layer `Einwohnerdichte2017' to data source `Einwohnerdichte2017/Einwohnerdichte2017.sqlite' using driver `SQLite'
    ## features:       14730
    ## fields:         5
    ## geometry type:  Multi Polygon
    ## Deleting source `Einwohnerdichte2017/Einwohnerdichte2017.xlsx' using driver `XLSX'
    ## Writing layer `Einwohnerdichte2017' to data source `Einwohnerdichte2017/Einwohnerdichte2017.xlsx' using driver `XLSX'
    ## features:       14730
    ## fields:         5
    ## geometry type:  Multi Polygon

## Tempolimit

``` r
z <- sf_fisbroker("re_vms_tempolimits")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_vms_tempolimits?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=re_vms_tempolimits"

``` r
dplyr::glimpse(z)
```

    ## Observations: 29,146
    ## Variables: 13
    ## $ gml_id        <chr> "re_vms_tempolimits.8125", "re_vms_tempolimits.8...
    ## $ spatial_name  <int> 8125, 8154, 8215, 8218, 8219, 8289, 8297, 8298, ...
    ## $ spatial_alias <int> 8125, 8154, 8215, 8218, 8219, 8289, 8297, 8298, ...
    ## $ spatial_type  <chr> "MultiLineString", "MultiLineString", "MultiLine...
    ## $ ELEM_NR       <chr> "40540001_41540003.01", "44590018_44590019.01", ...
    ## $ VRICHT_TXT    <chr> "beide Richtungen", "beide Richtungen", "beide R...
    ## $ WERT_VES      <int> 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, ...
    ## $ DURCH_T       <chr> "angeordnete Verkehrseinschränkung", "angeordnet...
    ## $ ZEIT_T        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ DANN_T        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ DAT_T         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ TAG_T         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ geometry      <MULTILINESTRING [Â°]> MULTILINESTRING ((13.31983 ...,...

``` r
sf_save(z, "Tempolimit")
```

    ## Deleting source `Tempolimit/Tempolimit.geojson' using driver `GeoJSON'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.geojson' using driver `GeoJSON'
    ## features:       29146
    ## fields:         12
    ## geometry type:  Multi Line String
    ## Deleting source `Tempolimit/Tempolimit.sqlite' using driver `SQLite'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.sqlite' using driver `SQLite'
    ## features:       29146
    ## fields:         12
    ## geometry type:  Multi Line String
    ## Deleting source `Tempolimit/Tempolimit.xlsx' using driver `XLSX'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.xlsx' using driver `XLSX'
    ## features:       29146
    ## fields:         12
    ## geometry type:  Multi Line String

## Wohnlagenkarte nach Adressen zum Berliner Mietspiegel 2017

[Source](https://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=s_wohnlagenadr2017@senstadt&type=WFS)

[Info](https://fbinter.stadt-berlin.de/fb/gisbroker.do;jsessionid=FF65A3005DCF9473F4C08A00DED9FB3F?cmd=map_getStat)

``` r
z <- sf_fisbroker("re_wohnlagenadr2015")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_wohnlagenadr2015?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=re_wohnlagenadr2015"

``` r
dplyr::glimpse(z)
```

    ## Observations: 100,000
    ## Variables: 13
    ## $ gml_id        <chr> "re_wohnlagenadr2015.00001001", "re_wohnlagenadr...
    ## $ spatial_name  <chr> "00001001", "00001002", "00001003", "00001004", ...
    ## $ spatial_alias <chr> "00001001", "00001002", "00001003", "00001004", ...
    ## $ spatial_type  <chr> "Point", "Point", "Point", "Point", "Point", "Po...
    ## $ BEZNAME       <chr> "Charlottenburg-Wilmersdorf", "Charlottenburg-Wi...
    ## $ PLZ           <int> 10713, 10713, 10713, 10713, 10713, 10713, 10713,...
    ## $ STRASSE       <chr> "Aachener Straße", "Aachener Straße", "Aachener ...
    ## $ ADR           <chr> "001", "002", "003", "004", "004A", "005", "008"...
    ## $ WOL           <chr> "gut", "gut", "gut", "gut", "gut", "gut", "gut",...
    ## $ STADTTEIL     <chr> "West", "West", "West", "West", "West", "West", ...
    ## $ PLR_NAME      <chr> "Brabanter Platz", "Brabanter Platz", "Brabanter...
    ## $ LAERM         <chr> NA, NA, NA, NA, NA, NA, NA, "Ja", "Ja", "Ja", "J...
    ## $ geometry      <POINT [Â°]> POINT (13.31834 52.48219), POINT (13.3184...

``` r
sf_save(z, "Wohnlagen")
```

    ## Deleting source `Wohnlagen/Wohnlagen.geojson' using driver `GeoJSON'
    ## Writing layer `Wohnlagen' to data source `Wohnlagen/Wohnlagen.geojson' using driver `GeoJSON'
    ## features:       100000
    ## fields:         12
    ## geometry type:  Point
    ## Deleting source `Wohnlagen/Wohnlagen.sqlite' using driver `SQLite'
    ## Writing layer `Wohnlagen' to data source `Wohnlagen/Wohnlagen.sqlite' using driver `SQLite'
    ## features:       100000
    ## fields:         12
    ## geometry type:  Point
    ## Deleting source `Wohnlagen/Wohnlagen.xlsx' using driver `XLSX'
    ## Writing layer `Wohnlagen' to data source `Wohnlagen/Wohnlagen.xlsx' using driver `XLSX'
    ## features:       100000
    ## fields:         12
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
    ## $ geometry   <POLYGON [Â°]> POLYGON ((13.29919 52.5961,..., POLYGON ((...

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
