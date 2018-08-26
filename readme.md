rr
================

``` r
library(glue)
library(httr)
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.2.3, proj.4 4.9.3

``` r
# Source: 
# - https://fbinter.stadt-berlin.de/fb/index.jsp?loginkey=zoomStart&mapId=wmsk_06_06ewdichte2017@senstadt
# - https://fbinter.stadt-berlin.de/fb/berlin/service.jsp?id=s06_06ewdichte2017@senstadt&type=WFS

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

form <- c(
  "geojson", 
  "gml",
  "kml",
  "shp",
  "sqlite",
  "xlsx"
   )


sf_save <- function(z, fname) {
  
  ifelse(!dir.exists(fname), dir.create(fname), "Folder exists already")
  ff <- paste(file.path(fname, fname), form, sep = ".")
  purrr::walk(ff, ~{ sf::st_write(z, .x, delete_dsn=TRUE)})
  saveRDS(z, paste(fname, ".rds"))
}
```

## Gebäudealter der Wohnbebauung 2015

``` r
z <- sf_fisbroker("s06_12baualter")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s06_12baualter?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=s06_12baualter"

``` r
z
```

    ## Simple feature collection with 13091 features and 22 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 13.09301 ymin: 52.33963 xmax: 13.73528 ymax: 52.66073
    ## epsg (SRID):    4326
    ## proj4string:    +proj=longlat +datum=WGS84 +no_defs
    ## # A tibble: 13,091 x 23
    ##    gml_id UEBERW_DEKADE_W~ FREISTEHEN GEREIHTES ANDERERTYP X1921_1930
    ##    <chr>  <chr>                 <int>     <int>      <int>      <int>
    ##  1 s06_1~ 1971-1980                 4         7         11          1
    ##  2 s06_1~ 1951-1960                 7         8         NA         NA
    ##  3 s06_1~ 1971-1980                19        29          1          2
    ##  4 s06_1~ 1951-1960                 9         1         NA         NA
    ##  5 s06_1~ 1961-1970                17         5         10          6
    ##  6 s06_1~ 1961-1970                12         6          2          2
    ##  7 s06_1~ 1961-1970                 4        37          2          1
    ##  8 s06_1~ 1931-1940                41        NA         NA          5
    ##  9 s06_1~ 1951-1960                 7         2         NA          6
    ## 10 s06_1~ 1921-1930                14         1         NA          4
    ## # ... with 13,081 more rows, and 17 more variables: X1931_1940 <int>,
    ## #   X1951_1960 <int>, X1961_1970 <int>, X1971_1980 <int>, EW2015 <int>,
    ## #   TYP <int>, TYPKLAR <chr>, X1991_2000 <int>, DOPPELHAUS <int>,
    ## #   X_BIS_1900 <int>, X1981_1990 <int>, X2011_2015 <chr>,
    ## #   X1911_1920 <int>, X1941_1950 <int>, X2001_2010 <int>,
    ## #   X1901_1910 <int>, geometry <MULTIPOLYGON [Â°]>

``` r
sf_save(z, "Gebaeudealter")
```

    ## Deleting source `Gebaeudealter/Gebaeudealter.geojson' failed
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.geojson' using driver `GeoJSON'
    ## features:       13091
    ## fields:         22
    ## geometry type:  Multi Polygon

    ## Warning in CPL_write_ogr(obj, dsn, layer, driver,
    ## as.character(dataset_options), : GDAL Error 4: Unable to open
    ## Gebaeudealter/Gebaeudealter.gml to obtain file list.

    ## Deleting source `Gebaeudealter/Gebaeudealter.gml' failed
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.gml' using driver `GML'
    ## features:       13091
    ## fields:         22
    ## geometry type:  Multi Polygon

    ## Warning in CPL_write_ogr(obj, dsn, layer, driver,
    ## as.character(dataset_options), : GDAL Error 4: Unable to open
    ## Gebaeudealter/Gebaeudealter.kml to obtain file list.

    ## Deleting source `Gebaeudealter/Gebaeudealter.kml' failed
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.kml' using driver `KML'
    ## features:       13091
    ## fields:         22
    ## geometry type:  Multi Polygon

    ## Warning in abbreviate_shapefile_names(obj): Field names abbreviated for
    ## ESRI Shapefile driver

    ## Warning in CPL_write_ogr(obj, dsn, layer, driver,
    ## as.character(dataset_options), : GDAL Error 1: Gebaeudealter/
    ## Gebaeudealter.shp does not appear to be a file or directory.

    ## Deleting source `Gebaeudealter/Gebaeudealter.shp' failed
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.shp' using driver `ESRI Shapefile'
    ## features:       13091
    ## fields:         22
    ## geometry type:  Multi Polygon

    ## Warning in CPL_write_ogr(obj, dsn, layer, driver,
    ## as.character(dataset_options), : GDAL Message 1: One or several characters
    ## couldn't be converted correctly from UTF-8 to ISO-8859-1. This warning will
    ## not be emitted anymore.

    ## Deleting source `Gebaeudealter/Gebaeudealter.sqlite' failed
    ## Writing layer `Gebaeudealter' to data source `Gebaeudealter/Gebaeudealter.sqlite' using driver `SQLite'
    ## features:       13091
    ## fields:         22
    ## geometry type:  Multi Polygon
    ## Deleting source `Gebaeudealter/Gebaeudealter.xlsx' failed
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
z
```

    ## Simple feature collection with 14730 features and 5 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 13.09301 ymin: 52.33963 xmax: 13.74157 ymax: 52.66074
    ## epsg (SRID):    4326
    ## proj4string:    +proj=longlat +datum=WGS84 +no_defs
    ## # A tibble: 14,730 x 6
    ##    gml_id   EW2017 FLALLE EW_HA2017 TYPKLAR                       geometry
    ##    <chr>     <int>  <dbl>     <dbl> <chr>              <MULTIPOLYGON [Â°]>
    ##  1 s06_06e~    500 10256.      488. Entkernte~ (((13.34376 52.48248, 13.3~
    ##  2 s06_06e~    489 10305.      475. Geschloss~ (((13.34525 52.481, 13.346~
    ##  3 s06_06e~    727 11640.      625. Geschloss~ (((13.3465 52.48059, 13.34~
    ##  4 s06_06e~    296 14027.      211. Entkernte~ (((13.34122 52.47999, 13.3~
    ##  5 s06_06e~    282  7264.      388. Geschloss~ (((13.34313 52.48067, 13.3~
    ##  6 s06_06e~    432  6630.      652. Blockrand~ (((13.34382 52.4792, 13.34~
    ##  7 s06_06e~    268  5894       455. Geschloss~ (((13.34519 52.47962, 13.3~
    ##  8 s06_06e~    936 21202.      441. Geschloss~ (((13.34459 52.47861, 13.3~
    ##  9 s06_06e~    500  9842.      508. Dichte Bl~ (((13.3473 52.48076, 13.34~
    ## 10 s06_06e~    499  9300.      537. Dichte Bl~ (((13.34744 52.48025, 13.3~
    ## # ... with 14,720 more rows

``` r
sf_save(z, "Einwohnerdichte2017")
```

    ## Deleting source `Einwohnerdichte2017/Einwohnerdichte2017.geojson' using driver `GeoJSON'
    ## Writing layer `Einwohnerdichte2017' to data source `Einwohnerdichte2017/Einwohnerdichte2017.geojson' using driver `GeoJSON'
    ## features:       14730
    ## fields:         5
    ## geometry type:  Multi Polygon
    ## Deleting source `Einwohnerdichte2017/Einwohnerdichte2017.gml' using driver `GML'
    ## Writing layer `Einwohnerdichte2017' to data source `Einwohnerdichte2017/Einwohnerdichte2017.gml' using driver `GML'
    ## features:       14730
    ## fields:         5
    ## geometry type:  Multi Polygon
    ## Deleting source `Einwohnerdichte2017/Einwohnerdichte2017.kml' using driver `KML'
    ## Writing layer `Einwohnerdichte2017' to data source `Einwohnerdichte2017/Einwohnerdichte2017.kml' using driver `KML'
    ## features:       14730
    ## fields:         5
    ## geometry type:  Multi Polygon
    ## Deleting source `Einwohnerdichte2017/Einwohnerdichte2017.shp' using driver `ESRI Shapefile'
    ## Writing layer `Einwohnerdichte2017' to data source `Einwohnerdichte2017/Einwohnerdichte2017.shp' using driver `ESRI Shapefile'
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

## Tempolimits

``` r
z <- sf_fisbroker("re_vms_tempolimits")
```

    ## [1] "http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/re_vms_tempolimits?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES=re_vms_tempolimits"

``` r
z
```

    ## Simple feature collection with 29146 features and 12 fields
    ## geometry type:  MULTILINESTRING
    ## dimension:      XY
    ## bbox:           xmin: 13.11188 ymin: 52.34041 xmax: 13.74162 ymax: 52.65995
    ## epsg (SRID):    4326
    ## proj4string:    +proj=longlat +datum=WGS84 +no_defs
    ## # A tibble: 29,146 x 13
    ##    gml_id spatial_name spatial_alias spatial_type ELEM_NR VRICHT_TXT
    ##    <chr>         <int>         <int> <chr>        <chr>   <chr>     
    ##  1 re_vm~         8125          8125 MultiLineSt~ 405400~ beide Ric~
    ##  2 re_vm~         8154          8154 MultiLineSt~ 445900~ beide Ric~
    ##  3 re_vm~         8215          8215 MultiLineSt~ 404900~ beide Ric~
    ##  4 re_vm~         8218          8218 MultiLineSt~ 404900~ beide Ric~
    ##  5 re_vm~         8219          8219 MultiLineSt~ 404900~ beide Ric~
    ##  6 re_vm~         8289          8289 MultiLineSt~ 426500~ beide Ric~
    ##  7 re_vm~         8297          8297 MultiLineSt~ 386700~ beide Ric~
    ##  8 re_vm~         8298          8298 MultiLineSt~ 386700~ beide Ric~
    ##  9 re_vm~         8299          8299 MultiLineSt~ 386700~ beide Ric~
    ## 10 re_vm~         8323          8323 MultiLineSt~ 484700~ beide Ric~
    ## # ... with 29,136 more rows, and 7 more variables: WERT_VES <int>,
    ## #   DURCH_T <chr>, ZEIT_T <chr>, DANN_T <chr>, DAT_T <chr>, TAG_T <chr>,
    ## #   geometry <MULTILINESTRING [Â°]>

``` r
sf_save(z, "Tempolimit")
```

    ## Deleting source `Tempolimit/Tempolimit.geojson' using driver `GeoJSON'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.geojson' using driver `GeoJSON'
    ## features:       29146
    ## fields:         12
    ## geometry type:  Multi Line String
    ## Deleting source `Tempolimit/Tempolimit.gml' using driver `GML'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.gml' using driver `GML'
    ## features:       29146
    ## fields:         12
    ## geometry type:  Multi Line String
    ## Deleting source `Tempolimit/Tempolimit.kml' using driver `KML'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.kml' using driver `KML'
    ## features:       29146
    ## fields:         12
    ## geometry type:  Multi Line String

    ## Warning in abbreviate_shapefile_names(obj): Field names abbreviated for
    ## ESRI Shapefile driver

    ## Deleting source `Tempolimit/Tempolimit.shp' using driver `ESRI Shapefile'
    ## Writing layer `Tempolimit' to data source `Tempolimit/Tempolimit.shp' using driver `ESRI Shapefile'
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
