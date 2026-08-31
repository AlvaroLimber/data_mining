#Ejemplo API (UA1)
rm(list = ls())
library(jsonlite)
library(dplyr)
library(tibble)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
#######################################
#cucu.bo: https://docs.cucu.bo/bcb-api
#######################################
#oro
url <- "https://apibcb.cucu.bo/api/v1/tc/oro"
datos_json <- fromJSON(url)
df<-as_tibble(datos_json$oro)
print(df)
#tc oficial
url <- "https://apibcb.cucu.bo/api/v1/tc/oficial"
datos_json <- fromJSON(url)
df<-datos_json$tc_oficial |> as_tibble()
df<-datos_json$tc_oficial |> map_if(is.null, ~ NA) |> as_tibble()
print(df)
#######################################
#apify https://apify.com/
#######################################
#enlace de output/dataset/json
url_apify <- "https://api.apify.com/v2/datasets/MvZXy7gbkIk1eG4y3/items?signature=MC4xNzg5MDgwNzYzMjE5LjNGRzJrNVVkUjFHWFdNY1hpY3FX&format=json&clean=true"
df_maps <- fromJSON(url_apify, flatten = TRUE)
glimpse(datos_maps)

reviews<-datos_maps |> 
  select(title, reviews) |> 
  unnest(cols = c(reviews))