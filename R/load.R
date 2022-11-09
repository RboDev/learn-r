#' Load data
#' 
#' references:
#' https://www.robwiederstein.org/2021/03/05/xml-to-dataframe/
#' https://appsilon.com/r-xml/
#' 
#' XML
#' https://www.w3schools.com/xml
#' 


# ==== Load Data ====

# ---- Request ----
library(httr2)

url <- "https://www.data.gouv.fr/fr/datasets/r/087dfcbc-8119-4814-8412-d0a387fac561"

req <- request(url)
req |> req_perform(path="req.zip")

# ---- unzip ----
library(zip)
unzip("req.zip")

# Full file
xml_file <- "PrixCarburants_instantane.xml"
# Shorter file
xml_file <- "PrixCarburants_short.xml"

# ==== parse XML ====
library(XML)

doc <- xmlTreeParse(xml_file)
(pdv <- doc$doc$children$pdv_liste[[1]])

# Get PDV attributes
xmlAttrs(pdv)

# Get PDV fuel prices
xmlChildren(pdv)

# ----
doc <- xmlParse(xml_file)
pdvs <- getNodeSet(doc, "/pdv_liste//pdv")

xmlToDataFrame(nodes=pdvs)  
# ! works only for very simple data structures (not nested)

# list of PDV Ids
(ids <- sapply(pdvs, function(el) xmlGetAttr(el, "id")))

# TODO: parse function to build the data frame.

# ==== parse XML2 ====
library(xml2)
library(purrr)
library(dplyr)


xml_doc <- read_xml(xml_file)

# XML contains a list of Point De Vente
xml_name(xml_doc)

# ---- Single PDV to DF ----

(pdv <- xml_find_first(xml_doc, ".//pdv"))

parse_pdv <- function(pdv) {
  
  df_pdv <- pdv |>
    xml_attrs() |>
    t() |>
    data.frame()
  
  #TODO: extract the other information in the XML if required (opening hours, etc..)
  
  # Multiple fuel prices
  df_fuel_price <- pdv |>
    xml_find_all("prix") |>
    xml_attrs() |>
    map(t) |>
    map(data.frame) |>
    bind_rows() |>
    rename(fuel_id = id)
  
  # bind the 2 data frames to get flat data
  bind_cols(df_pdv,df_fuel_price)
}

# ---- All PDV to DF ----

# find all the pdv
pdvs <- xml_find_all(xml_doc, "./pdv")

df <- pdvs |>
  map(parse_pdv) |>
  bind_rows()



