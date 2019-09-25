library(tidyverse)
library(readxl)
library(stringi)
library(rvest)
library(pdftools)

# load the excel files, can be downloaded from: https://renates2.dgeec.mec.pt/ppsq.asp?I=0&U=3135&N=&G=3&T=&A=2018&Ar=&Tt=&Es=1,2&Dc=1&Lc=24&Tc=Engenharia%20Inform%E1tica&Cc=0&Lc2=0&Tc2=&Cc2=0&Lc3=0&Tc3=&Cc3=0&Or=0&Pf=
files <-
  list.files(path = "./data",
             pattern = "Teses.*\\.xlsx$",
             full.names = T)
tbl <- files %>% map(read_xlsx) %>% bind_rows()

# some information about the tibble
glimpse(tbl)
ncol(tbl)
nrow(tbl)
View(tbl)

# sometimes the advisors names dont have accents and sometimes they do
tbl$Orientadores %>% unique %>% sort

# select the important columns and clean some data
tbl2 <- tbl %>%
  rename(
    ID = "Identificador TID",
    Sex = "Sexo do autor",
    Nationality = "Nacionalidade do autor",
    Specialization = "Especialidade/Especialização",
    Title = "Título do trabalho",
    Keywords = "Palavras chave",
    Date = "Data do grau",
    Grade = "Classificação final do curso",
    Advisors = "Orientadores",
    URL = "URL do depósito RCAAP"
  ) %>%
  select(ID,
         Sex,
         Nationality,
         Specialization,
         Title,
         Keywords,
         Date,
         Grade,
         Advisors,
         URL) %>%
  mutate(
    Grade = parse_number(.$Grade),
    Specialization = sub("Área de especialização: ", "", .$Specialization),
    Advisors = stri_trim(stri_trans_general(.$Advisors, "Any-Title; Latin-ASCII"))
  )

# reduced from 60 to 48 advisors
tbl2$Advisors %>% unique %>% sort %>% length

# function to return the thesis pdf url or NA if access is restricted
scrape_pdf_url <- function(url) {
  page_html <- read_html(url)
  
  # PDF cannot be downloaded if access is restricted
  if (grepl('Acesso Restrito.', as.character(page_html))) {
    return (NA)
  }
  
  pdf_relative_url <- page_html %>%
    html_node('table > tr:nth-child(2) > td:nth-child(5) > a') %>%
    html_attr('href')
  
  pdf_absolute_url <-
    paste0('https://recipp.ipp.pt', pdf_relative_url)
  return (pdf_absolute_url)
}

# scrape the pdf urls from the repository urls 
tbl3 <- tbl2 %>% mutate(URL = map(URL, scrape_pdf_url) %>% unlist)

# 248 pdfs to download out of 292 theses
tbl3 %>% filter(!is.na(URL)) %>% count

# function to download a pdf
download_pdf <- function(url, output_file) {
  if (!is.na(url)) {
    download.file(url, paste0("./data/pdfs/", output_file), mode = 'wb')
  }
}

# download all pdfs in sequence
walk2(tbl3$URL, paste0(tbl3$ID, ".pdf"), download_pdf)

# functions to extract data from pdfs
count_pages <- function(pdf) {
  pdf_info(pdf)$pages
}
count_words <- function(pdf) {
  text <- pdf_text(pdf)
  # sapply is similar to map and unlist, while lapply is like map
  sum(sapply(strsplit(text, "\\s+"), length))
}
pdf_size <- function(pdf) {
  file.info(pdf)$size / 1000^2
}
pdf_location = function(id) {
  paste0("./data/pdfs/", id, ".pdf")
}

# add the pdf information to the table
tbl4 <- tbl3 %>%
  mutate(
    Pages = map2(ID, URL, function(id, url) {
      ifelse(!is.na(url), count_pages(pdf_location(id)), NA)
    }) %>% unlist,
    Words = map2(ID, URL, function(id, url) {
      ifelse(!is.na(url), count_words(pdf_location(id)), NA)
    }) %>% unlist,
    Size = map2(ID, URL, function(id, url) {
      ifelse(!is.na(url), pdf_size(pdf_location(id)), NA)
    }) %>% unlist,
    WordsPerPage = Words / Pages
  ) %>%
  select(-URL)

# export the cleaned data to a csv
tbl4 %>% write_csv("./data/data.csv")
data <- read_csv("./data/data.csv")
glimpse(data)
