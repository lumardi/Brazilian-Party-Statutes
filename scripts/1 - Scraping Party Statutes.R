##################################################################################################################
##################################################################################################################
#####################################                                                 ############################
#####################################               PARTY STATUTE PROJECT             ############################
#####################################                                                 ############################
##################################################################################################################
##################################################################################################################

#####################################    PART I: SCRAPING OUR DATA    ###################################


#### 0. Configs ####

# Needed Libraries
pkgs <- c("tidyverse", "rvest")

# Install libraries
install <- function(x) {
  if (x %in% rownames(installed.packages()) == F) {
    install.packages(x,
      dependencies = T,
      repos = "http://cran.us.r-project.org"
    )
  }
}

lapply(pkgs, install)

# Loading libraries
lapply(pkgs, require, character.only = T)
rm(pkgs, install)


#### 1) Listing all links for party entries on TSE ####
party <- read_html("https://www.tse.jus.br/partidos/partidos-registrados-no-tse/registrados-no-tse") %>%
  html_table() %>%
  as.data.frame() %>%
  filter(!grepl("termos", SIGLA)) %>%
  mutate(
    link = read_html("https://www.tse.jus.br/partidos/partidos-registrados-no-tse/registrados-no-tse") %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      as_vector() %>%
      str_subset("partidos-registrados") %>%
      str_subset("tse/registrados|informacoes|historico", negate = T)
  ) %>%
  group_by(link) %>%
  nest() %>%
  mutate(
    estatuto = map(link, ~ {
      read_html(.x) %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        as_vector() %>%
        str_subset("estatuto")
    })
  ) %>%
  unnest(cols = c(data, estatuto))


#### 2) Downloading Party statutes ####
for (i in 1:nrow(party)) {
  try({
    download.file(
      url = paste0(party$estatuto[i], "/at_download/file"),
      destfile = paste0(party$SIGLA[i], "_", gsub(".*/", "", party$estatuto[i]), ".pdf"),
      mode = "wb"
    )
  })
}


#### 3) Scraping LOPP ####
lopp <- "https://www2.camara.leg.br/legin/fed/lei/1970-1979/lei-5682-21-julho-1971-357872-publicacaooriginal-1-pl.html" %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="content"]') %>%
  html_text() %>%
  enframe(name = NULL)

write_rds(lopp, "lopp.rds")


# End of File