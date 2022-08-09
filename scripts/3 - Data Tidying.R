##################################################################################################################
##################################################################################################################
#####################################                                                 ############################
#####################################               PARTY STATUTE PROJECT             ############################
#####################################                                                 ############################
##################################################################################################################
##################################################################################################################



#####################################    PART III: CLEANING OUR DATA    ##############################################


#### 0. Configs ####

# Needed Libraries
pkgs <- c(
  "tidyverse", "pdftools", "tesseract", "magick", "hunspell",
  "tm", "tidytext", "textclean", "stringdist", "lubridate"
)

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


### 1) Cleaning Text ###
statute_files <- statute_files
  mutate(
    across(everything(), enc2utf8), # change encoding to UTF-8
    value = as.character(value),
    value = gsub("[^A-zÄÅÁÂÀÃäáâàãÉÊËÈéêëèÍÎÏÌíîïìÖÓÔÒÕöóôòõÜÚÛüúûùÇç -]", "", value), # removing junk characters
    value = gsub("_|\\[", "", value), # removing junk characters
    value = removePunctuation(value, preserve_intra_word_dashes = T), # removing punctuation
    value = stemDocument(value, language = "portuguese"), # stemming document
    value = strip(value),
    value = gsub("[ ]{2,}", " ", value), # removing excess space
    value = gsub("[ ]{3,}", "", value), # removing excess space
    value = gsub("^[ ]", "", value), # removing excess space
    value = gsub("[ ]$", "", value), # removing excess space
    value = removeWords(value, stopwords("portuguese"))
  )

lopp <- lopp %>%
  mutate(
    across(everything(), enc2utf8),
    value = as.character(value),
    value = gsub("[^A-zÄÅÁÂÀÃäáâàãÉÊËÈéêëèÍÎÏÌíîïìÖÓÔÒÕöóôòõÜÚÛüúûùÇç -]", "", value),
    value = gsub("_|\\[", "", value),
    value = removePunctuation(value, preserve_intra_word_dashes = T),
    value = stemDocument(value, language = "portuguese"),
    value = removeNumbers(value),
    value = strip(value),
    value = gsub("[ ]{2,}", " ", value),
    value = gsub("[ ]{3,}", "", value),
    value = gsub("^[ ]", "", value),
    value = gsub("[ ]$", "", value),
    value = removeWords(value, stopwords("portuguese"))
  )

#### 2) Saving our data ####
write_rds(statute_files, "statute_files_clean.rds")
write_rds(lopp, "lopp_clean.rds")


# End of File
