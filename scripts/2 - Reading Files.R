##################################################################################################################
##################################################################################################################
#####################################                                                 ############################
#####################################               PARTY STATUTE PROJECT             ############################
#####################################                                                 ############################
##################################################################################################################
##################################################################################################################



#####################################    PART II: READING  PARTY STATUTES    #####################################



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


#### 1) Method 1: Reading pdf files with pdftools ####

# Obs: this with high-quality images or text-based pdfs
# We shall use another method in order to read the eventual remaining pdfs

statute_files <- list.files(pattern = "*_statute_project.pdf") %>%
  map_df(~ {
    pdf_text(.x) %>%
      paste(., collapse = "\r\n") %>% # collapse vector into one single string
      as_tibble() %>%
      mutate(file = .x)
  }) %>%
  separate(., file, into = c("party", "name"), sep = "---", remove = F, extra = "merge", fill = "right") %>%
  mutate(
    value = gsub("[\r\n]{3,}", "", value),
    value = gsub("^\003$", "", value),
    value = ifelse(value == "", NA, value)
  )


#### 2) Method 2: Reading pdf files with tesseract ####

## 2.1) Converting pages into .PNGs
png_files <- statute_files %>%
  filter(is.na(value)) %>%
  select(file) %>%
  as_vector()

my_files <- list.files(pattern = ".*pdf$")


my_files %>%
  subset(. %in% my_files) %>%
  map(~ {
    pdf_convert(.x,
      dpi = 600,
      format = "png"
    )
  })


## 2.2) Improving quality of converted pages
images_to_clean <- list.files(pattern = "_statute_project_") %>% # filtering files that we couldnt read with pdf_text
  subset(!grepl("clean", .))

for (i in 1:length(images_to_clean)) {
  image_read(images_to_clean[i]) %>%
    image_resize("2000") %>% # resize image
    image_convert(type = "grayscale") %>% # convert pic to greyscale
    image_modulate(brightness = 120) %>% # change brightness
    image_enhance() %>% # tries to minimize noise
    image_contrast() %>% # enhances intensity differences in image
    image_median() %>% # replaces each pixel with the median color in a circular neighborhood
    image_trim() %>% # automatically removes edges that are the background color from the image
    image_write(., path = paste0("clean_", images_to_clean[i]), format = "png")
  gc()
  print(i)
}


## 2.3) Installing Portuguese dictionary
# This will improve the results of our OCR
if (is.na(match("por", tesseract_info()$available))) tesseract_download("por")
portuguese <- tesseract("por")


## 2.4) Reading Improved .PNGs and saving them as .TXT
# images_to_read <- list.files(pattern = 'clean_') %>%
images_to_read <- list.files(pattern = ".*png") %>%
  gsub("\\.png", "", .) %>%
  gsub("\\.txt", "", .) %>%
  as_tibble() %>%
  group_by(value) %>%
  summarise(n = n()) %>%
  filter(n == 1) %>%
  select(value) %>%
  mutate(value = paste0(value, ".png")) %>%
  as_vector()

for (i in 1:length(images_to_read)) {
  image_read(images_to_read[i]) %>%
    ocr(engine = portuguese) %>%
    as_tibble() %>%
    write_tsv(., path = paste0(images_to_read[i], ".txt"))
  gc()
}



## 2.5) Reading .TXTs
statute_files_2 <- list.files(pattern = ".txt") %>%
  map_df(~ {
    read_table(.x) %>%
      paste(., collapse = "\r\n") %>%
      as_tibble() %>%
      mutate(file = .x)
  }) %>%
  mutate(
    file = gsub("(_project_)(.*)", "_project", file),
    file = gsub("clean_", "", file),
    file = paste0(file, ".pdf"),
    value = gsub("c\\(", "", value),
    value = gsub("\\)$", "", value),
    value = gsub('",', "", value),
    value = gsub('"', "", value),
    value = gsub("\\\\", "", value)
  ) %>%
  group_by(file) %>%
  summarise(value = paste(value, collapse = "\r\n")) %>%
  ungroup() %>%
  separate(., file, into = c("party", "name"), sep = "---", remove = F, extra = "merge", fill = "right") %>%
  select(value, everything()) %>%
  mutate(
    value = gsub("[\r\n]{3,}", "", value),
    value = ifelse(value == "", NA, value)
  )


#### 3) Joining files from both methods of extraction ####
statute_files %<>%
  filter(!is.na(value)) %>%
  bind_rows(., statute_files_2 %>%
    bind_rows(statute_files_3) %>%
    filter(!is.na(value)))


#### 4) Checking if we didnt leave any file out ####
check_left_out <- list.files(pattern = "_statute_project.pdf") %>%
  as_tibble() %>%
  rename("file" = "value") %>%
  anti_join(., statute_files %>%
    select(file))

check_left_out # looks ok!


#### 5) Saving our data ####
write_rds(statute_files, "statute_files.rds")


# End of File