
# ABOUT -------------------------------------------------------------------

# Description: <The aim of this script>
# Usage: <How to run this script: what input it requires and output produced
# Author: <Your name>
# Date: <current date>

# SETUP -------------------------------------------------------------------

# Script-specific options or packages

pacman::p_load(tidyverse, stringr, readtext, tidytext, xml2)

# RUN ---------------------------------------------------------------------

# META-DATA ---------------------------------------------------------------

# Curate ACTIV-ES Corpus (zip) --------------------------------------------

# _ Filename meta-data ----------------------------------------------------

# Read the .cor files and extract the metadata from the filenames
aes <- 
  readtext(file = "data/original/actives/plain/*.run", # read each file .run
           docvarsfrom = "filenames", # get attributes from filename
           docvarnames = c("language", "country", "year", "title", "type", "genre", "imdb_id")) %>% # # add the column names we want for each attribute
  mutate(doc_id = row_number()) # change doc_id to numbers

glimpse(aes) # preview the data structure

# Write the curated dataset to the `data/derived/` directory
write_csv(x = aes, path = "data/derived/aes_plain.csv")

# Explore country sub-corpus distribution
aes_tokens <- 
  aes %>% 
  unnest_tokens(output = terms, input = text) # tokenize `text` into words `terms`

# - Corpus word term size
aes_tokens %>% 
  count() # count terms

# - Corpus size by country
aes_tokens %>% 
  count(country) # count terms by `country`

# - Sub-corpus proportions
aes_country_props <- 
  aes_tokens %>% 
  count(country) %>% # count terms by `country`
  mutate(prop = n / sum(n) ) # add the word term proportion for each country

# - Visualize sub-corpus proportions
aes_country_props %>% # pass the data frame as our data source
  ggplot(aes(x = country, y = prop)) + # create x- and y-axis mappings
  geom_col() + # visualize a column-wise geometry
  labs(x = "Country", y = "Proportion (%)", title = "ACTIV-ES Corpus Distribution", subtitle = "Proportion of words in each country sub-corpus")

ggsave(filename = "figures/aes_country_props.png", width = 5, height = 4)

# # Tidy term-tags
# aes[1, ] %>% 
#   unnest_tokens(term_text, text, token = "regex", pattern = " ") %>% 
#   separate(col = term_text, into = c("term", "tag"), sep = "/") %>% 
#   filter(tag == "n") %>% 
#   count(term, sort = TRUE)



# Curate Switchboard Corpus Dialogues (SDAC) -------------------------------

# _ Inline meta-data -------------------------------------------------------

# Read file by lines
doc <- read_lines(file = "data/original/sdac/sw00utt/sw_0001_4325.utt") 

# Extract `doc_id`, `speaker_a_id`, and `speaker_b_id`
doc_speaker_info <- 
  doc[str_detect(doc, "\\d+_\\d+_\\d+")] %>% # isolate pattern
  str_extract("\\d+_\\d+_\\d+") %>% # extract the pattern
  str_split(pattern = "_") %>% # split the character vector
  unlist() # flatten the list to a character vector
doc_id <- doc_speaker_info[1] # extract `doc_id`
speaker_a_id <- doc_speaker_info[2] # extract `speaker_a_id`
speaker_b_id <- doc_speaker_info[3] # extract `speaker_b_id`

# Extract `text`
text_start_index <- # find where header info stops
  doc %>% 
  str_detect(pattern = "={3,}") %>% # match 3 or more `=`
  which() # find vector index

text_start_index <- text_start_index + 1 # increment index by 1
text_end_index <- length(doc)

text <- doc[text_start_index:text_end_index] # extract text
text <- str_trim(text) # remove leading and trailing whitespace
text <- text[text != ""] # remove blank lines

data <- data.frame(doc_id, text) # tidy format `doc_id` and `text`

data <- # extract column information from `text`
  data %>% 
  mutate(damsl_tag = str_extract(string = text, pattern = "^.+?\\s")) %>%  # extract damsl tags
  mutate(speaker_turn = str_extract(string = text, pattern = "[AB]\\.\\d+")) %>% # extract speaker_turn pairs
  mutate(utterance_num = str_extract(string = text, pattern = "utt\\d+")) %>% # extract utterance number
  mutate(utterance_text = str_extract(string = text, pattern = ":.+$")) %>%  # extract utterance text
  select(-text)

data <-
  data %>% 
  separate(col = speaker_turn, into = c("speaker", "turn_num")) # separate speaker_turn into distinct columns

data <- # clean up column information
  data %>% 
  mutate(damsl_tag = str_trim(damsl_tag)) %>% # remove leading/ trailing whitespace
  mutate(utterance_num = str_replace(string = utterance_num, pattern = "utt", replacement = "")) %>% # remove 'utt'
  mutate(utterance_text = str_replace(string = utterance_text, pattern = ":\\s", replacement = "")) %>% # remove ': '
  mutate(utterance_text = str_trim(utterance_text)) # trim leading/ trailing whitespace

data <- # link speaker with speaker_id
  data %>% 
  mutate(speaker_id = case_when(
    speaker == "A" ~ speaker_a_id,
    speaker == "B" ~ speaker_b_id
  ))

glimpse(data)

extract_sdac_metadata <- function(file) {
  # Function: to read a Switchboard Corpus Dialogue file and extract metadata
  cat("Reading", basename(file), "...")
  doc <- read_lines(file) # read file by lines
  
  # Extract `doc_id`, `speaker_a_id`, and `speaker_b_id`
  doc_speaker_info <- 
    doc[str_detect(doc, "\\d+_\\d+_\\d+")] %>% # isolate pattern
    str_extract("\\d+_\\d+_\\d+") %>% # extract the pattern
    str_split(pattern = "_") %>% # split the character vector
    unlist() # flatten the list to a character vector
  doc_id <- doc_speaker_info[1] # extract `doc_id`
  speaker_a_id <- doc_speaker_info[2] # extract `speaker_a_id`
  speaker_b_id <- doc_speaker_info[3] # extract `speaker_b_id`
  
  # Extract `text`
  text_start_index <- # find where header info stops
    doc %>% 
    str_detect(pattern = "={3,}") %>% # match 3 or more `=`
    which() # find vector index
  
  text_start_index <- text_start_index + 1 # increment index by 1
  text_end_index <- length(doc)
  
  text <- doc[text_start_index:text_end_index] # extract text
  text <- str_trim(text) # remove leading and trailing whitespace
  text <- text[text != ""] # remove blank lines
  
  data <- data.frame(doc_id, text) # tidy format `doc_id` and `text`
  
  data <- # extract column information from `text`
    data %>% 
    mutate(damsl_tag = str_extract(string = text, pattern = "^.+?\\s")) %>%  # extract damsl tags
    mutate(speaker_turn = str_extract(string = text, pattern = "[AB]\\.\\d+")) %>% # extract speaker_turn pairs
    mutate(utterance_num = str_extract(string = text, pattern = "utt\\d+")) %>% # extract utterance number
    mutate(utterance_text = str_extract(string = text, pattern = ":.+$")) %>%  # extract utterance text
    select(-text)
  
  data <-
    data %>% 
    separate(col = speaker_turn, into = c("speaker", "turn_num")) # separate speaker_turn into distinct columns
  
  data <- # clean up column information
    data %>% 
    mutate(damsl_tag = str_trim(damsl_tag)) %>% # remove leading/ trailing whitespace
    mutate(utterance_num = str_replace(string = utterance_num, pattern = "utt", replacement = "")) %>% # remove 'utt'
    mutate(utterance_text = str_replace(string = utterance_text, pattern = ":\\s", replacement = "")) %>% # remove ': '
    mutate(utterance_text = str_trim(utterance_text)) # trim leading/ trailing whitespace
  
  data <- # link speaker with speaker_id
    data %>% 
    mutate(speaker_id = case_when(
      speaker == "A" ~ speaker_a_id,
      speaker == "B" ~ speaker_b_id
    )) 
  cat(" done.\n")
  return(data) # return the data frame object
}

# Get a list of the files to read
files <- 
  list.files(path = "data/original/sdac", 
             pattern = "\\.utt", 
             full.names = TRUE, 
             recursive = TRUE)

# Read files and return a tidy dataset
sdac <- 
  files %>% # pass file names
  map(extract_sdac_metadata) %>% # read and tidy iteratively 
  bind_rows() # bind the results into a single data frame

# Diagnostics
sdac[!complete.cases(sdac), ] # check for missing values
sdac$doc_id %>% unique() %>% length() # check for unique files

# _ Stand-off meta-data ----------------------------------------------------

# Column information: https://catalog.ldc.upenn.edu/docs/LDC97S62/caller_doc.txt

# Get the speaker meta-data
sdac_speaker_meta <- 
  read_csv(file = "https://catalog.ldc.upenn.edu/docs/LDC97S62/caller_tab.csv", 
           col_names = c("speaker_id", # changed from `caller_no`
                         "pin",
                         "target",
                         "sex",
                         "birth_year",
                         "dialect_area",
                         "education",
                         "ti",
                         "payment_type",
                         "amt_pd",
                         "con",
                         "remarks",
                         "calls_deleted",
                         "speaker_partition"))

glimpse(sdac_speaker_meta) # preview dataset

sdac_speaker_meta <- # remove double quotes
  sdac_speaker_meta %>% 
  map(str_replace_all, pattern = '"', replacement = '') %>% # iteratively replace doubled quotes
  bind_rows() %>%  # combine the results by rows
  type_convert() # return columns to orignal data types
  
glimpse(sdac_speaker_meta) # preview the dataset

sdac_speaker_meta <- # select columns of interest
  sdac_speaker_meta %>% 
  select(speaker_id, sex, birth_year, dialect_area, education)

# Join `sdac` with `sdac_speaker_meta` by `speaker_id`
sdac$speaker_id <- sdac$speaker_id %>% as.numeric() # convert to integer
sdac <- left_join(sdac, sdac_speaker_meta) # join by `speaker_id`

glimpse(sdac) # preview the joined dataset

# Diagnostics
sdac[!complete.cases(sdac), ] %>% glimpse # view incomplete cases
sdac[!complete.cases(sdac), ] %>% select(speaker_id) %>% unique() # id speaker(s) with incomplete information

sdac <- # remove speaker 155
  sdac %>% 
  filter(speaker_id != 155)

# Write the curated dataset to the `data/derived/` directory
write_csv(x = sdac, path = "data/derived/sdac.csv")

# ANNOTATION --------------------------------------------------------------

# Curate Brown Corpus (tei-xml) -------------------------------------------

file_path <- "data/original/brown/a01.xml"
doc <- read_xml(file_path) # read the xml document
doc %>% xml_ns_strip() # remove the namespace to simplify path matching

text_id <- # extract id
  doc %>% 
  xml_find_all("//text") %>% # isolate text element
  xml_attr("id") # pull the id attribute
text_class <-  # extract class
  doc %>% 
  xml_find_all("//text") %>% # isolate text element
  xml_attr("decls") # extract decls attribute

text_words <- # extract words 
  doc %>% 
  xml_find_all("//s//w|//c") %>% # isolate w and c elements
  xml_text() # pull the text from these elements
text_pos <- # extract pos tags
  doc %>% 
  xml_find_all("//s//w|//c") %>% # isolate w and c elements
  xml_attr("type") # pull the type attribute value for these elements

# Combine results in tabular format
data_frame(text_id, text_class, text_words, text_pos)

tidy_document <- function(file_path) {
  # Function: takes a path to an xml file from Brown Corpus
  # and extracts the id, class, word, and pos tag 
  
  doc <- read_xml(file_path) # read the xml document
  doc %>% xml_ns_strip() # remove the namespace to simplify path matching
  
  text_id <- # extract id
    doc %>% 
    xml_find_all("//text") %>% # isolate text element
    xml_attr("id") # pull the id attribute
  text_class <-  # extract class
    doc %>% 
    xml_find_all("//text") %>% # isolate text element
    xml_attr("decls") # extract decls attribute
  
  text_words <- # extract words 
    doc %>% 
    xml_find_all("//s//w|//c") %>% # isolate w and c elements
    xml_text() # pull the text from these elements
  text_pos <- # extract pos tags
    doc %>% 
    xml_find_all("//s//w|//c") %>% # isolate w and c elements
    xml_attr("type") # pull the type attribute value for these elements
  
  # Combine results in tabular format
  return(data_frame(text_id, text_class, text_words, text_pos))
}

tidy_text <- 
  tidy_document(file_path = "data/original/brown/a02.xml") %>% 
  group_by(text_id, text_class) %>% 
  summarise(text_full = paste(text_words, collapse = " "))

tidy_text %>% 
  unnest_tokens(bigrams, text_full, token = "characters") %>% 
  select(bigrams)

list.files(path = "data/original/brown", 
           pattern = "\\w\\d+.xml", 
           full.names = TRUE) %>% 
  map(tidy_document) %>% 
  bind_rows()

# LOG ---------------------------------------------------------------------

# Any descriptives that will be helpful to understand the results of this
# script and how it contributes to the aims of the project

sink(file = "log/data_derived_aes.log")
glimpse(aes)
sink()

sink(file = "log/data_derived_sdac.log")
glimpse(sdac)
sink()

# CLEAN UP ----------------------------------------------------------------

# Remove all current environment variables
rm(list = ls())
