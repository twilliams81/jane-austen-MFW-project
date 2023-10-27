setwd("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/PhD/Fall 2023/Jane Austen/Code/MF Verb Project")

library(tidyverse)
library(cleanNLP)
library(reticulate)

# Specify the path to the Python executable
use_python("~/anaconda3/bin/python3.10")

# Initialize the `cleanNLP` package with the `spaCy` backend
cnlp_init_spacy(model="en_core_web_sm")

getwd()


# Directory containing all novels
novel_directory <- "~/Library/Mobile Documents/com~apple~CloudDocs/UVA/PhD/Fall 2023/Jane Austen/Code/Jane Austen Texts/"

list.files(novel_directory)



# List all text files in the directory
novel_files <- list.files(novel_directory, pattern = "*.txt", full.names = TRUE, recursive = TRUE)

# # Function to process a single novel
# process_novel <- function(file_path) {
#   # Extract filename without extension as an identifier
#   doc_id <- tools::file_path_sans_ext(basename(file_path))
#   
#   # Read the novel
#   novel_text <- read_lines(file_path)
#   
#   # Convert the text into a tibble for processing
#   novel <- tibble(text = novel_text)
#   
#   # Annotate the tokenized text for POS tagging
#   annotation <- cnlp_annotate(paste(novel$text, collapse = " "))
#   
#   # Extract tokens and their corresponding POS tags
#   tokens_pos <- annotation$token %>% as_tibble() %>% mutate(doc_id = !!doc_id)
#   
#   
#   # Find verbs that come after 'she'
#   verbs_after_she <- tokens_pos %>%
#     group_by(doc_id) %>%
#     mutate(next_word = lead(lemma),
#            next_pos = lead(upos)) %>%
#     filter(lemma == "she" & next_pos == "VERB") %>%
#     select(next_word)
#   
#   return(verbs_after_she)
# }
# 
# 

#Amended Function to cope with large novel

chunk_text <- function(text, chunk_size = 500000) {
  starts <- seq(1, nchar(text), by = chunk_size)
  ends <- c(starts[-1] - 1, nchar(text))
  chunks <- mapply(substr, start = starts, stop = ends, MoreArgs = list(x = text))
  return(chunks)
}

process_novel <- function(file_path) {
  # Extract filename without extension as an identifier
  doc_id <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the novel
  novel_text <- read_lines(file_path)
  
  # Combine all lines into a single string
  novel_string <- paste(novel_text, collapse = " ")
  
  # Break text into chunks of 500,000 characters each
  chunks <- chunk_text(novel_string)
  
  # Annotate each chunk separately
  annotation_list <- lapply(chunks, function(chunk) {
    cnlp_annotate(chunk)
  })
  
  # Combine annotations
  combined_annotation <- do.call(rbind, lapply(annotation_list, function(annot) annot$token))
  
  # Convert to tibble and assign doc_id
  tokens_pos <- as_tibble(combined_annotation) %>% mutate(doc_id = !!doc_id)
  
  # Find verbs that come after 'she'
  verbs_after_she <- tokens_pos %>%
    group_by(doc_id) %>%
    mutate(next_word = lead(lemma),
           next_pos = lead(upos)) %>%
    filter(lemma == "she" & next_pos == "VERB") %>%
    select(next_word)
  
  return(verbs_after_she)
}



# Process all novels and bind the results into a single tibble
all_verbs_after_she <- map_dfr(novel_files, process_novel)

# Count the frequency of each verb for each novel
verb_frequency <- all_verbs_after_she %>%
  count(doc_id, next_word, sort = TRUE)


total_verbs_per_doc <- verb_frequency %>%
  group_by(doc_id) %>%
  summarize(total_verbs = sum(n))

# Merge the total verbs data with the verb_frequency data
verb_frequency <- verb_frequency %>%
  left_join(total_verbs_per_doc, by = "doc_id") %>%
  mutate(percentage = (n / total_verbs) * 100)

# Display the most frequent verbs after 'she' with their respective percentages
head(verb_frequency)


#ALL NOVELISTS

# ... [rest of your code above]

# Directory containing the directories for all novelists
novelists_directory <- "~/Library/Mobile Documents/com~apple~CloudDocs/UVA/PhD/Fall 2023/Jane Austen/Code/Jane Austen Texts/"

# List all novelist directories
novelist_dirs <- list.dirs(novelists_directory, full.names = TRUE, recursive = FALSE)

# Function to process a single novelist's directory
process_novelist <- function(novelist_dir) {
  # Extract novelist's name from the directory name
  novelist_name <- basename(novelist_dir)
  
  # List all text files in the novelist's directory
  novel_files <- list.files(novelist_dir, pattern = "*.txt", full.names = TRUE)
  
  # Process all novels and bind the results into a single tibble
  verbs_after_she_novelist <- map_dfr(novel_files, process_novel)
  
  # Add novelist name to the tibble
  verbs_after_she_novelist %>%
    mutate(novelist = novelist_name)
}

# Process all novelists and bind the results into a single tibble
all_verbs_after_she <- map_dfr(novelist_dirs, process_novelist)

# Count the frequency of each verb for each novel
verb_frequency <- all_verbs_after_she %>%
  count(novelist, doc_id, next_word, sort = TRUE)

total_verbs_per_doc <- verb_frequency %>%
  group_by(novelist, doc_id) %>%
  summarize(total_verbs = sum(n))

# Merge the total verbs data with the verb_frequency data
verb_frequency <- verb_frequency %>%
  left_join(total_verbs_per_doc, by = c("novelist", "doc_id")) %>%
  mutate(percentage = (n / total_verbs) * 100)

# Compute the most frequent verbs across all novels by each novelist
novelist_verb_ranking <- verb_frequency %>%
  group_by(novelist, next_word) %>%
  summarize(total_count = sum(n)) %>%
  arrange(novelist, desc(total_count))

# Display the results
head(verb_frequency)
head(novelist_verb_ranking)


# Count the frequency of each verb for each novelist
verb_frequency_per_novelist <- all_verbs_after_she %>%
  count(novelist, next_word, sort = TRUE)

# Compute total verbs for each novelist
total_verbs_per_novelist <- verb_frequency_per_novelist %>%
  group_by(novelist) %>%
  summarise(total_verbs = sum(n))

# Compute the percentage for each verb within each novelist's corpus
verb_frequency_total <- verb_frequency_per_novelist %>%
  left_join(total_verbs_per_novelist, by = "novelist") %>%
  mutate(percentage = (n / total_verbs) * 100) %>% 
  group_by(novelist)


# View(verb_frequency)
# View(novelist_verb_ranking)
# 
# View(verb_frequency_total)

#write.csv2(verb_frequency_total, "vfTotalForAlyssa.csv")

#calculate novelists favourite verbs after SHE

agg_verb_frequency <- all_verbs_after_she %>%
  count(novelist, next_word) %>%
  group_by(novelist, next_word) %>%
  summarise(total = sum(n))

# Now, compute the total verbs per novelist
total_verbs_per_novelist <- agg_verb_frequency %>%
  group_by(novelist) %>%
  summarise(total_verbs = sum(total))

# Calculate the percentage
agg_verb_frequency <- left_join(agg_verb_frequency, total_verbs_per_novelist, by = "novelist") %>%
  mutate(percentage = (total / total_verbs) * 100) %>%
  arrange(novelist, desc(percentage))


View(agg_verb_frequency)

write_csv2(agg_verb_frequency, "verb frequency results 20231026.csv")


#trouble shooting

# List all novelist directories
novelist_dirs <- list.dirs(novelists_directory, full.names = TRUE, recursive = FALSE)
print(novelist_dirs)


test_output <- process_novel("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/PhD/Fall 2023/Jane Austen/Code/Jane Austen Texts/Jane Austen/emma.txt")
print(head(test_output))


test_novelist_output <- process_novelist("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/PhD/Fall 2023/Jane Austen/Code/Jane Austen Texts/Elizabeth Gaskell")
print(head(test_novelist_output))


#Troubleshooting the function
test_novelist_output <- process_novelist_test("~/Library/Mobile Documents/com~apple~CloudDocs/UVA/PhD/Fall 2023/Jane Austen/Code/Jane Austen Texts/Elizabeth Gaskell")
print(head(test_novelist_output))



process_novelist_test <- function(novelist_directory) {
  novelist_name <- basename(novelist_directory)
  novel_files <- list.files(novelist_directory, pattern = "*.txt", full.names = TRUE)
  
  results <- list()
  
  for(file_path in novel_files) {
    cat("Processing:", file_path, "\n")
    
    # Try processing the novel
    tryCatch({
      result <- process_novel(file_path)
      results[[length(results) + 1]] <- result
    }, error = function(e) {
      cat("Error processing:", file_path, "\nMessage:", e$message, "\n")
    })
  }
  
  # Combine results and add novelist_name
  all_verbs_after_she <- bind_rows(results) %>%
    mutate(novelist = novelist_name)
  
  return(all_verbs_after_she)
}



problematic_rows <- all_verbs_after_she %>%
  filter(novelist == "Elizabeth Gaskell", next_word == "'")

View(problematic_rows)
