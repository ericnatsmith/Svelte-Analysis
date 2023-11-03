# Load the udpipe library
library(udpipe)


# Define a function to remove proper nouns from a single string
remove_proper_nouns <- function(text) {
  require(udpipe)
  # Load the English model for udpipe
  ud_model <- udpipe_download_model(language = "english")
  ud_model <- udpipe_load_model(ud_model$file_model)
  # Tokenize the text using udpipe
  tokens <- udpipe_annotate(ud_model, x = text)
  tokens_df <- as.data.frame(tokens)
  # Replace proper nouns with "[NAME]" and concatenate the remaining tokens
  filtered_tokens <- ifelse(tokens_df$upos %in% c("PROPN"), "[NAME]", tokens_df$token)
  # Replace multiple consecutive instances of "[NAME]" with just one "[NAME]"
  filtered_tokens[is.na(filtered_tokens)] <- NULL
  for (i in length(filtered_tokens):2) {
    if (filtered_tokens[i] == "[NAME]" && filtered_tokens[i-1] == "[NAME]") {
      filtered_tokens[i] <- NA
    }
  }
  # Remove any trailing space before a period
  new_text <- gsub("\\s+\\.", ".", paste0(filtered_tokens[!is.na(filtered_tokens)], collapse = " "))
  return(new_text)
}

# Create a vector of open-response strings
responses <- c("I went to New York City to visit the Statue of Liberty.", 
               "John and Jane went to the store to buy groceries.", 
               "The Eiffel Tower is a famous landmark in Paris.")

# Apply the function to the vector of open-response strings
new_responses <- sapply(responses, remove_proper_nouns)

# Print the original responses and the new responses with proper nouns removed
cat("Original responses:\n", responses, "\n")
cat("\nNew responses with proper nouns removed:\n", new_responses, "\n")
