#' Basic preprocessing of text for classification/training
#'
#' Takes text and returns it lowercase, minus punctuation and stopwords.
#'
#'
#' @param text string or character vector
#'
#' @return
#' @export
#'
#' @examples
dpa_text_preprocess = function(text){
  
  library(tm)
  library(reticulate)
  
  ftfy=reticulate::import("ftfy")
  
  #must be a loop as ftfy can't do vectors afaik
  print("fixing mojibake...")
  for(i in 1:length(text)){
    text[i] = ftfy$fix_text(text[i])
  }
  
  
  return(text %>%
           gsub(pattern = "< ?br[\\s\\S]+", replacement = "", perl = T) %>% 
           removePunctuation() %>%
           tolower() %>%
           removeWords(tm::stopwords(kind = "en")) %>%
           gsub(pattern = "[‒–—]", replacement = "") %>% 
           gsub(pattern = "[^a-z]", replacement = " ") %>% 
           gsub(pattern = "\\s+", replacement = " ") %>% 
           trimws()
           
  )
  
}

