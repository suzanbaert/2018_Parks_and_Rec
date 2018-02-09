library(pdftools)
library(stringr)
library(dplyr)


#reading the harvest festival script
#returns a character vector of 34, each representing a page 
pdf_script <- pdf_text("data/parks-and-recreation-3x07-harvest-festival-2011.pdf")

#remove the first two pages that are cover sheets
pdf_script <- pdf_script[3:34] %>%
  #removing the first lines on every page that are just title text
  str_replace("PARKS.+\\\r\\n.+10", "") 


#extract all indented text
#this removes any description senteces such as "People shake their heads"
indented_text <- pdf_script %>%
  str_extract_all(pattern = "\r\n\\s{5,}.+") %>%
  unlist()

#this works but it splits the phrases:
clean_indented_text <- indented_text %>%
  str_replace("(END OF )?COLD OPEN", "") %>%  #removes cold open and end of cold open text
  str_replace("(END OF )?ACT [A-Z]+", "") %>%  #removes text as ACT ONE
  str_replace("TAG", "") %>%
  str_replace("END OF SHOW", "") %>%  
  str_replace("\\(.+\\)", "") %>%  #removes anything between brackets
  str_replace("\r\n\\s+", "")  #removes the new line and extra spaces


#collapsed in one chunk
one_long_text <- paste(clean_idented_text, collapse = " ")


#words quite well, but trips up add " I " and " I'm ", "I've "
#str_view_all(one_long_text, "[A-Z]{3,}(\\s*[A-Z]?[a-z]+\\W*)+") 

clean_script <- str_extract_all(one_long_text, "[A-Z]{3,}(\\s*\\W*[A-Z]?\\W*[a-z]+\\W*\\d*)+")  
clean_script <- unlist(clean_script)


#making a dataframe
script_split <- str_split(clean_script, pattern = " ", n=2)
script_split_t <- purrr::transpose(script_split)

script_S03E07 <- tibble(episode = "S03E07 Harvest Festival",
                        speaker = unlist(script_split_t[[1]]),
                        text = unlist(script_split_t[[2]]))


saveRDS(script_S03E07, "data/script_S03E07")
 