library(pdftools)
library(stringr)
library(dplyr)



### HARVEST FESTIVAL ####
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
one_long_text <- paste(clean_indented_text, collapse = " ")


#words quite well, but trips up add " I " and " I'm ", "I've "
#str_view_all(one_long_text, "[A-Z]{3,}(\\s*[A-Z]?[a-z]+\\W*)+") 

clean_script <- str_extract_all(one_long_text, "[A-Z]{3,}(\\s*\\W*[A-Z]?\\W*[a-z]+\\W*\\d*)+")  
clean_script <- unlist(clean_script)


#making a dataframe
script_split <- str_split(clean_script, pattern = " ", n=2)
script_split_t <- purrr::transpose(script_split)

script_3x07 <- tibble(episode = "3x07 Harvest Festival",
                        speaker = unlist(script_split_t[[1]]),
                        text = unlist(script_split_t[[2]]))





#### ####

#### FUNCTION FOR ALL OTHERS ####

clean_pr_script <- function(pdf_script, episodeinfo){
  
  indented_text <- pdf_script %>%
    #removing the first lines on every page that are just title text
    str_replace("PARKS.+\\\r\\n.+10", "") %>% 
    str_extract_all(pattern = "\r\n\\s{5,}.+") %>%
    unlist()

  
  clean_indented_text <- indented_text %>%
    str_replace("(END OF )?COLD OPEN", "") %>%  #removes cold open and end of cold open text
    str_replace("(END OF )?ACT [A-Z]+", "") %>%  #removes text as ACT ONE
    str_replace("TAG", "") %>%
    str_replace("END OF SHOW", "") %>%  
    str_replace("\\(.+\\)", "") %>%  #removes anything between brackets
    str_replace("\r\n\\s+", "")  #removes the new line and extra spaces 
  
  #pasting into one long text
  one_long_text <- paste(clean_indented_text, collapse = " ")
  
  #splitting by speaker
  clean_script <- str_extract_all(one_long_text, "[A-Z]{3,}(\\s*\\W*[A-Z]?\\W*[a-z]+\\W*\\d*)+")  
  clean_script <- unlist(clean_script)
  
  
  #making a dataframe
  script_split <- str_split(clean_script, pattern = " ", n=2)
  script_split_t <- purrr::transpose(script_split)
  
  output <- tibble(episode = episodeinfo,
         speaker = unlist(script_split_t[[1]]),
         text = unlist(script_split_t[[2]]))
  
  mutate_all(output, str_trim)
}




#reading the others
pdf_3x05 <- pdf_text("data/parks-and-recreation-3x05-media-blitz-2011.pdf")
pdf_3x05 <- pdf_3x05[3:36] 
script_3x05 <- clean_pr_script(pdf_3x05, "3x05 Media Blitz")


pdf_2x07 <- pdf_text("data/Parks_and_Recreation_2x07.pdf")
pdf_2x07 <- pdf_2x07[3:41] 
script_2x07 <- clean_pr_script(pdf_2x07, "2x07 Greg Pikitis")
script_2x07 <- script_2x07[2:308,]
script_2x07$text <- str_replace(script_2x07$text, "take one.\\s+", "")


pdf_1x02 <- pdf_text("data/parks-and-recreation-1x02-canvassing-2009.pdf")
pdf_1x02 <- pdf_1x02[2:38] 
script_1x02 <- clean_pr_script(pdf_1x02, "1x02 Canvassing")

pdf_2x02 <- pdf_text("data/parks-and-recreation-2x02-the-stakeout-2009.pdf")
pdf_2x02 <- pdf_2x02[5:45] 
script_2x02 <- clean_pr_script(pdf_2x02, "2x02 The Stakeout")

pdf_2x04 <- pdf_text("data/parks-and-recreation-2x04-practice-date-2009.pdf")
pdf_2x04 <- pdf_2x04[3:36] 
script_2x04 <- clean_pr_script(pdf_2x04, "2x04 The Practice Date")






script <- bind_rows(script_1x02, script_2x02, script_2x04, script_2x07,
                    script_3x05, script_3x07)
saveRDS(script, "data/script.RDS")
