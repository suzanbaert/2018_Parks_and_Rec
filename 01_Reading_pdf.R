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

clean_indented_text <- indented_text %>%
  str_replace("(END OF )?COLD OPEN", "") %>%  #removes cold open and end of cold open text
  str_replace("ACT [A-Z]+", "") %>%  #removes text as ACT ONE
  str_replace("\\(.+\\)", "") #%>%  #removes anything between brackets
  #str_replace("\r\n\\s+", "")  #removes the new line and extra spaces


  
  #remains removes anything between brackes
  


str_replace_all(script_quotes, "\r\n\\s+", "")


str_view(script_quotes, "\\(.+\\)")


test <- pdf_script[2]

##THIS ONE WORKS: GETS ALL TALKING PEOPLE
#looks for a single uppercase word that is alone on its line
str_view_all(test, "\\\r\n\\s*[A-Z]{2,}\\\r\n")


#identifying the indented lines - this removes the commentary lines such as "People shake their heads"
str_view_all(test, "\\\r\n\\s{3,}.+")

str_view(test, "(END OF )?COLD OPEN")




test %>%
  str_replace_all("\\\r\\n", "") %>%
  str_view_all(" [A-Z]{2,}.+(?!TOM)")

str_extract_all(test, " [A-Z]{2,}")
str_split(test, " [A-Z]{2,}" )


str_split(pdf_script, pattern)

q <- "OLGA is een gans. KATRIEN is much smarter"
str_which(q, "[A-Z]{2,}")


str_view_all("bacaccad", "a(?!c{2,})")



#I want to get chunks of text
#this gives every ine
script <- str_split(pdf_script, pattern = "\\\r\\n")

unlist(script)




#script gives a vector per page
#Page 1 and 2 are unnecessary info so I will remove them
pdf_script_no_intro <- pdf_script[3:34]
test <- data.frame(text = pdf_script_no_intro)
test <- mutate(text = cat(text))

#converting to usual text and putting in a dataframe


#there is a header on op of every page
#PARKS AND RECREATION         “Harvest Festival”     [03009]    1.\r\n  1st Revised Blue Shooting Draft                     10/07/10
script <- str_replace(script, "PARKS.+\\\r\\n.+10", "")


#split vectors into chunks based on \r\n
script <- str_split(script, pattern = "\\\r\\n")
cat(script[[1]])


#`not working et`

str_view_all(script, "[A-Z]+")


str_extract_all(script, "\\\r\\n\\s[A-Z]+\\s\\\r\\n")

str_replace(script, "\\s+", "\\s")




