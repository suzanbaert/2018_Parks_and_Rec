library(pdftools)
library(stringr)

#reading the harvest festival script
pdf_script <- pdf_text("data/parks-and-recreation-3x07-harvest-festival-2011.pdf")

#I have a character vecor of 34 pages
class(pdf_script)
length(pdf_script)

#script gives a vector per page
#Page 1 and 2 are unnecessary info so I will remove them
script <- pdf_script[3:34]


#there is a header on op of every page
#PARKS AND RECREATION         “Harvest Festival”     [03009]    1.\r\n  1st Revised Blue Shooting Draft                     10/07/10
str_replace(script, "PARKS.+\\\r\\n.+10", "")



#`not working et`

str_extract_all(script, "\\\r\\n\\s[A-Z]+\\s\\\r\\n")

str_replace(script, "\\s+", "\\s")




