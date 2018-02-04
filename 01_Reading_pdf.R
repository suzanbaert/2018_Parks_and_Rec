library(pdftools)

#reading the harvest festival script
pdf_script <- pdf_text("data/parks-and-recreation-3x07-harvest-festival-2011.pdf")

#I have a character vecor of 34 pages
class(pdf_script)
length(pdf_script)
