#install.packages('rsconnect')
library(rsconnect)
##authorize shinny app acount
rsconnect::setAccountInfo(name='discwt', 
                          token='5770DBC7DB8551AF666359CED6E8E083', 
                          secret='qZGoSqFE4P1KpKtPkS3aUQgYwbiDVvR0hl/25sb5')

rmarkdown::render("README.Rmd", output_format = "md_document")
rsconnect::deployApp()

