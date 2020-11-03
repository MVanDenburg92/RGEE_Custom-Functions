#Install greenbrown and RGEE

#RGEE
remotes::install_github("r-spatial/rgee")
library(rgee)

#Install ee_install to get earth engine function and intialize
ee_install()
ee_Initialize(email = 'shreenapyakurel@gmail.com')

#if there is an error saying python enviornment is not available or something related run these 2 lines and repeat previous 2 lines
 ee_clean_pyenv()
 ee_clean_credentials()


#load this AFTER ee is initalized
library(reticulate)


#Install greenbrown
install.packages("greenbrown", repos="http://R-Forge.R-project.org")
library(greenbrown)

