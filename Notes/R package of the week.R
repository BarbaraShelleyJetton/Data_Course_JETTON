#qrcode####
#The QR code package will generate QR codes for you
install.packages("qrcode")
library(qrcode)
url <- "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
qr <- qrcode::qr_code(url)
plot(qr)
install.packages("tidyverse")

#tidyverse####
#installs multiple packages that amke cleaning data better 
#dplyr in tidyverse masks the automatically started stats package
stats::filter()
dplyr::filter()

#ggmap ####
#creates google maps with R uisng GG plot knowledge

#leaflet ####
#creates maps that allow for 

#colorblindr ###
#gives you your plot based off color blind options so you can see what looks like to them and can change it

#measurements ###
install.packages("measurements")
library(measurements)
x <- c(12,31,44)
measurements::conv_unit(x, from = 'inch', to = 'ft')
measurements::conv_unit(x, from = 'inch', to = 'parsec')

#pdftools###
#does everything you want a program that works with pdf to do but it is free