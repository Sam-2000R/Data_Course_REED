library(qrcode) #library means open package

url <- 'https:countryoaks.skepsecurity.com'
qr <- qr_code(url)
qr
plot(qr)

install.packages('tidyverse')
library(tidyverse)
 %>% #pipe, object from left is input to right