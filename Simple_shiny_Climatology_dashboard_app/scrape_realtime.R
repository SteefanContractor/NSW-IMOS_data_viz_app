# Scrape Manly Hydraulics Waverider buouy sea temperature data and Randwick Council Lifeguard Reports

library(rvest)
library(stringr)
library(lubridate)

# Check if csv files exist and create them if not
if (!file.exists("./data/realtime/Manly_hydraulics.csv")) {
  system("echo site_name, site_code, sst, at, max, max_at, min, min_at > ./data/realtime/Manly_hydraulics.csv")
}
if (!file.exists("./data/realtime/Randwick_council.csv")) {
  system("echo site_name, sst, at > ./data/realtime/Randwick_council.csv")
}

# Manly Hydraulics waverider buouy data
sites <- c("Sydney", "Byron Bay", "Coffs Harbour", "Crowdy Head", "Port Kembla", "Bateman\\'s Bay", "Eden")
site_codes <- c("SYDDOW","BYRBOW","COFHOW","CRHDOW","PTKMOW","BATBOW","EDENOW")

data <- data.frame()
for (s in 1:7) {
  webpage <- read_html(str_c("https://www.mhl.nsw.gov.au/data/realtime/sst/",site_codes[s])) #https://www.mhl.nsw.gov.au/data/realtime/sst/BATBOW
  para <- html_nodes(webpage, 'p') %>% html_text() 
  para <- str_split(para[1], "\r\n      \r\n      ",simplify = T) %>% str_replace_all("\r\n    ", "")
  latest <- as.numeric(str_match(para[1], "Latest:(.*?)\\(Degrees C\\)")[,2])
  latest_at <- ymd_hms(str_match(para[1], "\\(Degrees C\\) @(.*?)AEST")[,2])
  max <- as.numeric(str_match(para[2], "Maximum:(.*?)\\(Degrees C\\)")[,2])
  max_at <- ymd_hms(str_match(para[2], "\\(Degrees C\\) @(.*?)AEST")[,2])
  min <- as.numeric(str_match(para[3], "Minimum:(.*?)\\(Degrees C\\)")[,2])
  min_at <- ymd_hms(str_match(para[3], "\\(Degrees C\\) @(.*?)AEST")[,2])
  # data_row <- data.frame(site_name = sites[s], site_code = site_codes[s], SST = latest, at = latest_at, max = max, max_at = max_at, min = min, min_at = min_at)
  # data <- rbind(data, data_row)
  system(paste("echo", paste(sites[s], site_codes[s], latest, latest_at, max, max_at, min, min_at, sep=", "), ">> ./data/realtime/Manly_hydraulics.csv"))
  Sys.sleep(0.5)
}


# Randwick Council Lifeguard Reports
webpage <- read_html("https://www.randwick.nsw.gov.au/facilities-and-recreation/beaches-and-coast/lifeguard-reports")
listing <- html_nodes(webpage, 'dl.listing__definitions') %>% html_text()
listing_times <- html_nodes(webpage, 'p.listing__emphasis') %>% html_text()
clovelly_temp <- as.numeric(str_match(listing[1], "Water temp\r\n\t\t\t\t(.*?)C\r\n\t\t\t\tWave height")[,2])
clovelly_time <- dmy_hm(listing_times[1])
system(paste("echo", paste("Clovelly beach", clovelly_temp, clovelly_time, sep=", "), ">> ./data/realtime/Randwick_council.csv"))
# data_row <- data.frame(site_name = "Clovelly", site_code = "CLOVLY", SST = clovelly_temp, at = clovelly_time, max = NA, max_at = NA, min = NA, min_at = NA)
# data = rbind(data, data_row)
maroubra_temp <- as.numeric(str_match(listing[2], "Water temp\r\n\t\t\t\t(.*?)C\r\n\t\t\t\tWave height")[,2])
maroubra_time <- dmy_hm(listing_times[2])
system(paste("echo", paste("Maroubra beach", maroubra_temp, maroubra_time, sep=", "), ">> ./data/realtime/Randwick_council.csv"))
# data_row <- data.frame(site_name = "Maroubra", site_code = "MRUBRA", SST = maroubra_temp, at = maroubra_time, max = NA, max_at = NA, min = NA, min_at = NA)
# data = rbind(data, data_row)
coogee_temp <- as.numeric(str_match(listing[3], "Water temp\r\n\t\t\t\t(.*?)C\r\n\t\t\t\tWave height")[,2])
coogee_time <- dmy_hm(listing_times[3])
system(paste("echo", paste("Coogee beach", coogee_temp, coogee_time, sep=", "), ">> ./data/realtime/Randwick_council.csv"))
# data_row <- data.frame(site_name = "Coogee", site_code = "COOGEE", SST = coogee_temp, at = coogee_time, max = NA, max_at = NA, min = NA, min_at = NA)
# data = rbind(data, data_row)
