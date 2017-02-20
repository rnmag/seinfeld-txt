## Getting the scripts ----
# Rafael N. Magalhães

# Load libraries
library(rvest)
library(dplyr)

# Download base url
url <- read_html("http://www.seinology.com/scripts-english.shtml")

# Extract links
link <- url %>% 
  html_nodes("a") %>% 
  html_attr("href")

link <- paste0("http://www.seinology.com/", link)

# Extract episode names
name <- url %>% 
  html_nodes("a") %>% 
  html_text()

# Create dataframe
df <- data.frame(name=name, 
                 link=link, 
                 stringsAsFactors = F)

# Eliminate irrelevant links
df <- df %>% 
  arrange(link) %>% 
  slice(c(81:103, 183:339))

# Eliminate repeated or irrelevant scripts. Those are:
# 83 - The Raincoats(2): repeated from ep. 82
# 100 - Highlights of a Hundred(1): clip montage
# 101 - Highlights of a Hundred(2): clip montage
# 177 - The Clip Show(1): clip montage
# 178 - The Clip Show(2): clip montage
# 180 - The Finale(2): repeated from ep. 179
df <- slice(df, -c(24, 25, 101, 102, 104, 164))

# Follow each link and extract the scripts
for(i in 1:nrow(df)) {
  script <- read_html(df$link[i]) %>% 
    html_nodes("font") %>% 
    html_text()
  script <- script[5] # script is fully contained in #5
  script <- repair_encoding(script) 
  fnames <- paste0("scripts/", df$name[i], ".txt") # file names
  sink(file = fnames) # start saving device
  cat(script) # save output
  sink()
  Sys.sleep(1) # wait 1 second until next request
}
