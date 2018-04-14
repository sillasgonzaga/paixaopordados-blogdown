# id (string): Video identifier
# title (string): Video title
# alt_title (string): A secondary title of the video
# creator (string): The creator of the video
# release_date (string): The date (YYYYMMDD) when the video was released
# timestamp (numeric): UNIX timestamp of the moment the video became available
# upload_date (string): Video upload date (YYYYMMDD)
# duration (numeric): Length of the video in seconds
# view_count (numeric): How many users have watched the video on the platform
# like_count (numeric): Number of positive ratings of the video
# dislike_count (numeric): Number of negative ratings of the video
# comment_count (numeric): Number of comments on the video
library(stringr)

fields_raw <- c("id", "title", "alt_title", "creator", "release_date", "timestamp",
            "upload_date", "duration", "view_count", "like_count", "dislike_count",
            "comment_count")

fields <- fields_raw %>% 
  map_chr(~paste0("%(", ., ")s")) %>% 
  paste0(collapse = "&&&") %>% 
  paste0('"', ., '"') %>% 
  cat()

channel_url <- "https://www.youtube.com/channel/UC8mDF5mWNGE-Kpfcvnn0bUg"

str_glue("youtube-dl -o {fields} -i -v -w --skip-download --write-auto-sub --sub-lang pt {channel_url}")
