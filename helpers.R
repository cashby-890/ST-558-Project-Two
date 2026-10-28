#Information about variables for viral videos data set

catvars <- c(
  "Category" = "category",
  "Day Uploaded" = "upload_day",
  "Hour Uploaded" = "upload_hour",
  "Viral?" = "viral",
  "Thumbnail Included?" = "thumbnail_face"
)

CategoryVals <- c("Beauty","Comedy","Cooking","DIY","Education",
                  "Finance","Fitness","Gaming","Music","News",
                  "Science","Sports","Tech","Travel","Vlog")

UploadDayVals <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

UploadHourVals <- c("Overnight","Morning","Afternoon","Evening")

ViralVals <- c("Yes","No")

ThumbnailVals <- c("Yes","No")

numvars <- c(
  "Duration (in seconds)" = "duration_sec",
  "Title Length" = "title_length",
  "Number of Tags" = "tags_count",
  "Description Length" = "description_length",
  "Number of Subscribers" = "subscriber_count",
  "Views" = "views",
  "Likes" = "likes",
  "Comments" = "comments",
  "Shares" = "shares"
)

app_description <- "This app allows a user to perform simple numerical and graphical analysis on a synthetic data set pertaining to a sample of 12,000 hypothetical YouTube videos. Since getting actual data for existing YouTube videos can be a taxing effort, a synthetic data set was used "
#link for more information about data
"https://www.kaggle.com/datasets/vedauv/what-makes-a-yt-video-go-viral-12000-vids/data"