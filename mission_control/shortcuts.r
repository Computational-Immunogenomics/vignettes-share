options(warn = -1)
shh <- suppressPackageStartupMessages

shh(library(tidyverse))
shh(library(data.table))
shh(library(dtplyr))

options(dplyr.summarise.inform=F)
options(repr.matrix.max.cols=150, repr.matrix.max.rows=200)

get_query_data <- function( query = "execute_sql_on_prod 'select * from hmfpatients.clinical'" ){
    system_call <- system(query, intern = TRUE)
    as.data.frame(fread(text = system_call))
}
query <- get_query_data
q <- query
corner <- function(mat, k = 5) mat[1:k,1:k]

df <- data.frame
gb <- group_by
ug <- ungroup
ar <- arrange
mu <- mutate
ij <- inner_join
lj <- left_join
rj <- right_join
se <- select
tm <- transmute
su <- summarise
rw <- rowwise
re <- reframe
fi <- filter
pu <- pull
sp <- spread
ma <- mutate_all
ren <- rename_with
uni <- unique
ga <- gather

my_theme <- 
theme_bw() + 
theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust = .5)# Remove minor gridlines
)

clean_filename <- function(x) {
#  x <- tolower(x)                              # convert to lowercase (optional)
  x <- gsub("[[:space:]]+", "_", x)            # replace spaces/tabs with underscores
  x <- gsub("[^a-zA-Z0-9_\\.-]", "", x)        # remove special characters except _, . and -
  x <- gsub("_+", "_", x)                      # collapse multiple underscores
  x <- gsub("^_|_$", "", x)                    # remove leading/trailing underscores
  return(x)
}
