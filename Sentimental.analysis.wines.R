
# Sentimental analysis of wine ratings file available from 
# https://www.kaggle.com/zynicide/wine-reviews and included in the
# tidytuesday by @R4DScommunity


#Load the required packages
suppressMessages(library(tidyverse))
library(tidytext)
library(reshape2)

#Load the data
wine <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

#Used to capitalize the first letter of each labeller in the facet_wrap
# From https://ggplot2.tidyverse.org/reference/labeller.html
capitalize <- function(string) {
    substr(string, 1, 1) <- toupper(substr(string, 1, 1))
    string
}

#Remove duplicated entries
wine <- wine %>% dplyr::distinct(description, .keep_all = TRUE)

#Get the words used in the description column
description <- wine %>% 
    unnest_tokens(word, description)

#Data with unnecessary words such as "the", "of", "in", ...
data("stop_words")

# Remove those "stop words"
description <- description %>% 
    anti_join(stop_words)

# Get the sentiments based on NRC from Saif Mohammad and Peter Turney
# available from the package tidytext.
word.counts <- description %>% 
    group_by(taster_name) %>% 
    inner_join(get_sentiments("nrc")) %>% 
    count(word, sentiment, sort = T) %>% 
    ungroup()


# Prepare for high resolution figure
tiff(filename = "Sentimental.analysis.wines.tif",
     width = 9.80, height = 5.80, units = "in", res = 600)

# Tidy the data and plot it
word.counts %>% 
    group_by(sentiment) %>% 
    top_n(10) %>% 
    ungroup() %>% 
    mutate(word = reorder(word, n)) %>% 
    filter(sentiment != "fear") %>% 
    ggplot(aes(word, n, fill = sentiment))+
    geom_col(show.legend = F)+
    facet_wrap(~sentiment, scales = "free_y", labeller = labeller(sentiment = capitalize, size = 15))+
    theme(strip.text.x = element_text(size = 20, face = "bold"))+
    labs(y = "Contribution to sentiment", x = NULL)+
    coord_flip()+
    scale_fill_viridis_d(aes(fill = sentiment))+
    theme_bw()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 15))+
    labs(title = "How do tasters feel when they describe a wine?",
         subtitle = "Tasters use words related to positive and negative sentiment.",
         caption = "Source = Kaggle | @JCMassante", size = 15)


# Close the plot window
dev.off()
