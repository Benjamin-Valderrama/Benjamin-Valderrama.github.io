library(tidyverse)
library(rentrez)
library(xml2)
library(countrycode)
library(rnaturalearth)

# Search PubMed
search_term <- '((Valderrama B[Author] OR Valderrama BP[Author]) AND (microbiome OR microbiota) )'

pm_search <- entrez_search(
        db = "pubmed",
        term = search_term,
        retmax = 10000
)

# Get the pubs from author
pm_fetch_raw <- entrez_fetch(
        db = "pubmed",
        id = pm_search$ids,
        rettype = "xml",
        parsed = FALSE
)

pm_fetch <- xml2::read_xml(pm_fetch_raw)

# Manually check if authors is correctly assigned to each publication
# xml2::xml_find_all(pm_fetch, ".//ArticleTitle") |> 
#         xml2::xml_text()


# Extract affiliations from pubs
affiliations <- xml2::xml_find_all(pm_fetch,
                                   ".//AffiliationInfo/Affiliation") %>%
        xml2::xml_text(trim = TRUE) %>%
        tibble(affiliation = .) %>%
        distinct()

# Get countries of coauthors from affiliation information
data("codelist", package = "countrycode")
country_names <- unique(codelist$country.name.en)

extract_countries <- function(text) {
        
        # Common special cases: UK and USA
        if (grepl(x = text, pattern = ".*UK[.]$|.*UK$")) {
                country <- "United Kingdom"
                
        } else if (grepl(x = text, pattern = ".*USA[.]$|.*USA$|.*US[.]$|.*US$")) {
                country <- "United States"
                
        # Any other case
        } else {
                
                country <- str_extract(string = text, 
                            pattern = paste(country_names, collapse = "|"))
        }
        
        return(country)
}

countries_raw <- affiliations %>%
        mutate(country = map(affiliation, extract_countries)) %>%
        unnest(country)

countries_clean <- countries_raw %>%
        mutate(country_std = countrycode(
                        country,
                        origin = "country.name.en",
                        destination = "country.name")
               ) %>%
        filter(!is.na(country_std)) %>%
        distinct(country_std)

# Prepare world map data for plotting
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
        # fix the US missmatch
        mutate(name = ifelse(name == "United States of America", 
                             yes = "United States", 
                             no = name))

world_map <- world %>%
        filter(continent != "Antarctica") %>% 
        mutate(has_coauthor = name %in% countries_clean$country_std)

# Make plot
collab_map <- ggplot(world_map) +
        geom_sf(aes(fill = has_coauthor), color = "white", size = 0.05) +
        
        # fix white space 
        coord_sf(crs = "+proj=robin", expand = FALSE) +
        
        # custom color for countries if co-authors are found or not
        scale_fill_manual(values = c("TRUE" = "#C9261B", "FALSE" = "grey75"),
                          breaks = c("TRUE", "FALSE"),
                          labels = c("Co-authored", "Not co-authored (yet)"), 
                          name = "") +
        # labs
        labs(#title = "My Global Co-authorship Map",
             caption = "Affiliation data for co-authors were obtained from PubMed") +
        
        # remove white color from legend
        guides(color = guide_legend(override.aes = TRUE)) +
        
        theme_void() +
        theme(# legend position
              legend.position = "inside",
              legend.position.inside = c(0.17, 0.3),
              
              # text size
              #plot.title = element_text(size = 12, hjust = 0.5),
              plot.caption = element_text(size = 10, hjust = 0.5),
              legend.text = element_text(size = 10)
              ); collab_map


ggsave(file = "images/world_map.png", plot = collab_map, 
       width = 8, height = 6, 
       units = "in")
