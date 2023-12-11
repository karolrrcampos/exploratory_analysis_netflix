################################################################################
# Fonte dataset: https://www.kaggle.com/datasets/shivamb/netflix-shows
################################################################################
# Carregar pacotes
library(countrycode)
library(tidyr)
library(lubridate)
library(ggplot2)
library(paletteer)
library(ggthemes)
library(treemapify)
library(RColorBrewer)
library(ggrepel)
library(dplyr)
library(plotly)
library(wordcloud)
library(tidytext)

# Carregar dataset
df <- read.csv("netflix_titles.csv",
               sep = ",",
               dec = ".")

names(df)

# Contagem observações unicas
unicos <- sapply(df, function(x) table(x))

# Arrumando observações
df$country <- gsub(", France, Algeria", "France, Algeria", df$country)
df$country <- gsub(", South Korea" , "South Korea" , df$country)

df$country[df$country == ""] <- "United States"

df$rating <- gsub("74 min", "", df$rating)
df$rating <- gsub("84 min", "", df$rating)
df$rating <- gsub("66 min", "", df$rating)

sum(df$rating == "")

# Como são apenas 7 observações que ficaram vazias na variável "rating", pesquisei os valores no IMDb pra completar as lacunas
df$rating[7313] <- "TV-G"
df$rating[c(5542, 5795, 5814)] <- "TV-MA"
df$rating[5990] <- "TV-PG"
df$rating[6828] <- "TV-14"
df$rating[7538] <- "PG-13"

sum(df$duration == "")

# Assim como foi feito em rating, pesquisei as durações no imdb para completar as lacunas
df$duration[5542] <- "74 min"
df$duration[5795] <- "82 min"
df$duration[5814] <- "66 min"

# Codificando os nomes dos países
netflix <- df

netflix$country <- netflix$country %>%
  strsplit(",") %>% 
  lapply(trimws) %>% 
  lapply(countrycode, origin = "country.name", destination = "iso3c", warn = FALSE) %>%
  sapply(function(x) paste(x, collapse = ","))

# Criar novas variáveis referente à data de adição
netflix$year_added <- year(mdy(netflix$date))
netflix$month_added <- month(mdy(netflix$date_added), abbr = FALSE, label = TRUE, locale = Sys.setlocale("LC_TIME", "C"))
# Reposicionado a nova coluna
netflix <- netflix %>%
  relocate(c(year_added, month_added), .before = which(names(netflix) == "release_year"))

# Separando a variável listed_in nos respectivos generos dos titulos
netflix <- separate(netflix, col = listed_in, into = c("primary_genre", "secondary_genre", "tertiary_genre"), sep = ",")

# Colocando cada país em uma linha separadamente
countries <- netflix
countries <- countries %>%
  separate_rows(country, sep = ",")

# Remover espaços extras
countries$country <- trimws(countries$country)

unique(countries$country)

# Novo df com as ofertas de Títulos da Netflix por País
count_country <- countries %>%
  group_by(country) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  head(20)

color <- paletteer_c("ggthemes::Classic Area Red", 30)
ggplot(data = count_country, aes(x = reorder(country, -count), y = count, fill = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Oferta de Títulos da Netflix por País",
       x = "Países",
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_gradientn(colors = color)

# Novo dataset com produções por diretor em números
count_director <- netflix %>%
  filter(director != "") %>%
  group_by(director) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  head(30)

color <- c("#000000", "#040000", "#090000", "#0d0000", "#120000", "#160000", "#1b0000", "#1f0000", "#240000", "#280000", "#2c0000", "#310000", "#350000", "#3a0000", "#3e0000", "#430000", "#470000", "#4c0000", "#500000", "#550000", "#590000", "#5d0000", "#620000", "#660000", "#6b0000", "#6f0000", "#740000", "#780000", "#7d0000", "#810000")
ggplot(count_director, aes(area = count, fill = director,
                           label = paste("Diretor:", director, "\nProduções:", count, sep = " "))) +
  geom_treemap() +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "topleft",
                    position = "identity",
                    size = 12) +
  theme(legend.position = "none") +
  scale_fill_manual(values = color)

# Novo df com produções lançados por ano
count_year_add <- netflix %>%
  filter(!is.na(year_added) & year_added != "") %>%
  group_by(year_added) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

ggplot(data = count_year_add, aes(x = reorder(year_added, count), y = count, fill = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Lançamentos da Netflix ao Longos dos Anos",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_gradientn(colors = color)

# gráfico de calor (heatmap) para visualizar a distribuição da atualização de conteúdos da Netflix ao longo dos meses e anos
# Definição da ordem dos meses
month_order <- c('December', 'November', 'October', 'September', 'August', 'July', 'June', 'May', 'April', 'March', 'February', 'January')

# Criação do gráfico de calor (heatmap)
color <- c("#ffc5c5", "#fbbebe", "#f6b7b7", "#f2b1b1", "#eeaaaa", "#e9a3a3", "#e59c9c", "#e19595", "#dc8f8f", "#d88888", "#d48181", "#cf7a7a", "#cb7373", "#c76d6d", "#c26666", "#be5f5f", "#b95858", "#b55252", "#b14b4b", "#ac4444", "#a83d3d", "#a43636", "#9f3030", "#9b2929", "#972222", "#921b1b", "#8e1414", "#8a0e0e", "#850707", "#810000")

netflix %>%
  filter(!is.na(month_added) & month_added != "") %>%
  group_by(year_added, month_added) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = factor(year_added), y = factor(month_added, levels = month_order), fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = color) +
  scale_x_discrete(limits = rev(levels(factor(netflix$year_added)))) +  
  labs(title = "Padrões de Variação Sazonal ao Longo dos Anos",
       x = NULL,
       y =  NULL,
       fill = "Legenda") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Novo df com a contagem dos países
genre_country <- countries %>%
  filter(country != "" & primary_genre != "") %>%
  group_by(country, primary_genre) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  head(30)

p <- ggplot(genre_country, aes(x = count, y = primary_genre, color = count)) +
  geom_point(stat = "identity", 
             aes(text = paste("Gênero:", primary_genre, 
                              "<br>", 
                              "País:", country, 
                              "<br>", 
                              "Produções:", count)), size = 3) +
  scale_color_gradient(low = "firebrick4", high = "lightpink") +
  labs(title = "Distribuição dos Gêneros das Produções por Países",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, max(genre_country$count), by = 50))
ggplotly(p, tooltip = "text")

################################################################################
netflix %>%
  group_by(type) %>%
  summarise(count = n())

# Separar o dataset em Movie e TV Show
movie <- netflix[netflix$type == "Movie", ]
# movie_countries <- countries[countries$type == "Movie", ]
tvshow <- netflix[netflix$type == "TV Show", ]

################################################################################

################################################################################
# Análise filmes
# Novo df com a contagem das classificações
rating_count <- movie %>%
  group_by(rating) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

ggplot(data = rating_count, aes(x = reorder(rating, count), y = count, fill = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Distribuição das Classificações dos Filmes",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_gradientn(colors = color)

# Novo df com a contagem dos generos
pri_genre <- movie %>%
  group_by(primary_genre) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

p <- ggplot(pri_genre, aes(x = count, y = primary_genre, color = count)) +
  geom_point(aes(text = paste("Gênero:", primary_genre, "<br>", "Produções:", count)), shape = 20, size = 3.5) +
  scale_color_gradient(low = "red4", high = "lightpink") +
  labs(title = "Distribuição dos Gêneros dos Filmes",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, max(pri_genre$count), by = 250))
ggplotly(p, tooltip = "text")

# Nuvem de palavras
sentiment <- movie[ ,13]
dados_unnested <- sentiment %>%
  count(primary_genre, sort = TRUE)
pal <- brewer.pal(8,"Dark2")
dados_unnested %>% with(wordcloud(primary_genre, n, rot.per=0, random.order = FALSE, max.words = 90, colors=pal))

################################################################################

################################################################################
# Análise Programas de TV
# Novo df com a contagem das classificações
rating_count <- tvshow %>%
  group_by(rating) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

ggplot(data = rating_count, aes(x = reorder(rating, count), y = count, fill = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Distribuição das Classificações dos Programas de TV",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_gradientn(colors = color)

# Novo df com a contagem dos generos
pri_genre <- tvshow %>%
  group_by(primary_genre) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

p <- ggplot(pri_genre, aes(x = count, y = primary_genre, color = count)) +
  geom_point(aes(text = paste("Gênero:", primary_genre, "<br>", "Produções:", count)), shape = 20, size = 3.5) +
  scale_color_gradient(low = "red4", high = "lightpink") +
  labs(title = "Distribuição dos Gêneros dos Programas de TV",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, max(pri_genre$count), by = 250))
ggplotly(p, tooltip = "text")

# Nuvem de palavras
sentiment <- tvshow[ ,13]
sentiment$primary_genre <- gsub("International TV Shows","International", sentiment$primary_genre)
sentiment$primary_genre <- gsub("TV Comedies","Comedies", sentiment$primary_genre)
sentiment$primary_genre <- gsub("TV Dramas","Dramas", sentiment$primary_genre)
dados_unnested <- sentiment %>%
  count(primary_genre, sort = TRUE)
dados_unnested %>% with(wordcloud(primary_genre, n, rot.per=0, random.order = FALSE, max.words = 90, colors=pal))