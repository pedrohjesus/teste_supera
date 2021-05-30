# Carregeando os packages necessários:

library(tidyverse)
library(dplyr)
library(caret)
library(stringr)

# Lendo o arquivo original:

bechdel <- read_excel("R/03 - supera-data-test/bechdel.xlsx", col_types = c("text", "text", "skip", "skip", "skip", "skip", "numeric", 
"numeric", "numeric", "text", "skip", "skip"), skip = 1)


#Criando data frame para trabalho e mantendo o original

bechdel_raw <- bechdel

# Tranformação da variável "pass" para o tipo logical

bechdel %<>% 
  mutate(pass_binary = ifelse(bechdel$binary == "PASS", 1, 0)) %<>% 
  transform(pass_binary = as.logical(pass_binary))

# Testando o  número de filmes que passam no teste

porc_pass <- bechdel %>% 
  summarise(porc_pass = mean(pass_binary)*100)

# Testando o número e filmes que passa no teste por ano

porc_pass_by_year <- bechdel %>% 
  group_by(year) %>% 
  summarise(porc_pass = mean(pass_binary)*100,
            n = n())

# Criando novo data frame e retirando os n/a para fazer a análise de valores 

bechdel <- na.omit(bechdel) #com essa função, perderam-se 16 linhas, o que é aprox. 1% dos valores

# A análise comparando se o filme passa no teste bechdel com o custo, faturamento internacional e faturamento local (US) será feita em cima dos valores atualizados para a inflação de 2013

bechdel %<>%
  mutate(ROI = ((domgross_2013 + intgross_2013)/budget_2013)) %>% 
  mutate(ROI_dom = ((domgross_2013)/budget_2013)) %>% 
  mutate(ROI_int = ((intgross_2013)/budget_2013))

# Regeressão linear considerando ROI contra o teste Bechdel 

aval_1 <- lm(pass_binary ~ ROI, data = bechdel) #R² = 0.00199
summary(aval_1)

subset()
aval_2 <- lm(pass_binary ~ ROI^2, data = bechdel) #R² = 0.00199
summary(aval_2)

aval_3 <- lm(pass_binary ~ log(ROI), data = bechdel) #R² = 2.13e-05
summary(aval_3)

# Pelas regressões lineares acima, não há ligação entre o ROI o teste Bechdel.
# Como os dados não mostraram ligações entre o ROI e o resultado do teste Bechdel, foram incluidas na análise mais dados sobre os filmes, retirados do site IMDB

IMDb_movies <- read_csv("R/03 - supera-data-test/IMDb movies.csv", col_types = cols(title = col_skip(), original_title = col_skip(), 
year = col_skip(), date_published = col_skip(), duration = col_skip(), country = col_skip(), language = col_skip(), director = col_skip(), writer = col_skip(), 
production_company = col_skip(), actors = col_skip(), description = col_skip(), votes = col_skip(), budget = col_skip(), usa_gross_income = col_skip(), 
 worlwide_gross_income = col_skip(), metascore = col_skip(), reviews_from_users = col_skip(), reviews_from_critics = col_skip()))

bechdel_with_genres <- left_join(bechdel, IMDb_movies, by =c("imdb" = "imdb_title_id"))

bechdel_with_genres <- na.omit(bechdel_with_genres)

str_split_fixed(bechdel_with_genres$genre, ", ", 3)

df <- data.frame(str_split_fixed(bechdel_with_genres$genre, ", ", 3))

df <- rename(df, genre_1 = X1)
df <- rename(df, genre_2 = X2)
df <- rename(df, genre_3 = X3)

bechdel_with_genres <- bind_cols(bechdel_with_genres, df)

# Criação das colunas para as categorias cos filmes

bechdel_with_genres <- add_column(bechdel_with_genres, "Action" = 0, "Adventure" = 0, "Animation" = 0, "Biography" = 0, "Comedy" = 0, "Crime" = 0, "Drama" = 0, "Family" = 0, "Fantasy" = 0, "History" = 0, "Horror" = 0, "Music" = 0, "Musical" = 0, "Mystery" = 0, "Romance"= 0, "Sci-Fi" = 0, "Sport" = 0, "Thriller" = 0, "War" = 0, "Western"  = 0)

# Adicionando fator para os filmes de cada categoria

bechdel_with_genres$Action[bechdel_with_genres$genre_1=="Action"|bechdel_with_genres$genre_2=="Action"|bechdel_with_genres$genre_3=="Action"] <- bechdel_with_genres$Action + 1

bechdel_with_genres$Adventure[bechdel_with_genres$genre_1=="Adventure"|bechdel_with_genres$genre_2=="Adventure"|bechdel_with_genres$genre_3=="Adventure"] <- bechdel_with_genres$Adventure + 1

bechdel_with_genres$Animation[bechdel_with_genres$genre_1=="Animation"|bechdel_with_genres$genre_2=="Animation"|bechdel_with_genres$genre_3=="Animation"] <- bechdel_with_genres$Animation + 1

bechdel_with_genres$Biography[bechdel_with_genres$genre_1=="Biography"|bechdel_with_genres$genre_2=="Biography"|bechdel_with_genres$genre_3=="Biography"] <- bechdel_with_genres$Biography + 1

bechdel_with_genres$Comedy[bechdel_with_genres$genre_1=="Comedy"|bechdel_with_genres$genre_2=="Comedy"|bechdel_with_genres$genre_3=="Comedy"] <- bechdel_with_genres$Comedy + 1

bechdel_with_genres$Crime[bechdel_with_genres$genre_1=="Crime"|bechdel_with_genres$genre_2=="Crime"|bechdel_with_genres$genre_3=="Crime"] <- bechdel_with_genres$Crime + 1

bechdel_with_genres$Drama[bechdel_with_genres$genre_1=="Drama"|bechdel_with_genres$genre_2=="Drama"|bechdel_with_genres$genre_3=="Drama"] <- bechdel_with_genres$Drama + 1

bechdel_with_genres$Family[bechdel_with_genres$genre_1=="Family"|bechdel_with_genres$genre_2=="Family"|bechdel_with_genres$genre_3=="Family"] <- bechdel_with_genres$Family + 1

bechdel_with_genres$Fantasy[bechdel_with_genres$genre_1=="Fantasy"|bechdel_with_genres$genre_2=="Fantasy"|bechdel_with_genres$genre_3=="Fantasy"] <- bechdel_with_genres$Fantasy + 1

bechdel_with_genres$History[bechdel_with_genres$genre_1=="History"|bechdel_with_genres$genre_2=="History"|bechdel_with_genres$genre_3=="History"] <- bechdel_with_genres$History + 1

bechdel_with_genres$Horror[bechdel_with_genres$genre_1=="Horror"|bechdel_with_genres$genre_2=="Horror"|bechdel_with_genres$genre_3=="Horror"] <- bechdel_with_genres$Horror + 1

bechdel_with_genres$Music[bechdel_with_genres$genre_1=="Music"|bechdel_with_genres$genre_2=="Music"|bechdel_with_genres$genre_3=="Music"] <- bechdel_with_genres$Music + 1

bechdel_with_genres$Musical[bechdel_with_genres$genre_1=="Musical"|bechdel_with_genres$genre_2=="Musical"|bechdel_with_genres$genre_3=="Musical"] <- bechdel_with_genres$Musical + 1

bechdel_with_genres$Mystery[bechdel_with_genres$genre_1=="Mystery"|bechdel_with_genres$genre_2=="Mystery"|bechdel_with_genres$genre_3=="Mystery"] <- bechdel_with_genres$Mystery + 1

bechdel_with_genres$Romance[bechdel_with_genres$genre_1=="Romance"|bechdel_with_genres$genre_2=="Romance"|bechdel_with_genres$genre_3=="Romance"] <- bechdel_with_genres$Romance + 1

bechdel_with_genres$Sci-Fi[bechdel_with_genres$genre_1=="Sci-Fi"|bechdel_with_genres$genre_2=="Sci-Fi"|bechdel_with_genres$genre_3=="Sci-Fi"] <- bechdel_with_genres$Sci-Fi + 1

bechdel_with_genres$Sport[bechdel_with_genres$genre_1=="Sport"|bechdel_with_genres$genre_2=="Sport"|bechdel_with_genres$genre_3=="Sport"] <- bechdel_with_genres$Sport + 1

bechdel_with_genres$Thriller[bechdel_with_genres$genre_1=="Thriller"|bechdel_with_genres$genre_2=="Thriller"|bechdel_with_genres$genre_3=="Thriller"] <- bechdel_with_genres$Thriller + 1

bechdel_with_genres$War[bechdel_with_genres$genre_1=="War"|bechdel_with_genres$genre_2=="War"|bechdel_with_genres$genre_3=="War"] <- bechdel_with_genres$War + 1

bechdel_with_genres$Western[bechdel_with_genres$genre_1=="Western"|bechdel_with_genres$genre_2=="Western"|bechdel_with_genres$genre_3=="Western"] <- bechdel_with_genres$Western + 1

#Será criada o data frame com as informações necessárias para a regressão logistica e em seguida criada a regressão logística múltipla

bechdel_regression <- bechdel_with_genres[,-c(1,3:6,8:11,13:15)]

model_bechdel <- glm( pass_binary ~ avg_vote+Action+Adventure+Animation+Biography+Comedy+Crime+Drama+Family+Fantasy+Horror+History+Musical+Music+Mystery+Romance+SciFi+Thriller+War+Western , data = bechdel_regression, family = binomial(link="logit"))

# Teste do modelo usando o arquivo test.csv

test <- read_csv("R/03 - supera-data-test/test.csv", col_types = cols(budget = col_skip(), domgross = col_skip(), intgross = col_skip(),  budget_2013 = col_skip(), domgross_2013 = col_skip(), intgross_2013 = col_skip()))

test_raw <- test

test <- left_join(test, IMDb_movies, by =c("imdb" = "imdb_title_id"))

df_test <- data.frame(str_split_fixed(test$genre, ", ", 3))

df_test <- rename(df_test, genre_1 = X1)
df_test <- rename(df_test, genre_2 = X2)
df_test <- rename(df_test, genre_3 = X3)

test <- bind_cols(test, df_test)

test <- add_column(test, "Action" = 0, "Adventure" = 0, "Biography" = 0, "Comedy" = 0, "Crime" = 0, "Drama" = 0, "Family" = 0, "Fantasy" = 0, "History" = 0, "Horror" = 0, "Music" = 0, "Musical" = 0, "Mystery" = 0, "Romance"= 0, "SciFi" = 0, "Sport" = 0, "Thriller" = 0, "War" = 0, "Western"  = 0)
test <- add_column(test, "Animation" = 0)

test <- rename(test, "SciFi"="Sci-Fi")

test$Action[test$genre_1=="Action"|test$genre_2=="Action"|test$genre_3=="Action"] <- test$Action + 1

test$Adventure[test$genre_1=="Adventure"|test$genre_2=="Adventure"|test$genre_3=="Adventure"] <- test$Adventure + 1

test$Animation[test$genre_1=="Animation"|test$genre_2=="Animation"|test$genre_3=="Animation"] <- test$Animation + 1

test$Biography[test$genre_1=="Biography"|test$genre_2=="Biography"|test$genre_3=="Biography"] <- test$Biography + 1

test$Comedy[test$genre_1=="Comedy"|test$genre_2=="Comedy"|test$genre_3=="Comedy"] <- test$Comedy + 1

test$Crime[test$genre_1=="Crime"|test$genre_2=="Crime"|test$genre_3=="Crime"] <- test$Crime + 1

test$Drama[test$genre_1=="Drama"|test$genre_2=="Drama"|test$genre_3=="Drama"] <- test$Drama + 1

test$Family[test$genre_1=="Family"|test$genre_2=="Family"|test$genre_3=="Family"] <- test$Family + 1

test$Fantasy[test$genre_1=="Fantasy"|test$genre_2=="Fantasy"|test$genre_3=="Fantasy"] <- test$Fantasy + 1

test$History[test$genre_1=="History"|test$genre_2=="History"|test$genre_3=="History"] <- test$History + 1

test$Horror[test$genre_1=="Horror"|test$genre_2=="Horror"|test$genre_3=="Horror"] <- test$Horror + 1

test$Music[test$genre_1=="Music"|test$genre_2=="Music"|test$genre_3=="Music"] <- test$Music + 1

test$Musical[test$genre_1=="Musical"|test$genre_2=="Musical"|test$genre_3=="Musical"] <- test$Musical + 1

test$Mystery[test$genre_1=="Mystery"|test$genre_2=="Mystery"|test$genre_3=="Mystery"] <- test$Mystery + 1

test$Romance[test$genre_1=="Romance"|test$genre_2=="Romance"|test$genre_3=="Romance"] <- test$Romance + 1

test$SciFi[test$genre_1=="SciFi"|test$genre_2=="SciFi"|test$genre_3=="SciFi"] <- test$SciFi + 1

test$Sport[test$genre_1=="Sport"|test$genre_2=="Sport"|test$genre_3=="Sport"] <- test$Sport + 1

test$Thriller[test$genre_1=="Thriller"|test$genre_2=="Thriller"|test$genre_3=="Thriller"] <- test$Thriller + 1

test$War[test$genre_1=="War"|test$genre_2=="War"|test$genre_3=="War"] <- test$War + 1

test$Western[test$genre_1=="Western"|test$genre_2=="Western"|test$genre_3=="Western"] <- test$Western + 1

probabilities <- model_bechdel %>% predict(test, type = "response") #Respostas continuas com a probabilidade de cada um dos filmes passarem no teste Bechdel


predicted.classes <- ifelse(probabilities > 0.5, "1", "0") # Modificação dos resultados para considerar passando no teste os valores acima de 0.5 e nao passantes dos abaixo de 0.5 

write.csv(predicted.classes, file="R/03 - supera-data-test/mdb.csv")
