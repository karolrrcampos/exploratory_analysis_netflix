SELECT * FROM netflix.netflix_titles;

-- Verificar a ocorrência de células vazias na coluna title
SELECT COUNT(*) AS total_empty_cells
FROM netflix.netflix_titles
WHERE title IS NULL;

-- Número de Produções por tipo
SELECT type,
count(*) AS AmountType
from netflix.netflix_titles
group by type;

-- Valores distintos diretores
SELECT count(director) 
from netflix.netflix_titles;

-- CRIANDO NOVA TABELA
-- Algumas colunas contém dois nomes, irei separar para uma contagem precisa de diretores
-- A função abaixo permite separar os nomes e evita que linhas em branco ocupem o espaço dessa tabela
CREATE TABLE netflix.directors_table (
    Director VARCHAR(30)
);
INSERT INTO netflix.directors_table (Director)
SELECT TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 1), ',', -1)) as Director
FROM netflix.netflix_titles
WHERE TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 1), ',', -1)) != ''
UNION
SELECT TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 2), ',', -1)) as Director
FROM netflix.netflix_titles
WHERE TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 2), ',', -1)) != ''
UNION
SELECT TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 3), ',', -1)) as Director
FROM netflix.netflix_titles
WHERE TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 3), ',', -1)) != ''
UNION
SELECT TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 4), ',', -1)) as Director
FROM netflix.netflix_titles
WHERE TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 4), ',', -1)) != ''
UNION
SELECT TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 5), ',', -1)) as Director
FROM netflix.netflix_titles
WHERE TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(director, ',', 5), ',', -1)) != '';

-- Número de Produções por diretor
SELECT Director,
Count(*) AS AmountJob
FROM netflix.directors_table
Group by Director;

-- Separando os atores em linhas distintas
CREATE TABLE netflix.actors_table (
    Actor VARCHAR(30)
);
INSERT INTO netflix.actors_table (Actor)
SELECT TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(cast, ',', n.n), ',', -1)) AS Actor
FROM netflix.netflix_titles
JOIN (
    SELECT 1 AS n UNION ALL
    SELECT 2 UNION ALL
    SELECT 3 UNION ALL
    SELECT 4 UNION ALL
    SELECT 5 UNION ALL
    SELECT 6 UNION ALL
    SELECT 7 UNION ALL
    SELECT 8 UNION ALL
    SELECT 9 UNION ALL
    SELECT 10 UNION ALL
    SELECT 11 UNION ALL
    SELECT 12 UNION ALL
    SELECT 13 UNION ALL
    SELECT 14 UNION ALL
    SELECT 15 UNION ALL
    SELECT 16 
) n
ON CHAR_LENGTH(cast) - CHAR_LENGTH(REPLACE(cast, ',', '')) >= n.n - 1
WHERE TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(cast, ',', n.n), ',', -1)) != '';

-- Número de Produções por ator
SELECT Actor,
Count(*) AS AmountJob
FROM netflix.actors_table
Group by Actor;

-- Número de Produções lançados por ano
SELECT release_year,
Count(*) AS AmountRelease
FROM netflix.netflix_titles
Group by release_year;

-- Número de Produções por classificação
SELECT rating,
Count(*) AS AmountRating
FROM netflix.netflix_titles
Group by rating;

-- Valores distintos
SELECT count(distinct duration) 
FROM netflix.netflix_titles;

-- duração
SELECT TRIM(duration) AS duration,
COUNT(*) AS AmountDuration
FROM netflix.netflix_titles
GROUP BY duration;

-- Separando os gêneros em linhas distintas
CREATE TABLE netflix.listed_table (
    Listed VARCHAR(30) 
);
INSERT INTO netflix.listed_table (Listed)
SELECT TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(listed_in, ',', n.n), ',', -1)) as AmountListed
FROM netflix.netflix_titles
JOIN (
    SELECT 1 AS n UNION ALL
    SELECT 2 UNION ALL
    SELECT 3 UNION ALL
    SELECT 4 UNION ALL
    SELECT 5 UNION ALL
    SELECT 6 UNION ALL
    SELECT 7 UNION ALL
    SELECT 8
) n
ON CHAR_LENGTH(listed_in) - CHAR_LENGTH(REPLACE(listed_in, ',', '')) >= n.n - 1
WHERE TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(listed_in, ',', n.n), ',', -1)) != '';

-- Número de Produções genero
SELECT Listed,
Count(*) AS AmountListed
FROM netflix.listed_table
Group by Listed;

-- Generos lançados por país
-- Como muitos generos estava na mesma linha, separados apenas por virgula, dividi cada categoria em diferentes linhas.
-- A partir do código abaixo, foi criado uma nova tabela chamada pais_tipo
SELECT
  TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(country, ',', n.n), ',', -1)) as Country,
  TRIM(SUBSTRING_INDEX(SUBSTRING_INDEX(listed_in, ',', n.n), ',', -1)) as ListedIn,
  COUNT(*) AS AmountCountry
FROM netflix.netflix_titles
JOIN (
   SELECT 1 AS n UNION ALL
   SELECT 2 UNION ALL
   SELECT 3 UNION ALL
   SELECT 4 UNION ALL
   SELECT 5
) n
GROUP BY Country, ListedIn, n.n
ORDER BY AmountCountry DESC;
  
-- Quantidade de gêneros lançados por país
SELECT  
  SUM(AmountCountry) AS TotalAmount,
  Country,
  ListedIn
FROM netflix.pais_tipo
GROUP BY Country, ListedIn;
