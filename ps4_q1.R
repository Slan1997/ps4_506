# ps4_1
# Packages
library(tidyverse)
library(dbplyr)
library(Lahman)
lahman = lahman_sqlite()
t1 =lahman %>% tbl(sql(
  '
SELECT Player, birthCountry AS `Country of Birth`, debut as Debut, Hits
FROM (
  SELECT m.playerID,m.nameFirst || " " || m.nameLast AS Player,
       birthCountry, debut, Hits
  FROM master m
  INNER JOIN (SELECT b.playerID, H as Hits
              FROM batting b
              GROUP BY b.playerID
              HAVING Hits == max(Hits)
  ) b ON m.playerID = b.playerID 
  GROUP BY m.playerID
  HAVING Hits >= 200
 )
GROUP BY `Country of Birth`
HAVING Hits == max(Hits)
ORDER BY -Hits
'
)) %>% collect()
