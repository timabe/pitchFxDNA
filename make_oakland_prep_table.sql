-- These are the mysql queries I wrote to build my dataset
-- requires these tables
-- atbat
-- pitch
-- game
-- all tables are in a db called baseball

use baseball;

-- build table of all Oakland A's Games
drop table if exists oak_events;
create table oak_events as 

select date, away_code, away_team_city, home_code, home_team_city, b.* 
from 
     (select * from game where (home_team_city='Oakland' OR away_team_city='Oakland')  ) a 
     INNER JOIN 
     atbat b 
     on (a.url = b.url);

DROP TABLE IF EXISTS oak_pitches; 
CREATE TABLE oak_pitches as 

SELECT a.*, b.pitcher, b.pitcher_name, b.batter_name 
FROM pitch a 
JOIN oak_events b  
ON ( a.num = b.num AND a.url = b.url) ; 