cat data/basic.csv | qjanno "SELECT foo,baz FROM - WHERE bar IS NOT NULL"
