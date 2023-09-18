cat data/csv/basic.csv | qjanno "SELECT foo,baz FROM - WHERE bar IS NOT NULL"
