cat basic.csv.gz | qjanno -H -z "SELECT foo,baz FROM - WHERE bar IS NOT NULL"
