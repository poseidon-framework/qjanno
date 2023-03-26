cat basic.csv | qjanno -H "SELECT foo,baz FROM - WHERE bar IS NOT NULL"
