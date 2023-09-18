qjanno "SELECT foo,sum(bar),COUNT(*) cnt FROM data/csv/big.csv WHERE bar IS NOT NULL GROUP BY foo ORDER BY cnt DESC LIMIT 3"
