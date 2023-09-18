qjanno "SELECT foo,sum(bar),COUNT(*) cnt FROM data/big.csv WHERE bar IS NOT NULL GROUP BY foo ORDER BY cnt DESC LIMIT 3"
