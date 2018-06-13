CREATE TABLE IF NOT EXISTS played_in (
	mid1 INTEGER,
	mid2 INTEGER,
	pid INTEGER,
	team INTEGER,
	ping INTEGER,
	kills INTEGER,
	assists INTEGER,
	deaths INTEGER,
	mvps INTEGER,
	hsp INTEGER,
	score INTEGER,

	FOREIGN KEY (mid1, mid2) REFERENCES match(match_id1, match_id2),
	FOREIGN KEY (pid) REFERENCES player(steam_id),
	PRIMARY KEY (mid1, mid2, pid)
);
