CREATE TABLE IF NOT EXISTS match (
	match_id1 INTEGER,
	match_id2 INTEGER,
	map TEXT,
	start_time TEXT,
	wait_time REAL,
	duration REAL,
	score_team_a INTEGER,
	score_team_b INTEGER,

	PRIMARY_KEY(matchId1, matchId2)
);

CREATE TABLE IF NOT EXISTS player (
	steam_id INTEGER,

	PRIMARY_KEY(steam_id)
);

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

	FOREIGN KEY (mid1) REFERENCES match(match_id1),
	FOREIGN KEY (mid2) REFERENCES match(match_id2),
	FOREIGN KEY (pid) REFERENCES player(steam_id),
	PRIMARY KEY (mid1, mid2, pid)
);
