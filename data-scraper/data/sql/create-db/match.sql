CREATE TABLE IF NOT EXISTS match (
	match_id1 INTEGER,
	match_id2 INTEGER,
	map TEXT,
	start_time TEXT,
	wait_time REAL,
	duration REAL,
	score_team_a INTEGER,
	score_team_b INTEGER,

	PRIMARY KEY(match_id1, match_id2)
);
