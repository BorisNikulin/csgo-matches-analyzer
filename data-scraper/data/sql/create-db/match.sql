CREATE TABLE IF NOT EXISTS match (
	map TEXT,
	start_time TEXT,
	wait_time REAL,
	duration REAL,
	score_team_a INTEGER,
	score_team_b INTEGER,

	PRIMARY KEY(map, start_time, duration)
);
