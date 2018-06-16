CREATE TABLE IF NOT EXISTS played_in (
	pid INTEGER,
	map TEXT,
	start_time TEXT,
	duration REAL,
	team INTEGER,
	ping INTEGER,
	kills INTEGER,
	assists INTEGER,
	deaths INTEGER,
	mvps INTEGER,
	hsp INTEGER,
	score INTEGER,

	FOREIGN KEY (map, start_time, duration) REFERENCES match(map, start_time, duration),
	FOREIGN KEY (pid) REFERENCES player(steam_id),
	PRIMARY KEY (pid, map, start_time, duration)
);
