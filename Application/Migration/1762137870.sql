
ALTER TABLE users ADD COLUMN username TEXT NOT NULL;
ALTER TABLE users ADD CONSTRAINT users_username_key UNIQUE(username);
