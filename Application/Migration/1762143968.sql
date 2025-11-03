ALTER TABLE posts ADD COLUMN post_username TEXT DEFAULT null;
ALTER TABLE users ADD CONSTRAINT users_username_key UNIQUE(username);
ALTER TABLE posts ADD CONSTRAINT posts_ref_post_username FOREIGN KEY (post_username) REFERENCES users (id) ON DELETE NO ACTION;
