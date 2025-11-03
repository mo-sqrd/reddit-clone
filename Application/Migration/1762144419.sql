ALTER TABLE posts ADD COLUMN post_username TEXT DEFAULT '' NOT NULL;
ALTER TABLE posts ADD CONSTRAINT posts_ref_post_username FOREIGN KEY (post_username) REFERENCES users (id) ON DELETE NO ACTION;
