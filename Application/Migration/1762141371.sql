ALTER TABLE comments ADD COLUMN user_id UUID NOT NULL;
ALTER TABLE posts ADD COLUMN user_id UUID NOT NULL;
CREATE INDEX posts_user_id_index ON posts (user_id);
CREATE INDEX comments_user_id_index ON comments (user_id);
ALTER TABLE comments ADD CONSTRAINT comments_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE posts ADD CONSTRAINT posts_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
