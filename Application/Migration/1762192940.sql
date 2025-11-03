CREATE TABLE reactions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    post_id UUID NOT NULL,
    user_id UUID NOT NULL,
    kind TEXT NOT NULL
);
CREATE INDEX reactions_post_id_index ON reactions (post_id);
CREATE INDEX reactions_user_id_index ON reactions (user_id);
ALTER TABLE reactions ADD CONSTRAINT reactions_ref_post_id FOREIGN KEY (post_id) REFERENCES posts (id) ON DELETE CASCADE;
ALTER TABLE reactions ADD CONSTRAINT reactions_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
