module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute PostsController 

--- post "/posts/:postId/react" PostsController ReactPostAction

--- post "/posts/:postId/react" PostsController ReactPostAction



instance AutoRoute CommentsController
instance AutoRoute SessionsController



instance AutoRoute UsersController



