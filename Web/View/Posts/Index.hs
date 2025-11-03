module Web.View.Posts.Index where
import Web.View.Prelude

data IndexView = IndexView { posts :: [Post] }
-- data IndexView = IndexView { posts :: [Post] , reactionsByPost :: Map (Id Post) (Map Text Int) }


instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h2>Hello {currentUser.username}!</h2> 
        <a class="js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>

        <h1>Post Board<a href={pathTo NewPostAction} class="btn btn-primary ms-4">+ New</a></h1>


        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th> Recent Posts</th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach posts renderPost}</tbody>
            </table>
            
        </div>

    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Posts" PostsAction
                ]

        

renderPost :: Post -> Html
renderPost post = 
    if post.userId == currentUserId then [hsx|
    <tr>
        <!-- main column -->
        <td class="align-top">
            <div>
                <small class="text-muted">User {post.postUsername}</small>
            </div>
            <div class="mt-1">
                <h3 class="mb-1">
                    <a class="text-dark text-decoration-none" href={ShowPostAction post.id}>
                        {post.title}
                    </a>
                </h3>
                <p class="text-dark mb-3">{post.body}</p>
            </div>

            <!-- Reaction UI: show emojis inline (no React button) -->
            <div id={"reactions-" <> tshow post.id} class="mt-2">
                <div>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="❤️">❤️ </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="👍">👍 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="👎">👎 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😂">😂 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😍">😍 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😢">😢 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😱">😱 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😡">😡 </span>
                </div>

            </div>
    
        </td>

        <!-- right-side actions column -->
        <td class="align-top text-end" style="width:10%;">
                <div class="d-flex flex-column align-items-end">
                    <p class="text-muted text-decoration-none">{post.createdAt |> timeAgo}</p>

                    <a href={EditPostAction post.id} class="btn btn-sm btn-outline-secondary mb-2">Edit</a>
                    <a href={DeletePostAction post.id} class="btn btn-sm btn-outline-danger js-delete">Delete</a>
                </div>
        </td>

        
    </tr>
|] else [hsx|
    <tr>
        <!-- main column -->
        <td class="align-top">
            <div>
                <small class="text-muted">User {post.postUsername}</small>
            </div>
            <div class="mt-1">
                <h3 class="mb-1">
                    <a class="text-dark text-decoration-none" href={ShowPostAction post.id}>
                        {post.title}
                    </a>
                </h3>
                <p class="text-dark mb-3">{post.body}</p>
            </div>

            <!-- Reaction UI: show emojis inline (no React button) -->
            <div id={"reactions-" <> tshow post.id} class="mt-2">
                <div>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="❤️">❤️ </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="👍">👍 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="👎">👎 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😂">😂 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😍">😍 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😢">😢 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😱">😱 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="😡">😡 </span>
                </div>

            </div>
        </td>

                <!-- right-side actions column -->
        <td class="align-top text-end" style="width:20%;">
                <div class="d-flex flex-column align-items-end">
                    <p class="text-muted text-decoration-none">{post.createdAt |> timeAgo}</p>
                </div>
        </td>

    </tr>
|]


