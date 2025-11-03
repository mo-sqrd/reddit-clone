module Web.View.Posts.Show where
import Web.View.Prelude
import qualified Text.MMark as MMark

data ShowView = ShowView { post :: Include "comments" Post }

renderMarkdown text =
    case text |> MMark.parse "" of
        Left error -> "Something went wrong"
        Right markdown -> MMark.render markdown |> tshow |> preEscapedToHtml

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}

        <!-- <h1>Hello {currentUser.email}</h1> -->

        <h1>{post.title}</h1>
        <p>{post.createdAt |> timeAgo}</p>

        {renderPostActions post}
        <div>{post.body |> renderMarkdown}</div>

                <div>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="heart">❤️ </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="like">👍 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="unlike">👎 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="laugh">😂 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="love">😍 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="cry">😢 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="shock">😱 </span>
                    <span class="emoji me-2 emoji-button" data-postid={tshow post.id} data-kind="angry">😡 </span>
                </div>


        <a href={NewCommentAction post.id}>Add Comment</a>

        <div>{forEach post.comments renderComment}</div>





    
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Posts" PostsAction
                            , breadcrumbText "Show Post"
                            ]

renderComment comment = [hsx|
    <div class="mt-4 ms-4">
        <small class="text-muted">{comment.author} - {comment.createdAt |> timeAgo}</small>
        <div class="card mt-2">
            <div class="card-body p-2">
                <a class="text-dark text-decoration-none" href={ShowCommentAction comment.id}>
                    {comment.body}
                </a>
            </div>
        </div>
    </div>
|]






renderPostActions :: Include "comments" Post -> Html
renderPostActions post =
    if post.userId == currentUserId then [hsx|
        <div class="mb-3 d-flex gap-2">
            <a href={EditPostAction post.id} class="btn btn-sm btn-outline-secondary">Edit</a>
            <a href={DeletePostAction post.id} class="btn btn-sm btn-outline-danger js-delete">Delete</a>
        </div>
    |] else [hsx||]