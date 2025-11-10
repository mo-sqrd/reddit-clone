module Web.View.Posts.Show where
import Web.View.Prelude
import qualified Text.MMark as MMark

--- data ShowView = ShowView { post :: Include "comments" Post }
data ShowView = ShowView { post :: Include "reactions" (Include "comments" Post) }


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



        <div class="d-flex gap-3 align-items-center flex-wrap my-3">
            {renderReactionButton post "heart"  "❤️"}
            {renderReactionButton post "like"   "👍"}
            {renderReactionButton post "unlike" "👎"}
            {renderReactionButton post "laugh"  "😂"}
            {renderReactionButton post "love"   "😍"}
            {renderReactionButton post "cry"    "😢"}
            {renderReactionButton post "shock"  "😱"}
            {renderReactionButton post "angry"  "😡"}
        </div>


        <a href={NewCommentAction post.id}>Add Comment</a>

        <div>{forEach post.comments renderComment}</div>
    
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Posts" PostsAction
                            , breadcrumbText "Show Post"
                            ]


renderReactionButton :: Include "reactions" (Include "comments" Post) -> Text -> Text -> Html
renderReactionButton post kind emoji = [hsx|
    <form action={CreateReactionAction} method="POST" class="d-inline">
        <input type="hidden" name="postId" value={tshow post.id}/>
        <input type="hidden" name="kind" value={kind}/>
        <button type="submit" class="btn btn-link p-0 m-0 align-baseline emoji-button">
            {emoji} {renderCount (numReactions (get #reactions post) kind)}
        </button>
    </form>
|]

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




renderPostActions :: Include "reactions" (Include "comments" Post) -> Html
renderPostActions post =
    if post.userId == currentUserId then [hsx|
        <div class="mb-3 d-flex gap-2">
            <a href={EditPostAction post.id} class="btn btn-sm btn-outline-secondary">Edit</a>
            <a href={DeletePostAction post.id} class="btn btn-sm btn-outline-danger js-delete">Delete</a>
        </div>
    |] else [hsx||]

-- Count helpers (unchanged)
numReactions :: [Reaction] -> Text -> Int
numReactions reactions kind =
    length (filter (\r -> get #kind r == kind) reactions)

renderCount :: Int -> Html
renderCount n = if n > 0 then [hsx| {n} |] else mempty