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


        <a href={NewCommentAction post.id}>Add Comment</a>

        <div>{forEach post.comments renderComment}</div>





    
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Posts" PostsAction
                            , breadcrumbText "Show Post"
                            ]

renderComment comment = [hsx|
        <div class="mt-4">
            <h5>{comment.author}</h5>
            <p>{comment.body}</p>
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