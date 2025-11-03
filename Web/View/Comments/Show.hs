module Web.View.Comments.Show where
import Web.View.Prelude

data ShowView = ShowView { comment :: Comment }

instance View ShowView where
    html ShowView { comment } = [hsx|
        {breadcrumb}
        <h1>Show Comment</h1>

        <div class="mt-3">
            <small class="text-muted">{comment.author} - {comment.createdAt |> timeAgo}</small>

            <div class="mt-2 ps-3">
                <p class="mb-2">{comment.body}</p>

                {renderCommentActions comment}
            </div>
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Comments" CommentsAction
                            , breadcrumbText "Show Comment"
                            ]

renderCommentActions :: Comment -> Html
renderCommentActions comment =
    if comment.author == currentUser.username then [hsx|
        <div class="d-flex gap-2">
            <a href={EditCommentAction comment.id} class="btn btn-sm btn-outline-secondary">Edit</a>
            <a href={DeleteCommentAction comment.id} class="btn btn-sm btn-outline-danger js-delete">Delete</a>
        </div>
    |] else [hsx||]