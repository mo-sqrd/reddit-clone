module Web.View.Comments.Show where
import Web.View.Prelude

data ShowView = ShowView { comment :: Comment }

instance View ShowView where
    html ShowView { comment } = [hsx|
        {breadcrumb}
        <h1>Show Comment</h1>
        <p>{comment}</p>

        <form method="POST" action={DeleteCommentAction (get #id comment)}>
            <input type="hidden" name="_method" value="DELETE"/>
            <button type="submit">Delete Comment</button>
        </form>
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Comments" CommentsAction
                            , breadcrumbText "Show Comment"
                            ]