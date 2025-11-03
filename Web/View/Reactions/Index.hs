module Web.View.Reactions.Index where
import Web.View.Prelude

data IndexView = IndexView { reactions :: [Reaction] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewReactionAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Reaction</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach reactions renderReaction}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Reactions" ReactionsAction
                ]

renderReaction :: Reaction -> Html
renderReaction reaction = [hsx|
    <tr>
        <td>{reaction}</td>
        <td><a href={ShowReactionAction reaction.id}>Show</a></td>
        <td><a href={EditReactionAction reaction.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteReactionAction reaction.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]