module Web.View.Reactions.Edit where
import Web.View.Prelude

data EditView = EditView { reaction :: Reaction }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Reaction</h1>
        {renderForm reaction}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Reactions" ReactionsAction
                , breadcrumbText "Edit Reaction"
                ]

renderForm :: Reaction -> Html
renderForm reaction = formFor reaction [hsx|
    {(textField #postId)}
    {(textField #userId)}
    {(textField #kind)}
    {submitButton}

|]