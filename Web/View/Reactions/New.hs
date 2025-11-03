module Web.View.Reactions.New where
import Web.View.Prelude

data NewView = NewView { reaction :: Reaction }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Reaction</h1>
        {renderForm reaction}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Reactions" ReactionsAction
                , breadcrumbText "New Reaction"
                ]

renderForm :: Reaction -> Html
renderForm reaction = formFor reaction [hsx|
    {(textField #postId)}
    {(textField #userId)}
    {(textField #kind)}
    {submitButton}

|]