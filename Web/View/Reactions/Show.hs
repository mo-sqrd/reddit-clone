module Web.View.Reactions.Show where
import Web.View.Prelude

data ShowView = ShowView { reaction :: Reaction }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Reaction</h1>
        <p>{reaction}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Reactions" ReactionsAction
                            , breadcrumbText "Show Reaction"
                            ]