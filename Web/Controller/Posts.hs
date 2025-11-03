module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Edit
import Web.View.Posts.Show
import qualified Text.MMark as MMark
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import qualified Data.Text as T




instance Controller PostsController where
    beforeAction = ensureIsUser

    action PostsAction = do
        posts <- query @Post
            |> orderByDesc #createdAt
            |> fetch
        render IndexView { .. }

    action NewPostAction = do
        let post = newRecord
        render NewView { .. }

    action ShowPostAction { postId } = do
        post <- fetch postId
            >>= pure . modify #comments (orderByDesc #createdAt)
            >>= fetchRelated #comments
        render ShowView { .. }

    action EditPostAction { postId } = do
        post <- fetch postId
        accessDeniedUnless (post.userId == currentUserId)
        render EditView { .. }

    action UpdatePostAction { postId } = do
        post <- fetch postId
        post
            |> buildPost
            |> ifValid \case
                Left post -> render EditView { .. }
                Right post -> do
                    post <- post |> updateRecord
                    setSuccessMessage "Post updated"
                    redirectTo EditPostAction { .. }

    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            |> ifValid \case
                Left post -> render NewView { .. } 
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction

    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post
        setSuccessMessage "Post deleted"
        redirectTo PostsAction

    -- action ReactPostAction { postId } = do
    -- -- read params (IO)
    -- kind     <- paramOrDefault @Text "" "kind"
    -- prevKind <- paramOrNothing @Text "prev"

    -- post <- fetch postId

    -- let adjustCount :: Post -> Text -> Int -> Post
    --     adjustCount p k delta = case k of
    --         "❤️" -> p |> set #heartCount (get #heartCount p + delta)
    --         "👍" -> p |> set #plusCount  (get #plusCount  p + delta)
    --         "👎" -> p |> set #minusCount (get #minusCount p + delta)
    --         "😂" -> p |> set #laughCount (get #laughCount p + delta)
    --         "😍" -> p |> set #loveCount  (get #loveCount  p + delta)
    --         "😢" -> p |> set #cryCount   (get #cryCount   p + delta)
    --         "😱" -> p |> set #shockCount (get #shockCount p + delta)
    --         "😡" -> p |> set #angryCount (get #angryCount p + delta)
    --         _    -> p

    -- -- undo previous reaction
    -- let post' = case prevKind of
    --         Just pk | not (T.null pk) -> adjustCount post pk (-1)
    --         _                         -> post

    -- -- apply new reaction if any
    -- let post'' = if T.null kind then post' else adjustCount post' kind 1

    -- postUpdated <- updateRecord post''

    -- renderJson $ object
    --     [ "heart" .= get #heartCount postUpdated
    --     , "plus"  .= get #plusCount  postUpdated
    --     , "minus" .= get #minusCount postUpdated
    --     , "laugh" .= get #laughCount postUpdated
    --     , "love"  .= get #loveCount  postUpdated
    --     , "cry"   .= get #cryCount   postUpdated
    --     , "shock" .= get #shockCount postUpdated
    --     , "angry" .= get #angryCount postUpdated
    --     ]



buildPost post = post
    |> fill @'["title", "body"]
    |> validateField #title nonEmpty
    |> validateField #body nonEmpty
    |> validateField #body isMarkdown

    |> set #userId currentUserId
    |> set #postUsername (get #username currentUser)

isMarkdown :: Text -> ValidatorResult
isMarkdown text =
    case MMark.parse "" text of
        Left _ -> Failure "Please provide valid Markdown"
        Right _ -> Success