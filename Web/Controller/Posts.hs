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
    --     -- "kind" comes from form / fetch body (application/x-www-form-urlencoded or JSON not required)
    --     let kind = "" 
    --     let prev  = ""

    --     post <- fetch postId

    --     -- decrement previous reaction if any
    --     let post1 = case prev of
    --             "❤️" -> post |> set #heartCount (get #heartCount post - 1)
    --             "👍" -> post |> set #plusCount  (get #plusCount  post - 1)
    --             "👎" -> post |> set #minusCount (get #minusCount post - 1)
    --             "😂" -> post |> set #laughCount (get #laughCount post - 1)
    --             "😍" -> post |> set #loveCount  (get #loveCount  post - 1)
    --             "😢" -> post |> set #cryCount   (get #cryCount   post - 1)
    --             "😱" -> post |> set #shockCount (get #shockCount post - 1)
    --             "😡" -> post |> set #angryCount (get #angryCount post - 1)
    --             _    -> post

    --     -- increment chosen reaction (if kind is empty this is a no-op)
    --     let post2 = case kind of
    --             "❤️" -> post1 |> set #heartCount (get #heartCount post1 + 1)
    --             "👍" -> post1 |> set #plusCount  (get #plusCount  post1 + 1)
    --             "👎" -> post1 |> set #minusCount (get #minusCount post1 + 1)
    --             "😂" -> post1 |> set #laughCount (get #laughCount post1 + 1)
    --             "😍" -> post1 |> set #loveCount  (get #loveCount  post1 + 1)
    --             "😢" -> post1 |> set #cryCount   (get #cryCount   post1 + 1)
    --             "😱" -> post1 |> set #shockCount (get #shockCount post1 + 1)
    --             "😡" -> post1 |> set #angryCount (get #angryCount post1 + 1)
    --             _    -> post1

    --     post' <- post2 |> updateRecord

    --     let counts = object
    --             [ "heart" .= get #heartCount post'
    --             , "plus"  .= get #plusCount post'
    --             , "minus" .= get #minusCount post'
    --             , "laugh" .= get #laughCount post'
    --             , "love"  .= get #loveCount post'
    --             , "cry"   .= get #cryCount post'
    --             , "shock" .= get #shockCount post'
    --             , "angry" .= get #angryCount post'
    --             ]
    --     renderJson counts

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