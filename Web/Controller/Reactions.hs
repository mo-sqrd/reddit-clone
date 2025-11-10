module Web.Controller.Reactions where

import Web.Controller.Prelude
import Web.View.Reactions.Index
import Web.View.Reactions.New
import Web.View.Reactions.Edit
import Web.View.Reactions.Show
import qualified Data.Map.Strict as Map


instance Controller ReactionsController where
    action ReactionsAction = do
        reactions <- query @Reaction |> fetch
        render IndexView { .. }

    action NewReactionAction  = do
        let reaction = newRecord

        render NewView { .. }

    action ShowReactionAction { reactionId } = do
        reaction <- fetch reactionId
        render ShowView { .. }

    action EditReactionAction { reactionId } = do
        reaction <- fetch reactionId
        render EditView { .. }

    action UpdateReactionAction { reactionId } = do
        reaction <- fetch reactionId
        reaction
            |> buildReaction
            |> ifValid \case
                Left reaction -> render EditView { .. }
                Right reaction -> do
                    reaction <- reaction |> updateRecord
                    setSuccessMessage "Reaction updated"
                    redirectTo EditReactionAction { .. }

    action CreateReactionAction = do
        let reaction = newRecord @Reaction
        reaction
            |> buildReaction
            |> ifValid \case
                Left reaction -> render NewView { .. } 
                Right reaction -> do
                    -- handle toggle semantics: one reaction per (post,user);
                    -- create if none, delete if same kind clicked again, or switch kind if different
                    reaction' <- toggleReaction reaction
                    case reaction' of
                        Just _ -> setSuccessMessage "Reaction updated"
                        Nothing -> setSuccessMessage "Reaction removed"
                    redirectTo ShowPostAction { postId = get #postId reaction }


    action DeleteReactionAction { reactionId } = do
        reaction <- fetch reactionId
        deleteRecord reaction
        setSuccessMessage "Reaction deleted"
        redirectTo ReactionsAction

buildReaction reaction = reaction
    |> fill @'["postId", "userId", "kind"]
    |> set #userId currentUserId
    |> validateField #kind nonEmpty


toggleReaction :: (?modelContext :: ModelContext) => Reaction -> IO (Maybe Reaction)
toggleReaction reaction = do
    let postId  = get #postId reaction
    let userId  = get #userId reaction
    let newKind = get #kind reaction

    existingMaybe <- query @Reaction
        |> filterWhere (#postId, postId)
        |> filterWhere (#userId, userId)
        |> fetchOneOrNothing

    case existingMaybe of
        -- No reaction yet -> create one
        Nothing -> do
            created <- createRecord reaction
            pure (Just created)

        Just existing -> do
            let existingKind = get #kind existing
            if existingKind == newKind
                then do
                    -- Same kind clicked -> toggle OFF (delete)
                    deleteRecord existing
                    pure Nothing
                else do
                    -- Different kind clicked -> switch kind
                    updated <- existing
                        |> set #kind newKind
                        |> updateRecord
                    pure (Just updated)

