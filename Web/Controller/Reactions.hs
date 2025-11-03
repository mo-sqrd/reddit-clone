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

    action NewReactionAction = do
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
                    redirectTo ReactionsAction

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
    let postId = get #postId reaction
    let userId = get #userId reaction
    let newKind = get #kind reaction

    existingMaybe <- query @Reaction
        |> filterWhere (#postId, postId)
        |> filterWhere (#userId, userId)
        |> fetchOneOrNothing

    case existingMaybe of
        Nothing -> do
            -- no existing reaction by this user on this post -> create one (count = 1)
            let reactionToCreate = reaction |> set #kindCount 1
            created <- createRecord reactionToCreate
            pure (Just created)

        Just existing -> do
            let existingKind  = get #kind existing
            let existingCount = get #kindCount existing

            if existingKind == newKind
                then
                    -- clicking same kind again -> decrement (or delete if it reaches 0)
                    if existingCount <= 1
                        then do
                            deleteRecord existing
                            pure Nothing
                        else do
                            updated <- existing
                                |> set #kindCount (existingCount - 1)
                                |> updateRecord
                            pure (Just updated)
                else do
                    -- different kind clicked -> switch kind and ensure count at least 1
                    updated <- existing
                        |> set #kind newKind
                        |> set #kindCount 1
                        |> updateRecord
                    pure (Just updated)

