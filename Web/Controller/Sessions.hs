module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.New
import qualified IHP.AuthSupport.Controller.Sessions as Sessions

instance Controller SessionsController where
    action NewSessionAction = Sessions.newSessionAction @User
    action CreateSessionAction = Sessions.createSessionAction @User
    action DeleteSessionAction = Sessions.deleteSessionAction @User


instance Sessions.SessionsControllerConfig User 

    -- action MyAction = do
    --     case currentUserOrNothing of
    --         Just currentUser -> do
    --             let text = "Hello " <> currentUser.email
    --             renderPlain text
    --         Nothing -> renderPlain "Please login first"


-- instance Sessions.SessionsControllerConfig User where
    -- beforeLogin = updateLoginHistory

    -- updateLoginHistory user = do
    --     user
    --         |> modify #logins (\count -> count + 1)
    --         |> updateRecord
    --     pure ()