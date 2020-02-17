module UserSystem.API (UserSystemAPI, AccountAPI, ObjectAPI, Ctx, AuthenticatedAccountAPI) where

import Servant 
  ( (:>)
  , (:<|>)
  , Raw
  , ReqBody
  , JSON
  , FormUrlEncoded
  , Post
  , AuthProtect )
import Servant.Server.Experimental.Auth 
  ( AuthHandler
  , AuthServerData )
import UserSystem.API.Types 
  ( WithCookieHeaders
  , Response
  , SignUp
  , SignIn
  , ChangePassword
  , ChangeUsername
  , CreateObject
  , EditObject
  , ReadObject
  , GiveUserRole )
import UserSystem.Ontology
  ( User )
import qualified Network.Wai as Wai

type UserSystemAPI
     = "account" :> AccountAPI 
  :<|> "object" :> ObjectAPI
  :<|> Raw

type SimpleReqRes req res = ReqBody '[JSON, FormUrlEncoded] req :> Post '[JSON] res

type SimpleResWithHeaders res = Post '[JSON] (WithCookieHeaders res)

type SimpleReq req = SimpleReqRes req (Response req)

type AccountAPI
     = "signup" :> SimpleReq SignUp
  :<|> "signin" :> SimpleReqRes SignIn (WithCookieHeaders (Response SignIn)) 
  :<|> AuthenticatedAccountAPI
  
type AuthenticatedAccountAPI
    = AuthProtect "user" :> 
    ( "new-token" :> SimpleResWithHeaders (Response SignIn)
 :<|> "change-password" :> SimpleReq ChangePassword
 :<|> "change-username" :> SimpleReq ChangeUsername
    )

type ObjectAPI
    = AuthProtect "user" :> 
    ( "create-object" :> SimpleReq CreateObject
 :<|> "edit-object" :> SimpleReq EditObject
 :<|> "read-object" :> SimpleReq ReadObject
 :<|> "give-user-role" :> SimpleReq GiveUserRole
    )

type Ctx = AuthHandler Wai.Request User ': '[]

type instance AuthServerData (AuthProtect "user") = User
