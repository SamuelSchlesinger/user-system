module Freeze.API (FreezeAPI, AccountAPI, FreezerAPI, Ctx, AuthenticatedAccountAPI) where

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
import Freeze.API.Types 
  ( WithCookieHeaders
  , Response
  , SignUp
  , SignIn
  , ChangePassword
  , ChangeUsername
  , CreateFreezer
  , CreateProject
  , CreateBox
  , SlotProbe
  , CreateSample
  , CreateSampleType
  , CreateMouse
  , CreateMouseProperty
  , GetFreezers )
import Freeze.Ontology
  ( User )
import qualified Network.Wai as Wai

type FreezeAPI
     = "account" :> AccountAPI 
  :<|> "freezer" :> AuthProtect "user" :> FreezerAPI
  :<|> Raw

type SimpleReqRes req res = ReqBody '[JSON, FormUrlEncoded] req :> Post '[JSON] res

type SimpleResWithHeaders res = Post '[JSON] (WithCookieHeaders res)

type SimpleReq req = SimpleReqRes req (Response req)

type SimpleReqNoForm req = ReqBody '[JSON] req :> Post '[JSON] (Response req)

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

type FreezerAPI
    =  "create-freezer"             :> SimpleReq CreateFreezer
  :<|> "create-project"             :> SimpleReq CreateProject
  :<|> "create-box"                 :> SimpleReq CreateBox
  :<|> "slot-probe"                 :> SimpleReq SlotProbe
  :<|> "create-mouse"               :> SimpleReq CreateMouse
  :<|> "create-sample"              :> SimpleReq CreateSample
  :<|> "create-sample-type"         :> SimpleReq CreateSampleType
  :<|> "create-mouse-property"      :> SimpleReq CreateMouseProperty
  :<|> "get-freezers"               :> SimpleReqNoForm GetFreezers

type Ctx = AuthHandler Wai.Request User ': '[]

type instance AuthServerData (AuthProtect "user") = User
