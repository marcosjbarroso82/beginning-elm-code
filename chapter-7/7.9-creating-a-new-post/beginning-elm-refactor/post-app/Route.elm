module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Post exposing (PostId)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Posts
    | Post PostId
    | NewPost


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Posts top
        , map Posts (s "posts")
        , map NewPost (s "posts" </> s "new")
        , map Post (s "posts" </> Post.idParser)
        ]


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Posts ->
            "/posts"

        NewPost ->
            "/posts/new"
        Post postId ->
            "/posts/" ++ Post.idToString postId

