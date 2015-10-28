{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Applicative
import Data.Scientific
import Data.ByteString.Lazy.Char8 (pack)

data Exp
    = Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Val Double
    deriving (Eq, Show)

eval :: Exp -> Maybe Exp
eval (Add (Val v1) (Val v2)) = Just $ Val $ v1 + v2
eval (Sub (Val v1) (Val v2)) = Just $ Val $ v1 - v2
eval (Mul (Val v1) (Val v2)) = Just $ Val $ v1 * v2
eval (Div (Val v1) (Val 0)) = Nothing
eval (Div (Val v1) (Val v2)) = Just $ Val $ v1 / v2
eval (Val v) = Just $ Val v

eval (Add e1 e2) = do
    let r1 = eval e1
    let r2 = eval e2
    case r1 of
        Just v1 -> case r2 of
            Just v2 -> eval $ Add v1 v2
            _ -> Nothing
        _ -> Nothing

eval (Sub e1 e2) = do
    let r1 = eval e1
    let r2 = eval e2
    case r1 of
        Just v1 -> case r2 of
            Just v2 -> eval $ Sub v1 v2
            _ -> Nothing
        _ -> Nothing

eval (Mul e1 e2) = do
    let r1 = eval e1
    let r2 = eval e2
    case r1 of
        Just v1 -> case r2 of
            Just v2 -> eval $ Mul v1 v2
            _ -> Nothing
        _ -> Nothing

eval (Div e1 e2) = do
    let r1 = eval e1
    let r2 = eval e2
    case r1 of
        Just v1 -> case r2 of
            Just v2 -> eval $ Div v1 v2
            _ -> Nothing
        _ -> Nothing

instance ToJSON Exp where
    toJSON (Val v) = 
        object ["tag"      .= ("Val" :: String)
               , "payload" .= v ]
    toJSON (Add e1 e2) = 
        object ["tag"      .= ("Add" :: String)
               , "payload" .= (toJSON e1, toJSON e2) ]
    toJSON (Sub e1 e2) = 
        object ["tag"      .= ("Sub" :: String)
               , "payload" .= (toJSON e1, toJSON e2) ]
    toJSON (Mul e1 e2) = 
        object ["tag"      .= ("Mul" :: String)
               , "payload" .= (toJSON e1, toJSON e2) ]
    toJSON (Div e1 e2) = 
        object ["tag"      .= ("Div" :: String)
               , "payload" .= (toJSON e1, toJSON e2) ]

instance FromJSON Exp where
    parseJSON (Number n) = return $ Val $ toRealFloat n
    parseJSON (Object v) = case V.toList v of
        [(key, val)]
            | key == "Val" -> Val $ toRealFloat val
    --parseJSON (Array a) = Add <$> mapM parseJSON (toList a)
    parseJSON _ = empty
    --parseJSON (Number n) = return $ toRealFloat n
    --parseJSON (Object v) = return $ case (v .: "tag") of
        
ex1 :: Exp
ex1 = Div (Add (Val 1.0) (Val 2.0)) (Val 3.0)

ex2 :: Exp
ex2 = Div (Add (Val 1.0) (Val 2.0)) (Sub (Val 2.0) (Val 2.0))

ex3 :: String
ex3 = "{\"tag\": \"Add\", \"payload\": [{\"tag\": \"Val\", \"payload\": 1}, {\"tag\": \"Val\", \"payload\": 2}]}"

main :: IO()
main = do
    print $ eval ex1
    print $ eval ex2
    print $ encode ex2
