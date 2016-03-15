module Desc where


class Desc a where
    name :: a -> String
    descrip :: a -> String
