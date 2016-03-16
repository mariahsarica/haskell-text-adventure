module Desc where


class Desc a where
    name :: a -> String
    describe :: a -> String
