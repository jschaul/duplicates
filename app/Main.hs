-- RIO is suggesting all of these pragmas. I'm not convinced yet.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import RIO
import Prelude (print, putStrLn)
import Data.Text (Text)
import Control.Applicative
import Control.Monad
import Data.List.Unique

import qualified NLP.Tokenize.Text as T
import qualified NLP.Tokenize.String as S
import qualified Data.Text as Text

-- assumes \n linebreaks; use `dos2unix` to convert, if necessary
linesTok :: T.Tokenizer
linesTok xs = T.E [Right w | w <- Text.split (=='\n') xs ]

dotsTok :: T.Tokenizer
dotsTok = T.E . map Right
               . Text.groupBy (\a b -> ('.' == a) == ('.' == b))

main :: IO ()
main = do
    let f = "input.txt"
    contents <- readFileUtf8 f

    let splitOnDots = T.run (T.uris
                   >=> T.contractions
                   >=> T.negatives
                   >=> linesTok
                   >=> dotsTok
                  ) contents

    let splitOnPunctuation = T.run (T.uris
                   >=> T.contractions
                   >=> T.negatives
                   >=> linesTok
                   >=> T.allPunctuation
                  ) contents

    output "sentences" $ onlyInteresting (repeated splitOnDots)
    output "phrases" $ onlyInteresting (repeated splitOnPunctuation)
  where
    output :: Text -> [Text] -> IO ()
    output str xs = do
        putStrLn $ Text.unpack $ "\n\n\n---Repeated " <> str <> ":\n\n\n"
        forM_ xs (putStrLn . Text.unpack)


    onlyInteresting :: [Text] -> [Text]
    onlyInteresting = filter (\s -> not $ Text.isInfixOf "|" s)
                    . filter (\s -> not $ Text.isInfixOf "------------" s)
                    . filter (\s -> Text.length s > 20)


