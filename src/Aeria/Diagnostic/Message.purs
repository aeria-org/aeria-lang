module Aeria.Diagnostic.Message
  ( Diagnostic(..)
  , DiagnosticInfo(..)
  , ppDiagnostic
  )
  where

import Prelude

import Aeria.Diagnostic.Position (SourcePos(..), Span(..))
import Aeria.Semantic.Error (SemanticError)
import Aeria.Syntax.Error (SyntaxError(..))
import Data.Array (replicate, slice)
import Data.Array as A
import Data.String (Pattern(..), split)

data DiagnosticInfo
  = DiagnosticSyntaxError SyntaxError
  | DiagnosticSemanticError SemanticError

data Diagnostic = Diagnostic
  { filepath    :: String
  , span        :: Span
  , source      :: String
  , info        :: DiagnosticInfo
  }

getLines :: String -> Array String
getLines str = split (Pattern "\n") str

getSourceCode :: String -> Int -> Int -> Array String
getSourceCode source startLine endLine =
  let
    lines = getLines source
    startLine' = max (startLine - 4) 0
  in if startLine' >= 0 && endLine < A.length lines
      then slice startLine' endLine lines
      else []

ppDiagnostic :: Diagnostic -> String
ppDiagnostic (Diagnostic { filepath, span, source, info }) =
  let
    Span (SourcePos _ startLine _) (SourcePos _ endline _) = span

    message =
      case info of
        DiagnosticSyntaxError (SyntaxError m) -> m
        DiagnosticSemanticError se -> show se

    above = getSourceCode source startLine endline
  in
    A.intercalate "\n"
      [ ppPosition span filepath
      , ppSourceCode above
      , ppMessage span message
      ]

ppSourceCode :: Array String -> String
ppSourceCode lines =
  A.intercalate "\n" (map (\l -> "| " <> l) lines)

ppMessage :: Span -> String -> String
ppMessage span message =
  let Span (SourcePos startIndex _ startColumn) (SourcePos endIndex _ _) = span
  in
    A.intercalate ""
      [ "| "
      , A.intercalate "" $ replicate (startColumn - 1) " "
      , (A.intercalate "" $ replicate (max (endIndex - startIndex) 1) "^")
      , " " <> message
      , "\n|"
      ]

ppPosition :: Span -> String -> String
ppPosition span filepath =
  let Span (SourcePos startIndex startLine startColumn) (SourcePos endIndex endLine endColumn) = span
    in
      A.intercalate ""
        [ "--> "
        , filepath
        , ":"
        , show startLine
        , ":"
        , show startColumn
        , " - "
        , show endLine
        , ":"
        , show endColumn
        ," (line "
        , show startLine
        , ", column "
        , show startColumn
        , " - "
        ,"line "
        , show endLine
        ,", column "
        , show endColumn
        , ") "
        ]
