{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main
  )
where

import qualified Burrito
import qualified Burrito.Internal.Parse as Parse
import qualified Burrito.Internal.Render as Render
import qualified Burrito.Internal.Type.Case as Case
import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.Digit as Digit
import qualified Burrito.Internal.Type.Expression as Expression
import qualified Burrito.Internal.Type.Field as Field
import qualified Burrito.Internal.Type.Literal as Literal
import qualified Burrito.Internal.Type.MaxLength as MaxLength
import qualified Burrito.Internal.Type.Modifier as Modifier
import qualified Burrito.Internal.Type.Name as Name
import qualified Burrito.Internal.Type.Operator as Operator
import qualified Burrito.Internal.Type.Template as Template
import qualified Burrito.Internal.Type.Token as Token
import qualified Burrito.Internal.Type.Value as Value
import qualified Burrito.Internal.Type.Variable as Variable
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Test.Hspec as Hspec
import qualified Test.Hspec.QuickCheck as Hspec
import qualified Test.QuickCheck as QC

main :: IO ()
main = Hspec.hspec . Hspec.describe "Burrito" $ do

  Monad.forM_ tests $ \test ->
    Hspec.it (show (testInput test, unwrapOutput $ testOutput test))
      $ runTest test

  Hspec.describe "match" $ do
    let
      matchTest
        :: Stack.HasCallStack
        => String
        -> [(String, Burrito.Value)]
        -> String
        -> Hspec.Spec
      matchTest input values output = Hspec.it (show (input, output)) $ do
        template <- maybe (fail "invalid Template") pure $ Burrito.parse input
        let matches = Burrito.match output template
        matches `Hspec.shouldSatisfy` elem values
        Monad.forM_ matches $ \match -> do
          let it = Burrito.expand match template
          Monad.when (it /= output) . fail $ show
            (input, values, output, matches, match, it)

    matchTest "" [] ""
    matchTest "a" [] "a"
    matchTest "!" [] "!"
    matchTest "\xa0" [] "%C2%A0"

    matchTest "{a}" ["a" =: s ""] ""
    matchTest "{a}" ["a" =: s "A"] "A"
    matchTest "{a}" ["a" =: s "AB"] "AB"

    matchTest "a{b}" ["b" =: s ""] "a"
    matchTest "a{b}" ["b" =: s "B"] "aB"
    matchTest "a{b}" ["b" =: s "BC"] "aBC"

    matchTest "{a}b" ["a" =: s ""] "b"
    matchTest "{a}b" ["a" =: s "A"] "Ab"
    matchTest "{a}b" ["a" =: s "AB"] "ABb"

    matchTest "{a}{a}" ["a" =: s "A"] "AA"
    matchTest "{a}{a}" ["a" =: s "AB"] "ABAB"

    matchTest "{a}" ["a" =: s "%"] "%25"
    matchTest "{a}" ["a" =: s "/"] "%2F"
    matchTest "{a}" ["a" =: s "\xa0"] "%C2%A0"
    matchTest "{a}" ["a" =: s "\xd7ff"] "%ED%9F%BF"
    matchTest "{a}" ["a" =: s "\x10000"] "%F0%90%80%80"
    matchTest "{a}" ["a" =: s "A/B"] "A%2FB"
    matchTest "{a}" ["a" =: s "WX\xa0\xa1YZ"] "WX%C2%A0%C2%A1YZ"

    matchTest "{+a}" ["a" =: s "%"] "%25"
    matchTest "{+a}" ["a" =: s "/"] "/"

    matchTest "{#a}" [] ""
    matchTest "{#a}" ["a" =: s ""] "#"
    matchTest "{#a}" ["a" =: s "A"] "#A"
    matchTest "{#a}" ["a" =: s "%"] "#%25"
    matchTest "{#a}" ["a" =: s "/"] "#/"

    matchTest "{.a}" [] ""
    matchTest "{.a}" ["a" =: s ""] "."
    matchTest "{.a}" ["a" =: s "A"] ".A"
    matchTest "{.a}" ["a" =: s "%"] ".%25"
    matchTest "{.a}" ["a" =: s "/"] ".%2F"

    matchTest "{/a}" [] ""
    matchTest "{/a}" ["a" =: s ""] "/"
    matchTest "{/a}" ["a" =: s "A"] "/A"
    matchTest "{/a}" ["a" =: s "%"] "/%25"
    matchTest "{/a}" ["a" =: s "/"] "/%2F"

    matchTest "{;a}" [] ""
    matchTest "{;a}" ["a" =: s ""] ";a"
    matchTest "{;a}" ["a" =: s "A"] ";a=A"
    matchTest "{;a}" ["a" =: s "%"] ";a=%25"
    matchTest "{;a}" ["a" =: s "/"] ";a=%2F"

    matchTest "{?a}" [] ""
    matchTest "{?a}" ["a" =: s ""] "?a="
    matchTest "{?a}" ["a" =: s "A"] "?a=A"
    matchTest "{?a}" ["a" =: s "%"] "?a=%25"
    matchTest "{?a}" ["a" =: s "/"] "?a=%2F"

    matchTest "{&a}" [] ""
    matchTest "{&a}" ["a" =: s ""] "&a="
    matchTest "{&a}" ["a" =: s "A"] "&a=A"
    matchTest "{&a}" ["a" =: s "%"] "&a=%25"
    matchTest "{&a}" ["a" =: s "/"] "&a=%2F"

    matchTest "{a,b}" ["a" =: s "A", "b" =: s "B"] "A,B"
    matchTest "{+a,b}" ["a" =: s "A", "b" =: s "B"] "A,B"
    matchTest "{#a,b}" ["a" =: s "A", "b" =: s "B"] "#A,B"
    matchTest "{.a,b}" ["a" =: s "A", "b" =: s "B"] ".A.B"
    matchTest "{/a,b}" ["a" =: s "A", "b" =: s "B"] "/A/B"
    matchTest "{;a,b}" ["a" =: s "A", "b" =: s "B"] ";a=A;b=B"
    matchTest "{?a,b}" ["a" =: s "A", "b" =: s "B"] "?a=A&b=B"
    matchTest "{&a,b}" ["a" =: s "A", "b" =: s "B"] "&a=A&b=B"

    matchTest "{a,b}" ["a" =: s "A"] "A"
    matchTest "{a,b,c}" ["a" =: s "A"] "A"
    matchTest "{a,b,c}" ["a" =: s "A", "b" =: s "B"] "A,B"
    matchTest "{a}{a,b}" ["b" =: s "B"] "B"
    matchTest "{a}{a,b,c}" ["b" =: s "B"] "B"
    matchTest "{a,b}{a,b,c}" ["c" =: s "C"] "C"
    matchTest "{b}{a,b,c}" ["a" =: s "A", "c" =: s "C"] "A,C"
    matchTest "{a}{a,b,c}" ["b" =: s "B", "c" =: s "C"] "B,C"

    matchTest "{a:1}/{a}" ["a" =: s "AB"] "A/AB"

    -- TODO: Test matching on explode modifier.
    -- matchTest "{a*}" ["a" =: s "A"] "A"

    -- TODO: Test matching on lists.
    -- matchTest "{a}" ["a" =: l ["A", "B"]] "A,B"

    -- TODO: Test matching on dictionaries.
    -- matchTest "{a}" ["a" =: d ["k" =: "v"]] "k,v"

  Hspec.describe "uriTemplate"
    . Hspec.it "works as an expression"
    $ Just [Burrito.uriTemplate|a{b}c|]
    `Hspec.shouldBe` Burrito.parse "a{b}c"

  Hspec.modifyMaxSize (const 10)
    . Hspec.it "round trips"
    . QC.property
    $ \(Template template) ->
        Burrito.parse (Burrito.render template) == Just template

-- brittany-next-binding --columns 160
tests :: [Test]
tests = mconcat
  [ [ Test "" [] ""
    , Test "" ["a" =: s ""] ""
    , Test "" ["a" =: l []] ""
    , Test "" ["a" =: d []] ""
    , Test "!" [] "!"
    , Test "#" [] "#"
    , Test "$" [] "$"
    , Test "&" [] "&"
    , Test "(" [] "("
    , Test ")" [] ")"
    , Test "*" [] "*"
    , Test "+" [] "+"
    , Test "," [] ","
    , Test "-" [] "-"
    , Test "." [] "."
    , Test "/" [] "/"
    , Test "0" [] "0"
    , Test "9" [] "9"
    , Test ":" [] ":"
    , Test ";" [] ";"
    , Test "=" [] "="
    , Test "?" [] "?"
    , Test "@" [] "@"
    , Test "A" [] "A"
    , Test "Z" [] "Z"
    , Test "[" [] "["
    , Test "]" [] "]"
    , Test "_" [] "_"
    , Test "a" [] "a"
    , Test "z" [] "z"
    , Test "~" [] "~"
    , Test "\xa0" [] "%C2%A0"
    , Test "\xd7ff" [] "%ED%9F%BF"
    , Test "\xf900" [] "%EF%A4%80"
    , Test "\xfdcf" [] "%EF%B7%8F"
    , Test "\xfdf0" [] "%EF%B7%B0"
    , Test "\xffef" [] "%EF%BF%AF"
    , Test "\x10000" [] "%F0%90%80%80"
    , Test "\x1fffd" [] "%F0%9F%BF%BD"
    , Test "\x20000" [] "%F0%A0%80%80"
    , Test "\x2fffd" [] "%F0%AF%BF%BD"
    , Test "\x30000" [] "%F0%B0%80%80"
    , Test "\x3fffd" [] "%F0%BF%BF%BD"
    , Test "\x40000" [] "%F1%80%80%80"
    , Test "\x4fffd" [] "%F1%8F%BF%BD"
    , Test "\x50000" [] "%F1%90%80%80"
    , Test "\x5fffd" [] "%F1%9F%BF%BD"
    , Test "\x60000" [] "%F1%A0%80%80"
    , Test "\x6fffd" [] "%F1%AF%BF%BD"
    , Test "\x70000" [] "%F1%B0%80%80"
    , Test "\x7fffd" [] "%F1%BF%BF%BD"
    , Test "\x80000" [] "%F2%80%80%80"
    , Test "\x8fffd" [] "%F2%8F%BF%BD"
    , Test "\x90000" [] "%F2%90%80%80"
    , Test "\x9fffd" [] "%F2%9F%BF%BD"
    , Test "\xa0000" [] "%F2%A0%80%80"
    , Test "\xafffd" [] "%F2%AF%BF%BD"
    , Test "\xb0000" [] "%F2%B0%80%80"
    , Test "\xbfffd" [] "%F2%BF%BF%BD"
    , Test "\xc0000" [] "%F3%80%80%80"
    , Test "\xcfffd" [] "%F3%8F%BF%BD"
    , Test "\xd0000" [] "%F3%90%80%80"
    , Test "\xdfffd" [] "%F3%9F%BF%BD"
    , Test "\xe1000" [] "%F3%A1%80%80"
    , Test "\xefffd" [] "%F3%AF%BF%BD"
    , Test "\xe000" [] "%EE%80%80"
    , Test "\xf8ff" [] "%EF%A3%BF"
    , Test "\xf0000" [] "%F3%B0%80%80"
    , Test "\xffffd" [] "%F3%BF%BF%BD"
    , Test "\x100000" [] "%F4%80%80%80"
    , Test "\x10fffd" [] "%F4%8F%BF%BD"
    , Test "%00" [] "%00"
    , Test "%AA" [] "%AA"
    , Test "%Aa" [] "%Aa"
    , Test "%aA" [] "%aA"
    , Test "%aa" [] "%aa"
    , Test "%" [] noParse
    , Test "%0" [] noParse
    , Test "%0z" [] noParse
    , Test "%z" [] noParse
    , Test "%30" [] "%30"
    , Test " " [] noParse
    , Test "\"" [] noParse
    , Test "'" [] noParse
    , Test "%" [] noParse
    , Test "<" [] noParse
    , Test ">" [] noParse
    , Test "\\" [] noParse
    , Test "^" [] noParse
    , Test "`" [] noParse
    , Test "{" [] noParse
    , Test "|" [] noParse
    , Test "}" [] noParse
    , Test "{}" [] noParse
    , Test "{,}" [] noParse
    , Test "{a,,b}" [] noParse
    , Test "{+}" [] noParse
    , Test "{:1}" [] noParse
    , Test "{*}" [] noParse
    , Test "{AZ}" [] ""
    , Test "{az}" [] ""
    , Test "{09}" [] ""
    , Test "{_a}" [] ""
    , Test "{a_}" [] ""
    , Test "{_}" [] ""
    , Test "{A.A}" [] ""
    , Test "{a.a}" [] ""
    , Test "{0.0}" [] ""
    , Test "{_._}" [] ""
    , Test "{%aa.%aa}" [] ""
    , Test "{.}" [] noParse
    , Test "{a.}" [] noParse
    , Test "{+.a}" [] noParse
    , Test "{a..b}" [] noParse
    , Test "{%00}" [] ""
    , Test "{%}" [] noParse
    , Test "{%0}" [] noParse
    , Test "{%0z}" [] noParse
    , Test "{%z}" [] noParse
    , Test "{!}" [] noParse
    , Test "{" [] noParse
    , Test "{{}" [] noParse
    , Test "}" [] noParse
    , Test "{}}" [] noParse
    , Test "{a,b}" [] ""
    , Test "{a,b,c,d}" [] ""
    , Test "{a,a}" [] ""
    , Test "{a:5}" [] ""
    , Test "{a:67}" [] ""
    , Test "{a:801}" [] ""
    , Test "{a:234}" [] ""
    , Test "{a:9999}" [] ""
    , Test "{a:123}" ["a" =: s (replicate 200 'a')] . Output . Just $ replicate 123 'a'
    , Test "{a:}" [] noParse
    , Test "{a:0}" [] noParse
    , Test "{a:10000}" [] noParse
    , Test "{a:-1}" [] noParse
    , Test "{a*}" [] ""
    , Test "{a:1*}" [] noParse
    , Test "{a*:1}" [] noParse
    , Test "{a,b:1,c*}" [] ""
    , Test "{+a}" [] ""
    , Test "{#a}" [] ""
    , Test "{.a}" [] ""
    , Test "{/a}" [] ""
    , Test "{;a}" [] ""
    , Test "{?a}" [] ""
    , Test "{&a}" [] ""
    , Test "{=a}" [] noParse
    , Test "{,a}" [] noParse
    , Test "{!a}" [] noParse
    , Test "{@a}" [] noParse
    , Test "{|a}" [] noParse
    , Test "{+#a}" [] noParse
    , Test "{+a,#b}" [] noParse
    , Test "{+a:1}" [] ""
    , Test "{#a*}" [] ""
    , Test "{+a,b}" [] ""
    , Test "{#a,b}" [] ""
    , Test "{.a,b}" [] ""
    , Test "{/a,b}" [] ""
    , Test "{;a,b}" [] ""
    , Test "{?a,b}" [] ""
    , Test "{&a,b}" [] ""
    , Test "{a}{b}" [] ""
    , Test "{a}{b}{c}{d}" [] ""
    , Test "{a}{a}" [] ""
    , Test "{{}}" [] noParse
    , Test "{a{b}}" [] noParse
    , Test "{{a}b}" [] noParse
    , Test "{a{b}c}" [] noParse
    , Test "a{b}" [] "a"
    , Test "{a}b" [] "b"
    , Test "a{b}c" [] "ac"
    , Test "{a}b{c}" [] "b"
    , Test "{a}" [] ""
    , Test "{+a}" [] ""
    , Test "{#a}" [] ""
    , Test "{.a}" [] ""
    , Test "{/a}" [] ""
    , Test "{;a}" [] ""
    , Test "{?a}" [] ""
    , Test "{&a}" [] ""
    , Test "http://example.com/~{username}/" ["username" =: s "fred"] "http://example.com/~fred/"
    , Test "http://example.com/~{username}/" ["username" =: s "mark"] "http://example.com/~mark/"
    , Test "http://example.com/dictionary/{term:1}/{term}" ["term" =: s "cat"] "http://example.com/dictionary/c/cat"
    , Test "http://example.com/dictionary/{term:1}/{term}" ["term" =: s "dog"] "http://example.com/dictionary/d/dog"
    , Test "http://example.com/search{?q,lang}" ["q" =: s "cat", "lang" =: s "en"] "http://example.com/search?q=cat&lang=en"
    , Test "http://example.com/search{?q,lang}" ["q" =: s "chien", "lang" =: s "fr"] "http://example.com/search?q=chien&lang=fr"
    , Test "http://www.example.com/foo{?query,number}" ["query" =: s "mycelium", "number" =: s "100"] "http://www.example.com/foo?query=mycelium&number=100"
    , Test "http://www.example.com/foo{?query,number}" ["number" =: s "100"] "http://www.example.com/foo?number=100"
    , Test "http://www.example.com/foo{?query,number}" [] "http://www.example.com/foo"
    , Test "{a}" [] ""
    , Test "{a}" ["a" =: l []] ""
    , Test "{a}" ["a" =: d []] ""
    , Test "{a}" ["a" =: s ""] ""
    , Test "{a}" ["a" =: s "A"] "A"
    , Test "{a}" ["a" =: s "~"] "~"
    , Test "{a}" ["a" =: s "%"] "%25"
    , Test "{a}" ["a" =: s "?"] "%3F"
    , Test "{a}" ["a" =: s "&"] "%26"
    , Test "{a}" ["a" =: s "\xa0"] "%C2%A0"
    , Test "{a}" ["a" =: s "\xd7ff"] "%ED%9F%BF"
    , Test "{a}" ["a" =: s "\x10000"] "%F0%90%80%80"
    , Test "{a}" ["a" =: l ["A"]] "A"
    , Test "{a}" ["a" =: l ["A", "B"]] "A,B"
    , Test "{a}" ["a" =: l ["%"]] "%25"
    , Test "{a}" ["a" =: l ["\xa0"]] "%C2%A0"
    , Test "{a}" ["a" =: d ["A" =: "1"]] "A,1"
    , Test "{a}" ["a" =: d ["A" =: "1", "B" =: "2"]] "A,1,B,2"
    , Test "{a}" ["a" =: d ["A" =: "%"]] "A,%25"
    , Test "{a}" ["a" =: d ["A" =: "\xa0"]] "A,%C2%A0"
    , Test "{a}" ["a" =: d ["%" =: "1"]] "%25,1"
    , Test "{a*}" [] ""
    , Test "{a*}" ["a" =: s ""] ""
    , Test "{a*}" ["a" =: s "A"] "A"
    , Test "{a*}" ["a" =: l []] ""
    , Test "{a*}" ["a" =: l ["A"]] "A"
    , Test "{a*}" ["a" =: l ["A", "B"]] "A,B"
    , Test "{a*}" ["a" =: d []] ""
    , Test "{a*}" ["a" =: d ["A" =: "1"]] "A=1"
    , Test "{a*}" ["a" =: d ["A" =: "1", "B" =: "2"]] "A=1,B=2"
    , Test "{a:1}" [] ""
    , Test "{a:1}" ["a" =: s ""] ""
    , Test "{a:1}" ["a" =: s "A"] "A"
    , Test "{a:1}" ["a" =: s "AB"] "A"
    , Test "{a:1}" ["a" =: s "%B"] "%25"
    , Test "{a:1}" ["a" =: s "\xa0\&B"] "%C2%A0"
    , Test "{a:1}" ["a" =: s "\xd7ff\&B"] "%ED%9F%BF"
    , Test "{a:1}" ["a" =: s "\x10000\&B"] "%F0%90%80%80"
    , Test "{a:1}" ["a" =: l []] ""
    , Test "{a:1}" ["a" =: l ["AB"]] "AB"
    , Test "{a:1}" ["a" =: l ["AB", "CD"]] "AB,CD"
    , Test "{a:1}" ["a" =: d []] ""
    , Test "{a:1}" ["a" =: d ["AB" =: "12"]] "AB,12"
    , Test "{a:1}" ["a" =: d ["AB" =: "12", "CD" =: "34"]] "AB,12,CD,34"
    , Test "{a,a}" [] ""
    , Test "{a,a}" ["a" =: l []] ""
    , Test "{a,a}" ["a" =: d []] ""
    , Test "{a,a}" ["a" =: s ""] ","
    , Test "{a,b}" ["a" =: s ""] ""
    , Test "{a,b}" ["b" =: s ""] ""
    , Test "{%aa}" ["%aa" =: s "A"] "A"
    , Test "{%aa}" ["%aa" =: l ["A", "B"]] "A,B"
    , Test "{%aa}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "A,1,B,2"
    , Test "{%aa*}" ["%aa" =: s "A"] "A"
    , Test "{%aa*}" ["%aa" =: l ["A", "B"]] "A,B"
    , Test "{%aa*}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "A=1,B=2"
    , Test "{+a}" [] ""
    , Test "{+a}" ["a" =: l []] ""
    , Test "{+a}" ["a" =: d []] ""
    , Test "{+a}" ["a" =: s ""] ""
    , Test "{+a}" ["a" =: s "A"] "A"
    , Test "{+a}" ["a" =: s "~"] "~"
    , Test "{+a}" ["a" =: s "%"] "%25"
    , Test "{+a}" ["a" =: s "?"] "?"
    , Test "{+a}" ["a" =: s "&"] "&"
    , Test "{+a}" ["a" =: s "\xa0"] "%C2%A0"
    , Test "{+a}" ["a" =: s "\xd7ff"] "%ED%9F%BF"
    , Test "{+a}" ["a" =: s "\x10000"] "%F0%90%80%80"
    , Test "{+a}" ["a" =: l ["A"]] "A"
    , Test "{+a}" ["a" =: l ["A", "B"]] "A,B"
    , Test "{+a}" ["a" =: l ["%"]] "%25"
    , Test "{+a}" ["a" =: l ["\xa0"]] "%C2%A0"
    , Test "{+a}" ["a" =: d ["A" =: "1"]] "A,1"
    , Test "{+a}" ["a" =: d ["A" =: "1", "B" =: "2"]] "A,1,B,2"
    , Test "{+a}" ["a" =: d ["A" =: "%"]] "A,%25"
    , Test "{+a}" ["a" =: d ["A" =: "\xa0"]] "A,%C2%A0"
    , Test "{+a}" ["a" =: d ["%" =: "1"]] "%25,1"
    , Test "{+a*}" [] ""
    , Test "{+a*}" ["a" =: s ""] ""
    , Test "{+a*}" ["a" =: s "A"] "A"
    , Test "{+a*}" ["a" =: l []] ""
    , Test "{+a*}" ["a" =: l ["A"]] "A"
    , Test "{+a*}" ["a" =: l ["A", "B"]] "A,B"
    , Test "{+a*}" ["a" =: d []] ""
    , Test "{+a*}" ["a" =: d ["A" =: "1"]] "A=1"
    , Test "{+a*}" ["a" =: d ["A" =: "1", "B" =: "2"]] "A=1,B=2"
    , Test "{+a:1}" [] ""
    , Test "{+a:1}" ["a" =: s ""] ""
    , Test "{+a:1}" ["a" =: s "A"] "A"
    , Test "{+a:1}" ["a" =: s "AB"] "A"
    , Test "{+a:1}" ["a" =: s "%B"] "%25"
    , Test "{+a:1}" ["a" =: s "\xa0\&B"] "%C2%A0"
    , Test "{+a:1}" ["a" =: s "\xd7ff\&B"] "%ED%9F%BF"
    , Test "{+a:1}" ["a" =: s "\x10000\&B"] "%F0%90%80%80"
    , Test "{+a:1}" ["a" =: l []] ""
    , Test "{+a:1}" ["a" =: l ["AB"]] "AB"
    , Test "{+a:1}" ["a" =: l ["AB", "CD"]] "AB,CD"
    , Test "{+a:1}" ["a" =: d []] ""
    , Test "{+a:1}" ["a" =: d ["AB" =: "12"]] "AB,12"
    , Test "{+a:1}" ["a" =: d ["AB" =: "12", "CD" =: "34"]] "AB,12,CD,34"
    , Test "{+a,a}" [] ""
    , Test "{+a,a}" ["a" =: l []] ""
    , Test "{+a,a}" ["a" =: d []] ""
    , Test "{+a,a}" ["a" =: s ""] ","
    , Test "{+a,b}" ["a" =: s ""] ""
    , Test "{+a,b}" ["b" =: s ""] ""
    , Test "{+%aa}" ["%aa" =: s "A"] "A"
    , Test "{+%aa}" ["%aa" =: l ["A", "B"]] "A,B"
    , Test "{+%aa}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "A,1,B,2"
    , Test "{+%aa*}" ["%aa" =: s "A"] "A"
    , Test "{+%aa*}" ["%aa" =: l ["A", "B"]] "A,B"
    , Test "{+%aa*}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "A=1,B=2"
    , Test "{#a}" [] ""
    , Test "{#a}" ["a" =: l []] ""
    , Test "{#a}" ["a" =: d []] ""
    , Test "{#a}" ["a" =: s ""] "#"
    , Test "{#a}" ["a" =: s "A"] "#A"
    , Test "{#a}" ["a" =: s "~"] "#~"
    , Test "{#a}" ["a" =: s "%"] "#%25"
    , Test "{#a}" ["a" =: s "?"] "#?"
    , Test "{#a}" ["a" =: s "&"] "#&"
    , Test "{#a}" ["a" =: s "\xa0"] "#%C2%A0"
    , Test "{#a}" ["a" =: s "\xd7ff"] "#%ED%9F%BF"
    , Test "{#a}" ["a" =: s "\x10000"] "#%F0%90%80%80"
    , Test "{#a}" ["a" =: l ["A"]] "#A"
    , Test "{#a}" ["a" =: l ["A", "B"]] "#A,B"
    , Test "{#a}" ["a" =: l ["%"]] "#%25"
    , Test "{#a}" ["a" =: l ["\xa0"]] "#%C2%A0"
    , Test "{#a}" ["a" =: d ["A" =: "1"]] "#A,1"
    , Test "{#a}" ["a" =: d ["A" =: "1", "B" =: "2"]] "#A,1,B,2"
    , Test "{#a}" ["a" =: d ["A" =: "%"]] "#A,%25"
    , Test "{#a}" ["a" =: d ["A" =: "\xa0"]] "#A,%C2%A0"
    , Test "{#a}" ["a" =: d ["%" =: "1"]] "#%25,1"
    , Test "{#a*}" [] ""
    , Test "{#a*}" ["a" =: s ""] "#"
    , Test "{#a*}" ["a" =: s "A"] "#A"
    , Test "{#a*}" ["a" =: l []] ""
    , Test "{#a*}" ["a" =: l ["A"]] "#A"
    , Test "{#a*}" ["a" =: l ["A", "B"]] "#A,B"
    , Test "{#a*}" ["a" =: d []] ""
    , Test "{#a*}" ["a" =: d ["A" =: "1"]] "#A=1"
    , Test "{#a*}" ["a" =: d ["A" =: "1", "B" =: "2"]] "#A=1,B=2"
    , Test "{#a:1}" [] ""
    , Test "{#a:1}" ["a" =: s ""] "#"
    , Test "{#a:1}" ["a" =: s "A"] "#A"
    , Test "{#a:1}" ["a" =: s "AB"] "#A"
    , Test "{#a:1}" ["a" =: s "%B"] "#%25"
    , Test "{#a:1}" ["a" =: s "\xa0\&B"] "#%C2%A0"
    , Test "{#a:1}" ["a" =: s "\xd7ff\&B"] "#%ED%9F%BF"
    , Test "{#a:1}" ["a" =: s "\x10000\&B"] "#%F0%90%80%80"
    , Test "{#a:1}" ["a" =: l []] ""
    , Test "{#a:1}" ["a" =: l ["AB"]] "#AB"
    , Test "{#a:1}" ["a" =: l ["AB", "CD"]] "#AB,CD"
    , Test "{#a:1}" ["a" =: d []] ""
    , Test "{#a:1}" ["a" =: d ["AB" =: "12"]] "#AB,12"
    , Test "{#a:1}" ["a" =: d ["AB" =: "12", "CD" =: "34"]] "#AB,12,CD,34"
    , Test "{#a,a}" [] ""
    , Test "{#a,a}" ["a" =: l []] ""
    , Test "{#a,a}" ["a" =: d []] ""
    , Test "{#a,a}" ["a" =: s ""] "#,"
    , Test "{#a,b}" ["a" =: s ""] "#"
    , Test "{#a,b}" ["b" =: s ""] "#"
    , Test "{#%aa}" ["%aa" =: s "A"] "#A"
    , Test "{#%aa}" ["%aa" =: l ["A", "B"]] "#A,B"
    , Test "{#%aa}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "#A,1,B,2"
    , Test "{#%aa*}" ["%aa" =: s "A"] "#A"
    , Test "{#%aa*}" ["%aa" =: l ["A", "B"]] "#A,B"
    , Test "{#%aa*}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "#A=1,B=2"
    , Test "{.a}" [] ""
    , Test "{.a}" ["a" =: l []] ""
    , Test "{.a}" ["a" =: d []] ""
    , Test "{.a}" ["a" =: s ""] "."
    , Test "{.a}" ["a" =: s "A"] ".A"
    , Test "{.a}" ["a" =: s "~"] ".~"
    , Test "{.a}" ["a" =: s "%"] ".%25"
    , Test "{.a}" ["a" =: s "?"] ".%3F"
    , Test "{.a}" ["a" =: s "&"] ".%26"
    , Test "{.a}" ["a" =: s "\xa0"] ".%C2%A0"
    , Test "{.a}" ["a" =: s "\xd7ff"] ".%ED%9F%BF"
    , Test "{.a}" ["a" =: s "\x10000"] ".%F0%90%80%80"
    , Test "{.a}" ["a" =: l ["A"]] ".A"
    , Test "{.a}" ["a" =: l ["A", "B"]] ".A,B"
    , Test "{.a}" ["a" =: l ["%"]] ".%25"
    , Test "{.a}" ["a" =: l ["\xa0"]] ".%C2%A0"
    , Test "{.a}" ["a" =: d ["A" =: "1"]] ".A,1"
    , Test "{.a}" ["a" =: d ["A" =: "1", "B" =: "2"]] ".A,1,B,2"
    , Test "{.a}" ["a" =: d ["A" =: "%"]] ".A,%25"
    , Test "{.a}" ["a" =: d ["A" =: "\xa0"]] ".A,%C2%A0"
    , Test "{.a}" ["a" =: d ["%" =: "1"]] ".%25,1"
    , Test "{.a*}" [] ""
    , Test "{.a*}" ["a" =: s ""] "."
    , Test "{.a*}" ["a" =: s "A"] ".A"
    , Test "{.a*}" ["a" =: l []] ""
    , Test "{.a*}" ["a" =: l ["A"]] ".A"
    , Test "{.a*}" ["a" =: l ["A", "B"]] ".A.B"
    , Test "{.a*}" ["a" =: d []] ""
    , Test "{.a*}" ["a" =: d ["A" =: "1"]] ".A=1"
    , Test "{.a*}" ["a" =: d ["A" =: "1", "B" =: "2"]] ".A=1.B=2"
    , Test "{.a:1}" [] ""
    , Test "{.a:1}" ["a" =: s ""] "."
    , Test "{.a:1}" ["a" =: s "A"] ".A"
    , Test "{.a:1}" ["a" =: s "AB"] ".A"
    , Test "{.a:1}" ["a" =: s "%B"] ".%25"
    , Test "{.a:1}" ["a" =: s "\xa0\&B"] ".%C2%A0"
    , Test "{.a:1}" ["a" =: s "\xd7ff\&B"] ".%ED%9F%BF"
    , Test "{.a:1}" ["a" =: s "\x10000\&B"] ".%F0%90%80%80"
    , Test "{.a:1}" ["a" =: l []] ""
    , Test "{.a:1}" ["a" =: l ["AB"]] ".AB"
    , Test "{.a:1}" ["a" =: l ["AB", "CD"]] ".AB,CD"
    , Test "{.a:1}" ["a" =: d []] ""
    , Test "{.a:1}" ["a" =: d ["AB" =: "12"]] ".AB,12"
    , Test "{.a:1}" ["a" =: d ["AB" =: "12", "CD" =: "34"]] ".AB,12,CD,34"
    , Test "{.a,a}" [] ""
    , Test "{.a,a}" ["a" =: l []] ""
    , Test "{.a,a}" ["a" =: d []] ""
    , Test "{.a,a}" ["a" =: s ""] ".."
    , Test "{.a,b}" ["a" =: s ""] "."
    , Test "{.a,b}" ["b" =: s ""] "."
    , Test "{.%aa}" ["%aa" =: s "A"] ".A"
    , Test "{.%aa}" ["%aa" =: l ["A", "B"]] ".A,B"
    , Test "{.%aa}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] ".A,1,B,2"
    , Test "{.%aa*}" ["%aa" =: s "A"] ".A"
    , Test "{.%aa*}" ["%aa" =: l ["A", "B"]] ".A.B"
    , Test "{.%aa*}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] ".A=1.B=2"
    , Test "{/a}" [] ""
    , Test "{/a}" ["a" =: l []] ""
    , Test "{/a}" ["a" =: d []] ""
    , Test "{/a}" ["a" =: s ""] "/"
    , Test "{/a}" ["a" =: s "A"] "/A"
    , Test "{/a}" ["a" =: s "~"] "/~"
    , Test "{/a}" ["a" =: s "%"] "/%25"
    , Test "{/a}" ["a" =: s "?"] "/%3F"
    , Test "{/a}" ["a" =: s "&"] "/%26"
    , Test "{/a}" ["a" =: s "\xa0"] "/%C2%A0"
    , Test "{/a}" ["a" =: s "\xd7ff"] "/%ED%9F%BF"
    , Test "{/a}" ["a" =: s "\x10000"] "/%F0%90%80%80"
    , Test "{/a}" ["a" =: l ["A"]] "/A"
    , Test "{/a}" ["a" =: l ["A", "B"]] "/A,B"
    , Test "{/a}" ["a" =: l ["%"]] "/%25"
    , Test "{/a}" ["a" =: l ["\xa0"]] "/%C2%A0"
    , Test "{/a}" ["a" =: d ["A" =: "1"]] "/A,1"
    , Test "{/a}" ["a" =: d ["A" =: "1", "B" =: "2"]] "/A,1,B,2"
    , Test "{/a}" ["a" =: d ["A" =: "%"]] "/A,%25"
    , Test "{/a}" ["a" =: d ["A" =: "\xa0"]] "/A,%C2%A0"
    , Test "{/a}" ["a" =: d ["%" =: "1"]] "/%25,1"
    , Test "{/a*}" [] ""
    , Test "{/a*}" ["a" =: s ""] "/"
    , Test "{/a*}" ["a" =: s "A"] "/A"
    , Test "{/a*}" ["a" =: l []] ""
    , Test "{/a*}" ["a" =: l ["A"]] "/A"
    , Test "{/a*}" ["a" =: l ["A", "B"]] "/A/B"
    , Test "{/a*}" ["a" =: d []] ""
    , Test "{/a*}" ["a" =: d ["A" =: "1"]] "/A=1"
    , Test "{/a*}" ["a" =: d ["A" =: "1", "B" =: "2"]] "/A=1/B=2"
    , Test "{/a:1}" [] ""
    , Test "{/a:1}" ["a" =: s ""] "/"
    , Test "{/a:1}" ["a" =: s "A"] "/A"
    , Test "{/a:1}" ["a" =: s "AB"] "/A"
    , Test "{/a:1}" ["a" =: s "%B"] "/%25"
    , Test "{/a:1}" ["a" =: s "\xa0\&B"] "/%C2%A0"
    , Test "{/a:1}" ["a" =: s "\xd7ff\&B"] "/%ED%9F%BF"
    , Test "{/a:1}" ["a" =: s "\x10000\&B"] "/%F0%90%80%80"
    , Test "{/a:1}" ["a" =: l []] ""
    , Test "{/a:1}" ["a" =: l ["AB"]] "/AB"
    , Test "{/a:1}" ["a" =: l ["AB", "CD"]] "/AB,CD"
    , Test "{/a:1}" ["a" =: d []] ""
    , Test "{/a:1}" ["a" =: d ["AB" =: "12"]] "/AB,12"
    , Test "{/a:1}" ["a" =: d ["AB" =: "12", "CD" =: "34"]] "/AB,12,CD,34"
    , Test "{/a,a}" [] ""
    , Test "{/a,a}" ["a" =: l []] ""
    , Test "{/a,a}" ["a" =: d []] ""
    , Test "{/a,a}" ["a" =: s ""] "//"
    , Test "{/a,b}" ["a" =: s ""] "/"
    , Test "{/a,b}" ["b" =: s ""] "/"
    , Test "{/%aa}" ["%aa" =: s "A"] "/A"
    , Test "{/%aa}" ["%aa" =: l ["A", "B"]] "/A,B"
    , Test "{/%aa}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "/A,1,B,2"
    , Test "{/%aa*}" ["%aa" =: s "A"] "/A"
    , Test "{/%aa*}" ["%aa" =: l ["A", "B"]] "/A/B"
    , Test "{/%aa*}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "/A=1/B=2"
    , Test "{;a}" [] ""
    , Test "{;a}" ["a" =: l []] ""
    , Test "{;a}" ["a" =: d []] ""
    , Test "{;a}" ["a" =: s ""] ";a"
    , Test "{;a}" ["a" =: s "A"] ";a=A"
    , Test "{;a}" ["a" =: s "~"] ";a=~"
    , Test "{;a}" ["a" =: s "%"] ";a=%25"
    , Test "{;a}" ["a" =: s "?"] ";a=%3F"
    , Test "{;a}" ["a" =: s "&"] ";a=%26"
    , Test "{;a}" ["a" =: s "\xa0"] ";a=%C2%A0"
    , Test "{;a}" ["a" =: s "\xd7ff"] ";a=%ED%9F%BF"
    , Test "{;a}" ["a" =: s "\x10000"] ";a=%F0%90%80%80"
    , Test "{;a}" ["a" =: l ["A"]] ";a=A"
    , Test "{;a}" ["a" =: l ["A", "B"]] ";a=A,B"
    , Test "{;a}" ["a" =: l ["%"]] ";a=%25"
    , Test "{;a}" ["a" =: l ["\xa0"]] ";a=%C2%A0"
    , Test "{;a}" ["a" =: d ["A" =: "1"]] ";a=A,1"
    , Test "{;a}" ["a" =: d ["A" =: "1", "B" =: "2"]] ";a=A,1,B,2"
    , Test "{;a}" ["a" =: d ["A" =: "%"]] ";a=A,%25"
    , Test "{;a}" ["a" =: d ["A" =: "\xa0"]] ";a=A,%C2%A0"
    , Test "{;a}" ["a" =: d ["%" =: "1"]] ";a=%25,1"
    , Test "{;a*}" [] ""
    , Test "{;a*}" ["a" =: s ""] ";a"
    , Test "{;a*}" ["a" =: s "A"] ";a=A"
    , Test "{;a*}" ["a" =: l []] ""
    , Test "{;a*}" ["a" =: l ["A"]] ";a=A"
    , Test "{;a*}" ["a" =: l ["A", "B"]] ";a=A;a=B"
    , Test "{;a*}" ["a" =: d []] ""
    , Test "{;a*}" ["a" =: d ["A" =: "1"]] ";A=1"
    , Test "{;a*}" ["a" =: d ["A" =: "1", "B" =: "2"]] ";A=1;B=2"
    , Test "{;a:1}" [] ""
    , Test "{;a:1}" ["a" =: s ""] ";a"
    , Test "{;a:1}" ["a" =: s "A"] ";a=A"
    , Test "{;a:1}" ["a" =: s "AB"] ";a=A"
    , Test "{;a:1}" ["a" =: s "%B"] ";a=%25"
    , Test "{;a:1}" ["a" =: s "\xa0\&B"] ";a=%C2%A0"
    , Test "{;a:1}" ["a" =: s "\xd7ff\&B"] ";a=%ED%9F%BF"
    , Test "{;a:1}" ["a" =: s "\x10000\&B"] ";a=%F0%90%80%80"
    , Test "{;a:1}" ["a" =: l []] ""
    , Test "{;a:1}" ["a" =: l ["AB"]] ";a=AB"
    , Test "{;a:1}" ["a" =: l ["AB", "CD"]] ";a=AB,CD"
    , Test "{;a:1}" ["a" =: d []] ""
    , Test "{;a:1}" ["a" =: d ["AB" =: "12"]] ";a=AB,12"
    , Test "{;a:1}" ["a" =: d ["AB" =: "12", "CD" =: "34"]] ";a=AB,12,CD,34"
    , Test "{;a,a}" [] ""
    , Test "{;a,a}" ["a" =: l []] ""
    , Test "{;a,a}" ["a" =: d []] ""
    , Test "{;a,a}" ["a" =: s ""] ";a;a"
    , Test "{;a,b}" ["a" =: s ""] ";a"
    , Test "{;a,b}" ["b" =: s ""] ";b"
    , Test "{;%aa}" ["%aa" =: s "A"] ";%aa=A"
    , Test "{;%aa}" ["%aa" =: l ["A", "B"]] ";%aa=A,B"
    , Test "{;%aa}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] ";%aa=A,1,B,2"
    , Test "{;%aa*}" ["%aa" =: s "A"] ";%aa=A"
    , Test "{;%aa*}" ["%aa" =: l ["A", "B"]] ";%aa=A;%aa=B"
    , Test "{;%aa*}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] ";A=1;B=2"
    , Test "{?a}" [] ""
    , Test "{?a}" ["a" =: l []] ""
    , Test "{?a}" ["a" =: d []] ""
    , Test "{?a}" ["a" =: s ""] "?a="
    , Test "{?a}" ["a" =: s "A"] "?a=A"
    , Test "{?a}" ["a" =: s "~"] "?a=~"
    , Test "{?a}" ["a" =: s "%"] "?a=%25"
    , Test "{?a}" ["a" =: s "?"] "?a=%3F"
    , Test "{?a}" ["a" =: s "&"] "?a=%26"
    , Test "{?a}" ["a" =: s "\xa0"] "?a=%C2%A0"
    , Test "{?a}" ["a" =: s "\xd7ff"] "?a=%ED%9F%BF"
    , Test "{?a}" ["a" =: s "\x10000"] "?a=%F0%90%80%80"
    , Test "{?a}" ["a" =: l ["A"]] "?a=A"
    , Test "{?a}" ["a" =: l ["A", "B"]] "?a=A,B"
    , Test "{?a}" ["a" =: l ["%"]] "?a=%25"
    , Test "{?a}" ["a" =: l ["\xa0"]] "?a=%C2%A0"
    , Test "{?a}" ["a" =: d ["A" =: "1"]] "?a=A,1"
    , Test "{?a}" ["a" =: d ["A" =: "1", "B" =: "2"]] "?a=A,1,B,2"
    , Test "{?a}" ["a" =: d ["A" =: "%"]] "?a=A,%25"
    , Test "{?a}" ["a" =: d ["A" =: "\xa0"]] "?a=A,%C2%A0"
    , Test "{?a}" ["a" =: d ["%" =: "1"]] "?a=%25,1"
    , Test "{?a*}" [] ""
    , Test "{?a*}" ["a" =: s ""] "?a="
    , Test "{?a*}" ["a" =: s "A"] "?a=A"
    , Test "{?a*}" ["a" =: l []] ""
    , Test "{?a*}" ["a" =: l ["A"]] "?a=A"
    , Test "{?a*}" ["a" =: l ["A", "B"]] "?a=A&a=B"
    , Test "{?a*}" ["a" =: d []] ""
    , Test "{?a*}" ["a" =: d ["A" =: "1"]] "?A=1"
    , Test "{?a*}" ["a" =: d ["A" =: "1", "B" =: "2"]] "?A=1&B=2"
    , Test "{?a:1}" [] ""
    , Test "{?a:1}" ["a" =: s ""] "?a="
    , Test "{?a:1}" ["a" =: s "A"] "?a=A"
    , Test "{?a:1}" ["a" =: s "AB"] "?a=A"
    , Test "{?a:1}" ["a" =: s "%B"] "?a=%25"
    , Test "{?a:1}" ["a" =: s "\xa0\&B"] "?a=%C2%A0"
    , Test "{?a:1}" ["a" =: s "\xd7ff\&B"] "?a=%ED%9F%BF"
    , Test "{?a:1}" ["a" =: s "\x10000\&B"] "?a=%F0%90%80%80"
    , Test "{?a:1}" ["a" =: l []] ""
    , Test "{?a:1}" ["a" =: l ["AB"]] "?a=AB"
    , Test "{?a:1}" ["a" =: l ["AB", "CD"]] "?a=AB,CD"
    , Test "{?a:1}" ["a" =: d []] ""
    , Test "{?a:1}" ["a" =: d ["AB" =: "12"]] "?a=AB,12"
    , Test "{?a:1}" ["a" =: d ["AB" =: "12", "CD" =: "34"]] "?a=AB,12,CD,34"
    , Test "{?a,a}" [] ""
    , Test "{?a,a}" ["a" =: l []] ""
    , Test "{?a,a}" ["a" =: d []] ""
    , Test "{?a,a}" ["a" =: s ""] "?a=&a="
    , Test "{?a,b}" ["a" =: s ""] "?a="
    , Test "{?a,b}" ["b" =: s ""] "?b="
    , Test "{?%aa}" ["%aa" =: s "A"] "?%aa=A"
    , Test "{?%aa}" ["%aa" =: l ["A", "B"]] "?%aa=A,B"
    , Test "{?%aa}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "?%aa=A,1,B,2"
    , Test "{?%aa*}" ["%aa" =: s "A"] "?%aa=A"
    , Test "{?%aa*}" ["%aa" =: l ["A", "B"]] "?%aa=A&%aa=B"
    , Test "{?%aa*}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "?A=1&B=2"
    , Test "{&a}" [] ""
    , Test "{&a}" ["a" =: l []] ""
    , Test "{&a}" ["a" =: d []] ""
    , Test "{&a}" ["a" =: s ""] "&a="
    , Test "{&a}" ["a" =: s "A"] "&a=A"
    , Test "{&a}" ["a" =: s "~"] "&a=~"
    , Test "{&a}" ["a" =: s "%"] "&a=%25"
    , Test "{&a}" ["a" =: s "?"] "&a=%3F"
    , Test "{&a}" ["a" =: s "&"] "&a=%26"
    , Test "{&a}" ["a" =: s "\xa0"] "&a=%C2%A0"
    , Test "{&a}" ["a" =: s "\xd7ff"] "&a=%ED%9F%BF"
    , Test "{&a}" ["a" =: s "\x10000"] "&a=%F0%90%80%80"
    , Test "{&a}" ["a" =: l ["A"]] "&a=A"
    , Test "{&a}" ["a" =: l ["A", "B"]] "&a=A,B"
    , Test "{&a}" ["a" =: l ["%"]] "&a=%25"
    , Test "{&a}" ["a" =: l ["\xa0"]] "&a=%C2%A0"
    , Test "{&a}" ["a" =: d ["A" =: "1"]] "&a=A,1"
    , Test "{&a}" ["a" =: d ["A" =: "1", "B" =: "2"]] "&a=A,1,B,2"
    , Test "{&a}" ["a" =: d ["A" =: "%"]] "&a=A,%25"
    , Test "{&a}" ["a" =: d ["A" =: "\xa0"]] "&a=A,%C2%A0"
    , Test "{&a}" ["a" =: d ["%" =: "1"]] "&a=%25,1"
    , Test "{&a*}" [] ""
    , Test "{&a*}" ["a" =: s ""] "&a="
    , Test "{&a*}" ["a" =: s "A"] "&a=A"
    , Test "{&a*}" ["a" =: l []] ""
    , Test "{&a*}" ["a" =: l ["A"]] "&a=A"
    , Test "{&a*}" ["a" =: l ["A", "B"]] "&a=A&a=B"
    , Test "{&a*}" ["a" =: d []] ""
    , Test "{&a*}" ["a" =: d ["A" =: "1"]] "&A=1"
    , Test "{&a*}" ["a" =: d ["A" =: "1", "B" =: "2"]] "&A=1&B=2"
    , Test "{&a:1}" [] ""
    , Test "{&a:1}" ["a" =: s ""] "&a="
    , Test "{&a:1}" ["a" =: s "A"] "&a=A"
    , Test "{&a:1}" ["a" =: s "AB"] "&a=A"
    , Test "{&a:1}" ["a" =: s "%B"] "&a=%25"
    , Test "{&a:1}" ["a" =: s "\xa0\&B"] "&a=%C2%A0"
    , Test "{&a:1}" ["a" =: s "\xd7ff\&B"] "&a=%ED%9F%BF"
    , Test "{&a:1}" ["a" =: s "\x10000\&B"] "&a=%F0%90%80%80"
    , Test "{&a:1}" ["a" =: l []] ""
    , Test "{&a:1}" ["a" =: l ["AB"]] "&a=AB"
    , Test "{&a:1}" ["a" =: l ["AB", "CD"]] "&a=AB,CD"
    , Test "{&a:1}" ["a" =: d []] ""
    , Test "{&a:1}" ["a" =: d ["AB" =: "12"]] "&a=AB,12"
    , Test "{&a:1}" ["a" =: d ["AB" =: "12", "CD" =: "34"]] "&a=AB,12,CD,34"
    , Test "{&a,a}" [] ""
    , Test "{&a,a}" ["a" =: l []] ""
    , Test "{&a,a}" ["a" =: d []] ""
    , Test "{&a,a}" ["a" =: s ""] "&a=&a="
    , Test "{&a,b}" ["a" =: s ""] "&a="
    , Test "{&a,b}" ["b" =: s ""] "&b="
    , Test "{&%aa}" ["%aa" =: s "A"] "&%aa=A"
    , Test "{&%aa}" ["%aa" =: l ["A", "B"]] "&%aa=A,B"
    , Test "{&%aa}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "&%aa=A,1,B,2"
    , Test "{&%aa*}" ["%aa" =: s "A"] "&%aa=A"
    , Test "{&%aa*}" ["%aa" =: l ["A", "B"]] "&%aa=A&%aa=B"
    , Test "{&%aa*}" ["%aa" =: d ["A" =: "1", "B" =: "2"]] "&A=1&B=2"
    ]
  , let values = ["%AA" =: s "1", "%Aa" =: s "2", "%aA" =: s "3", "%aa" =: s "4"]
    in
      [ Test "{%AA}" values "1"
      , Test "{%Aa}" values "2"
      , Test "{%aA}" values "3"
      , Test "{%aa}" values "4"
          -- This comment forces Brittany to use a multi-line layout.
      ]
  , let values = ["a" =: s "abcdefghijklmnopqrstuvwxyz"]
    in
      [ Test "{a:1}" values "a"
      , Test "{a:5}" values "abcde"
      , Test "{a:10}" values "abcdefghij"
      , Test "{a:15}" values "abcdefghijklmno"
      , Test "{a:20}" values "abcdefghijklmnopqrst"
      , Test "{a:25}" values "abcdefghijklmnopqrstuvwxy"
      , Test "{a:26}" values "abcdefghijklmnopqrstuvwxyz"
      , Test "{a:30}" values "abcdefghijklmnopqrstuvwxyz"
      ]
  , let values = ["a" =: l []]
    in
      [ Test "{a}" values ""
      , Test "{+a}" values ""
      , Test "{#a}" values ""
      , Test "{.a}" values ""
      , Test "{/a}" values ""
      , Test "{;a}" values ""
      , Test "{?a}" values ""
      , Test "{&a}" values ""
      ]
  , let values = ["a" =: d []]
    in
      [ Test "{a}" values ""
      , Test "{+a}" values ""
      , Test "{#a}" values ""
      , Test "{.a}" values ""
      , Test "{/a}" values ""
      , Test "{;a}" values ""
      , Test "{?a}" values ""
      , Test "{&a}" values ""
      ]
  , let values = ["a" =: s ""]
    in
      [ Test "{a}" values ""
      , Test "{+a}" values ""
      , Test "{#a}" values "#"
      , Test "{.a}" values "."
      , Test "{/a}" values "/"
      , Test "{;a}" values ";a"
      , Test "{?a}" values "?a="
      , Test "{&a}" values "&a="
      ]
  , let values = ["a" =: s "A"]
    in
      [ Test "{a}" values "A"
      , Test "{+a}" values "A"
      , Test "{#a}" values "#A"
      , Test "{.a}" values ".A"
      , Test "{/a}" values "/A"
      , Test "{;a}" values ";a=A"
      , Test "{?a}" values "?a=A"
      , Test "{&a}" values "&a=A"
      ]
  , let values = ["b" =: s "B"]
    in
      [ Test "{a,b}" values "B"
      , Test "{+a,b}" values "B"
      , Test "{#a,b}" values "#B"
      , Test "{.a,b}" values ".B"
      , Test "{/a,b}" values "/B"
      , Test "{;a,b}" values ";b=B"
      , Test "{?a,b}" values "?b=B"
      , Test "{&a,b}" values "&b=B"
      ]
  , let values = ["a" =: s ""]
    in
      [ Test "{a,a}" values ","
      , Test "{+a,a}" values ","
      , Test "{#a,a}" values "#,"
      , Test "{.a,a}" values ".."
      , Test "{/a,a}" values "//"
      , Test "{;a,a}" values ";a;a"
      , Test "{?a,a}" values "?a=&a="
      , Test "{&a,a}" values "&a=&a="
      ]
  , let values = ["a" =: s "A", "b" =: s "B"]
    in
      [ Test "{a,b}" values "A,B"
      , Test "{+a,b}" values "A,B"
      , Test "{#a,b}" values "#A,B"
      , Test "{.a,b}" values ".A.B"
      , Test "{/a,b}" values "/A/B"
      , Test "{;a,b}" values ";a=A;b=B"
      , Test "{?a,b}" values "?a=A&b=B"
      , Test "{&a,b}" values "&a=A&b=B"
      ]
  , let values = ["a" =: d ["K! \xa0\xd7ff\x10000" =: "V! \xa0\xd7ff\x10000"]]
    in
      [ Test "{a*}" values "K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
      , Test "{+a*}" values "K!%20%C2%A0%ED%9F%BF%F0%90%80%80=V!%20%C2%A0%ED%9F%BF%F0%90%80%80"
      , Test "{#a*}" values "#K!%20%C2%A0%ED%9F%BF%F0%90%80%80=V!%20%C2%A0%ED%9F%BF%F0%90%80%80"
      , Test "{.a*}" values ".K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
      , Test "{/a*}" values "/K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
      , Test "{;a*}" values ";K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
      , Test "{?a*}" values "?K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
      , Test "{&a*}" values "&K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
      ]
  , let
      values =
        [ "empty" =: s ""
        , "hello" =: s "Hello World!"
        , "keys" =: d ["semi" =: ";", "dot" =: ".", "comma" =: ","]
        , "list" =: l ["red", "green", "blue"]
        , "path" =: s "/foo/bar"
        , "var" =: s "value"
        , "x" =: s "1024"
        , "y" =: s "768"
        ]
    in
      [ Test "{var}" values "value"
      , Test "{hello}" values "Hello%20World%21"
      , Test "{+var}" values "value"
      , Test "{+hello}" values "Hello%20World!"
      , Test "{+path}/here" values "/foo/bar/here"
      , Test "here?ref={+path}" values "here?ref=/foo/bar"
      , Test "X{#var}" values "X#value"
      , Test "X{#hello}" values "X#Hello%20World!"
      , Test "map?{x,y}" values "map?1024,768"
      , Test "{x,hello,y}" values "1024,Hello%20World%21,768"
      , Test "{+x,hello,y}" values "1024,Hello%20World!,768"
      , Test "{+path,x}/here" values "/foo/bar,1024/here"
      , Test "{#x,hello,y}" values "#1024,Hello%20World!,768"
      , Test "{#path,x}/here" values "#/foo/bar,1024/here"
      , Test "X{.var}" values "X.value"
      , Test "X{.x,y}" values "X.1024.768"
      , Test "{/var}" values "/value"
      , Test "{/var,x}/here" values "/value/1024/here"
      , Test "{;x,y}" values ";x=1024;y=768"
      , Test "{;x,y,empty}" values ";x=1024;y=768;empty"
      , Test "{?x,y}" values "?x=1024&y=768"
      , Test "{?x,y,empty}" values "?x=1024&y=768&empty="
      , Test "?fixed=yes{&x}" values "?fixed=yes&x=1024"
      , Test "{&x,y,empty}" values "&x=1024&y=768&empty="
      , Test "{var:3}" values "val"
      , Test "{var:30}" values "value"
      , Test "{list}" values "red,green,blue"
      , Test "{list*}" values "red,green,blue"
      , Test "{keys}" values "comma,%2C,dot,.,semi,%3B"
      , Test "{keys*}" values "comma=%2C,dot=.,semi=%3B"
      , Test "{+path:6}/here" values "/foo/b/here"
      , Test "{+list}" values "red,green,blue"
      , Test "{+list*}" values "red,green,blue"
      , Test "{+keys}" values "comma,,,dot,.,semi,;"
      , Test "{+keys*}" values "comma=,,dot=.,semi=;"
      , Test "{#path:6}/here" values "#/foo/b/here"
      , Test "{#list}" values "#red,green,blue"
      , Test "{#list*}" values "#red,green,blue"
      , Test "{#keys}" values "#comma,,,dot,.,semi,;"
      , Test "{#keys*}" values "#comma=,,dot=.,semi=;"
      , Test "X{.var:3}" values "X.val"
      , Test "X{.list}" values "X.red,green,blue"
      , Test "X{.list*}" values "X.red.green.blue"
      , Test "X{.keys}" values "X.comma,%2C,dot,.,semi,%3B"
      , Test "X{.keys*}" values "X.comma=%2C.dot=..semi=%3B"
      , Test "{/var:1,var}" values "/v/value"
      , Test "{/list}" values "/red,green,blue"
      , Test "{/list*}" values "/red/green/blue"
      , Test "{/list*,path:4}" values "/red/green/blue/%2Ffoo"
      , Test "{/keys}" values "/comma,%2C,dot,.,semi,%3B"
      , Test "{/keys*}" values "/comma=%2C/dot=./semi=%3B"
      , Test "{;hello:5}" values ";hello=Hello"
      , Test "{;list}" values ";list=red,green,blue"
      , Test "{;list*}" values ";list=red;list=green;list=blue"
      , Test "{;keys}" values ";keys=comma,%2C,dot,.,semi,%3B"
      , Test "{;keys*}" values ";comma=%2C;dot=.;semi=%3B"
      , Test "{?var:3}" values "?var=val"
      , Test "{?list}" values "?list=red,green,blue"
      , Test "{?list*}" values "?list=red&list=green&list=blue"
      , Test "{?keys}" values "?keys=comma,%2C,dot,.,semi,%3B"
      , Test "{?keys*}" values "?comma=%2C&dot=.&semi=%3B"
      , Test "{&var:3}" values "&var=val"
      , Test "{&list}" values "&list=red,green,blue"
      , Test "{&list*}" values "&list=red&list=green&list=blue"
      , Test "{&keys}" values "&keys=comma,%2C,dot,.,semi,%3B"
      , Test "{&keys*}" values "&comma=%2C&dot=.&semi=%3B"
      ]
  , let values = ["var" =: s "value", "semi" =: s ";"]
    in
      [ Test "{var}" values "value"
      , Test "{var:20}" values "value"
      , Test "{var:3}" values "val"
      , Test "{semi}" values "%3B"
      , Test "{semi:2}" values "%3B"
          -- This comment forces Brittany to use a multi-line layout.
      ]
  , let values = ["year" =: l ["1965", "2000", "2012"], "dom" =: l ["example", "com"]]
    in
      [ Test "find{?year*}" values "find?year=1965&year=2000&year=2012"
      , Test "www{.dom*}" values "www.example.com"
          -- This comment forces Brittany to use a multi-line layout.
      ]
  , let
      values =
        [ "base" =: s "http://example.com/home/"
        , "count" =: l ["one", "two", "three"]
        , "dom" =: l ["example", "com"]
        , "dub" =: s "me/too"
        , "empty_keys" =: d []
        , "empty" =: s ""
        , "half" =: s "50%"
        , "hello" =: s "Hello World!"
        , "keys" =: d ["semi" =: ";", "dot" =: ".", "comma" =: ","]
        , "list" =: l ["red", "green", "blue"]
        , "path" =: s "/foo/bar"
        , "v" =: s "6"
        , "var" =: s "value"
        , "who" =: s "fred"
        , "x" =: s "1024"
        , "y" =: s "768"
        ]
    in
      [ Test "{count}" values "one,two,three"
      , Test "{count*}" values "one,two,three"
      , Test "{/count}" values "/one,two,three"
      , Test "{/count*}" values "/one/two/three"
      , Test "{;count}" values ";count=one,two,three"
      , Test "{;count*}" values ";count=one;count=two;count=three"
      , Test "{?count}" values "?count=one,two,three"
      , Test "{?count*}" values "?count=one&count=two&count=three"
      , Test "{&count*}" values "&count=one&count=two&count=three"
      , Test "{var}" values "value"
      , Test "{hello}" values "Hello%20World%21"
      , Test "{half}" values "50%25"
      , Test "O{empty}X" values "OX"
      , Test "O{undef}X" values "OX"
      , Test "{x,y}" values "1024,768"
      , Test "{x,hello,y}" values "1024,Hello%20World%21,768"
      , Test "?{x,empty}" values "?1024,"
      , Test "?{x,undef}" values "?1024"
      , Test "?{undef,y}" values "?768"
      , Test "{var:3}" values "val"
      , Test "{var:30}" values "value"
      , Test "{list}" values "red,green,blue"
      , Test "{list*}" values "red,green,blue"
      , Test "{keys}" values "comma,%2C,dot,.,semi,%3B"
      , Test "{keys*}" values "comma=%2C,dot=.,semi=%3B"
      , Test "{+var}" values "value"
      , Test "{+hello}" values "Hello%20World!"
      , Test "{+half}" values "50%25"
      , Test "{base}index" values "http%3A%2F%2Fexample.com%2Fhome%2Findex"
      , Test "{+base}index" values "http://example.com/home/index"
      , Test "O{+empty}X" values "OX"
      , Test "O{+undef}X" values "OX"
      , Test "{+path}/here" values "/foo/bar/here"
      , Test "here?ref={+path}" values "here?ref=/foo/bar"
      , Test "up{+path}{var}/here" values "up/foo/barvalue/here"
      , Test "{+x,hello,y}" values "1024,Hello%20World!,768"
      , Test "{+path,x}/here" values "/foo/bar,1024/here"
      , Test "{+path:6}/here" values "/foo/b/here"
      , Test "{+list}" values "red,green,blue"
      , Test "{+list*}" values "red,green,blue"
      , Test "{+keys}" values "comma,,,dot,.,semi,;"
      , Test "{+keys*}" values "comma=,,dot=.,semi=;"
      , Test "{#var}" values "#value"
      , Test "{#hello}" values "#Hello%20World!"
      , Test "{#half}" values "#50%25"
      , Test "foo{#empty}" values "foo#"
      , Test "foo{#undef}" values "foo"
      , Test "{#x,hello,y}" values "#1024,Hello%20World!,768"
      , Test "{#path,x}/here" values "#/foo/bar,1024/here"
      , Test "{#path:6}/here" values "#/foo/b/here"
      , Test "{#list}" values "#red,green,blue"
      , Test "{#list*}" values "#red,green,blue"
      , Test "{#keys}" values "#comma,,,dot,.,semi,;"
      , Test "{#keys*}" values "#comma=,,dot=.,semi=;"
      , Test "{.who}" values ".fred"
      , Test "{.who,who}" values ".fred.fred"
      , Test "{.half,who}" values ".50%25.fred"
      , Test "www{.dom*}" values "www.example.com"
      , Test "X{.var}" values "X.value"
      , Test "X{.empty}" values "X."
      , Test "X{.undef}" values "X"
      , Test "X{.var:3}" values "X.val"
      , Test "X{.list}" values "X.red,green,blue"
      , Test "X{.list*}" values "X.red.green.blue"
      , Test "X{.keys}" values "X.comma,%2C,dot,.,semi,%3B"
      , Test "X{.keys*}" values "X.comma=%2C.dot=..semi=%3B"
      , Test "X{.empty_keys}" values "X"
      , Test "X{.empty_keys*}" values "X"
      , Test "{/who}" values "/fred"
      , Test "{/who,who}" values "/fred/fred"
      , Test "{/half,who}" values "/50%25/fred"
      , Test "{/who,dub}" values "/fred/me%2Ftoo"
      , Test "{/var}" values "/value"
      , Test "{/var,empty}" values "/value/"
      , Test "{/var,undef}" values "/value"
      , Test "{/var,x}/here" values "/value/1024/here"
      , Test "{/var:1,var}" values "/v/value"
      , Test "{/list}" values "/red,green,blue"
      , Test "{/list*}" values "/red/green/blue"
      , Test "{/list*,path:4}" values "/red/green/blue/%2Ffoo"
      , Test "{/keys}" values "/comma,%2C,dot,.,semi,%3B"
      , Test "{/keys*}" values "/comma=%2C/dot=./semi=%3B"
      , Test "{;who}" values ";who=fred"
      , Test "{;half}" values ";half=50%25"
      , Test "{;empty}" values ";empty"
      , Test "{;v,empty,who}" values ";v=6;empty;who=fred"
      , Test "{;v,bar,who}" values ";v=6;who=fred"
      , Test "{;x,y}" values ";x=1024;y=768"
      , Test "{;x,y,empty}" values ";x=1024;y=768;empty"
      , Test "{;x,y,undef}" values ";x=1024;y=768"
      , Test "{;hello:5}" values ";hello=Hello"
      , Test "{;list}" values ";list=red,green,blue"
      , Test "{;list*}" values ";list=red;list=green;list=blue"
      , Test "{;keys}" values ";keys=comma,%2C,dot,.,semi,%3B"
      , Test "{;keys*}" values ";comma=%2C;dot=.;semi=%3B"
      , Test "{?who}" values "?who=fred"
      , Test "{?half}" values "?half=50%25"
      , Test "{?x,y}" values "?x=1024&y=768"
      , Test "{?x,y,empty}" values "?x=1024&y=768&empty="
      , Test "{?x,y,undef}" values "?x=1024&y=768"
      , Test "{?var:3}" values "?var=val"
      , Test "{?list}" values "?list=red,green,blue"
      , Test "{?list*}" values "?list=red&list=green&list=blue"
      , Test "{?keys}" values "?keys=comma,%2C,dot,.,semi,%3B"
      , Test "{?keys*}" values "?comma=%2C&dot=.&semi=%3B"
      , Test "{&who}" values "&who=fred"
      , Test "{&half}" values "&half=50%25"
      , Test "?fixed=yes{&x}" values "?fixed=yes&x=1024"
      , Test "{&x,y,empty}" values "&x=1024&y=768&empty="
      , Test "{&x,y,undef}" values "&x=1024&y=768"
      , Test "{&var:3}" values "&var=val"
      , Test "{&list}" values "&list=red,green,blue"
      , Test "{&list*}" values "&list=red&list=green&list=blue"
      , Test "{&keys}" values "&keys=comma,%2C,dot,.,semi,%3B"
      , Test "{&keys*}" values "&comma=%2C&dot=.&semi=%3B"
      ]
  ]

data Test = Test
  { testInput :: String
  , testValues :: [(String, Burrito.Value)]
  , testOutput :: Output
  } deriving (Eq, Show)

runTest :: Test -> Hspec.Expectation
runTest test =
  case (Burrito.parse $ testInput test, unwrapOutput $ testOutput test) of
    (Nothing, Nothing) -> pure ()
    (Nothing, Just _) -> Hspec.expectationFailure "should have parsed"
    (Just _, Nothing) -> Hspec.expectationFailure "should not have parsed"
    (Just template, Just expected) -> do
      let
        values = testValues test
        actual = Burrito.expand values template
      actual `Hspec.shouldBe` expected
      Burrito.parse (Burrito.render template) `Hspec.shouldBe` Just template
      let
        relevant =
          List.sort $ keepRelevant (templateVariables template) values
      Monad.when (isMatchable template relevant) $ do
        let matches = List.sort <$> Burrito.match expected template
        matches `Hspec.shouldSatisfy` elem relevant

isMatchable :: Template.Template -> [(String, Burrito.Value)] -> Bool
isMatchable template values =
  (not . any (isAsterisk . Variable.modifier) $ templateVariables template)
    && all (isString . snd) values

isString :: Burrito.Value -> Bool
isString value = case value of
  Value.String _ -> True
  _ -> False

isAsterisk :: Modifier.Modifier -> Bool
isAsterisk modifier = case modifier of
  Modifier.Asterisk -> True
  _ -> False

keepRelevant
  :: Set.Set Variable.Variable
  -> [(String, Burrito.Value)]
  -> [(String, Burrito.Value)]
keepRelevant variables =
  let
    vs =
      Map.fromListWith
          (\mx my -> case (mx, my) of
            (Just x, Just y) -> Just $ max x y
            _ -> Nothing
          )
        . fmap
            (\v ->
              ( Render.builderToString . Render.name $ Variable.name v
              , case Variable.modifier v of
                Modifier.Colon n -> Just $ MaxLength.count n
                _ -> Nothing
              )
            )
        $ Set.toList variables
  in
    Maybe.mapMaybe $ \(k, v) -> do
      m <- Map.lookup k vs
      pure . (,) k $ case m of
        Nothing -> v
        Just n -> case v of
          Value.String t -> Value.String $ Text.take n t
          _ -> v

templateVariables :: Template.Template -> Set.Set Variable.Variable
templateVariables =
  Set.fromList
    . concatMap
        (\token -> case token of
          Token.Expression expression ->
            NonEmpty.toList $ Expression.variables expression
          Token.Literal _ -> []
        )
    . Template.tokens

newtype Output = Output
  { unwrapOutput :: Maybe String
  } deriving (Eq, Show)

instance String.IsString Output where
  fromString = Output . Just

noParse :: Output
noParse = Output Nothing

(=:) :: a -> b -> (a, b)
(=:) = (,)

s :: String -> Burrito.Value
s = Burrito.stringValue

l :: [String] -> Burrito.Value
l = Burrito.listValue

d :: [(String, String)] -> Burrito.Value
d = Burrito.dictionaryValue

newtype Template = Template
  { unwrapTemplate :: Template.Template
  } deriving (Eq)

instance Show Template where
  show (Template template) =
    unwords [show $ Burrito.render template, "{-", show template, "-}"]

instance QC.Arbitrary Template where
  arbitrary = Template <$> arbitraryTemplate
  shrink = fmap Template . shrinkTemplate . unwrapTemplate

type Shrink a = a -> [a]

arbitraryTemplate :: QC.Gen Template.Template
arbitraryTemplate = Template.Template . simplify <$> QC.listOf arbitraryToken

shrinkTemplate :: Shrink Template.Template
shrinkTemplate =
  fmap (Template.Template . simplify)
    . QC.shrinkList shrinkToken
    . Template.tokens

simplify :: [Token.Token] -> [Token.Token]
simplify tokens = case tokens of
  t1 : t2 : ts -> case (t1, t2) of
    (Token.Literal l1, Token.Literal l2) ->
      simplify $ Token.Literal (appendLiteral l1 l2) : ts
    _ -> t1 : simplify (t2 : ts)
  _ -> tokens

appendLiteral :: Literal.Literal -> Literal.Literal -> Literal.Literal
appendLiteral x y =
  Literal.Literal $ Literal.characters x <> Literal.characters y

arbitraryToken :: QC.Gen Token.Token
arbitraryToken = QC.oneof
  [ Token.Expression <$> arbitraryExpression
  , Token.Literal <$> arbitraryLiteral
  ]

shrinkToken :: Shrink Token.Token
shrinkToken x = case x of
  Token.Expression y -> Token.Expression <$> shrinkExpression y
  Token.Literal y -> Token.Literal <$> shrinkLiteral y

arbitraryExpression :: QC.Gen Expression.Expression
arbitraryExpression =
  Expression.Expression
    <$> arbitraryOperator
    <*> arbitraryNonEmpty arbitraryVariable

shrinkExpression :: Shrink Expression.Expression
shrinkExpression x = uncurry Expression.Expression <$> shrinkTuple
  shrinkOperator
  (shrinkNonEmpty shrinkVariable)
  (Expression.operator x, Expression.variables x)

arbitraryOperator :: QC.Gen Operator.Operator
arbitraryOperator = QC.elements
  [ Operator.Ampersand
  , Operator.FullStop
  , Operator.None
  , Operator.NumberSign
  , Operator.PlusSign
  , Operator.QuestionMark
  , Operator.Semicolon
  , Operator.Solidus
  ]

shrinkOperator :: Shrink Operator.Operator
shrinkOperator x = case x of
  Operator.None -> []
  _ -> [Operator.None]

arbitraryNonEmpty :: QC.Gen a -> QC.Gen (NonEmpty.NonEmpty a)
arbitraryNonEmpty g = (NonEmpty.:|) <$> g <*> QC.listOf g

shrinkNonEmpty :: Shrink a -> Shrink (NonEmpty.NonEmpty a)
shrinkNonEmpty f x = uncurry (NonEmpty.:|)
  <$> shrinkTuple f (QC.shrinkList f) (NonEmpty.head x, NonEmpty.tail x)

arbitraryVariable :: QC.Gen Variable.Variable
arbitraryVariable = Variable.Variable <$> arbitraryName <*> arbitraryModifier

shrinkVariable :: Shrink Variable.Variable
shrinkVariable variable = uncurry Variable.Variable <$> shrinkTuple
  shrinkName
  shrinkModifier
  (Variable.name variable, Variable.modifier variable)

shrinkTuple :: Shrink a -> Shrink b -> Shrink (a, b)
shrinkTuple f g (x, y) =
  fmap (\a -> (a, y)) (f x) <> fmap (\b -> (x, b)) (g y)

arbitraryName :: QC.Gen Name.Name
arbitraryName = Name.Name <$> arbitraryNonEmpty arbitraryField

shrinkName :: Shrink Name.Name
shrinkName = fmap Name.Name . shrinkNonEmpty shrinkField . Name.fields

arbitraryField :: QC.Gen Field.Field
arbitraryField = Field.Field <$> arbitraryNonEmpty arbitraryFieldCharacter

shrinkField :: Shrink Field.Field
shrinkField =
  fmap Field.Field . shrinkNonEmpty shrinkFieldCharacter . Field.characters

arbitraryFieldCharacter :: QC.Gen (Character.Character Field.Field)
arbitraryFieldCharacter = QC.oneof
  [ Character.Encoded <$> arbitraryDigit <*> arbitraryDigit
  , Character.Unencoded <$> QC.suchThat QC.arbitrary Parse.isFieldCharacter
  ]

shrinkFieldCharacter :: Shrink (Character.Character Field.Field)
shrinkFieldCharacter x = case x of
  Character.Encoded y z ->
    uncurry Character.Encoded <$> shrinkTuple shrinkDigit shrinkDigit (y, z)
  Character.Unencoded y ->
    fmap Character.Unencoded . filter Parse.isFieldCharacter $ QC.shrink y

arbitraryDigit :: QC.Gen Digit.Digit
arbitraryDigit = QC.oneof
  [ pure Digit.Ox0
  , pure Digit.Ox1
  , pure Digit.Ox2
  , pure Digit.Ox3
  , pure Digit.Ox4
  , pure Digit.Ox5
  , pure Digit.Ox6
  , pure Digit.Ox7
  , pure Digit.Ox8
  , pure Digit.Ox9
  , Digit.OxA <$> arbitraryCase
  , Digit.OxB <$> arbitraryCase
  , Digit.OxC <$> arbitraryCase
  , Digit.OxD <$> arbitraryCase
  , Digit.OxE <$> arbitraryCase
  , Digit.OxF <$> arbitraryCase
  ]

shrinkDigit :: Shrink Digit.Digit
shrinkDigit x = case x of
  Digit.Ox0 -> []
  Digit.Ox1 -> [Digit.Ox0]
  Digit.Ox2 -> [Digit.Ox0]
  Digit.Ox3 -> [Digit.Ox0]
  Digit.Ox4 -> [Digit.Ox0]
  Digit.Ox5 -> [Digit.Ox0]
  Digit.Ox6 -> [Digit.Ox0]
  Digit.Ox7 -> [Digit.Ox0]
  Digit.Ox8 -> [Digit.Ox0]
  Digit.Ox9 -> [Digit.Ox0]
  Digit.OxA y -> Digit.Ox0 : fmap Digit.OxA (shrinkCase y)
  Digit.OxB y -> Digit.Ox0 : fmap Digit.OxA (shrinkCase y)
  Digit.OxC y -> Digit.Ox0 : fmap Digit.OxA (shrinkCase y)
  Digit.OxD y -> Digit.Ox0 : fmap Digit.OxA (shrinkCase y)
  Digit.OxE y -> Digit.Ox0 : fmap Digit.OxA (shrinkCase y)
  Digit.OxF y -> Digit.Ox0 : fmap Digit.OxA (shrinkCase y)

arbitraryCase :: QC.Gen Case.Case
arbitraryCase = QC.elements [Case.Lower, Case.Upper]

shrinkCase :: Shrink Case.Case
shrinkCase x = case x of
  Case.Lower -> []
  Case.Upper -> [Case.Lower]

arbitraryModifier :: QC.Gen Modifier.Modifier
arbitraryModifier = QC.oneof
  [ pure Modifier.Asterisk
  , Modifier.Colon <$> arbitraryMaxLength
  , pure Modifier.None
  ]

shrinkModifier :: Shrink Modifier.Modifier
shrinkModifier x = case x of
  Modifier.Asterisk -> [Modifier.None]
  Modifier.Colon y -> Modifier.None : fmap Modifier.Colon (shrinkMaxLength y)
  Modifier.None -> []

arbitraryMaxLength :: QC.Gen MaxLength.MaxLength
arbitraryMaxLength =
  MaxLength.MaxLength <$> QC.suchThat QC.arbitrary Parse.isMaxLength

shrinkMaxLength :: Shrink MaxLength.MaxLength
shrinkMaxLength =
  fmap MaxLength.MaxLength
    . filter Parse.isMaxLength
    . QC.shrink
    . MaxLength.count

arbitraryLiteral :: QC.Gen Literal.Literal
arbitraryLiteral =
  Literal.Literal <$> arbitraryNonEmpty arbitraryLiteralCharacter

shrinkLiteral :: Shrink Literal.Literal
shrinkLiteral =
  fmap Literal.Literal
    . shrinkNonEmpty shrinkLiteralCharacter
    . Literal.characters

arbitraryLiteralCharacter :: QC.Gen (Character.Character Literal.Literal)
arbitraryLiteralCharacter = QC.oneof
  [ Character.Encoded <$> arbitraryDigit <*> arbitraryDigit
  , Character.Unencoded <$> QC.suchThat QC.arbitrary Parse.isLiteralCharacter
  ]

shrinkLiteralCharacter :: Shrink (Character.Character Literal.Literal)
shrinkLiteralCharacter x = case x of
  Character.Encoded y z ->
    uncurry Character.Encoded <$> shrinkTuple shrinkDigit shrinkDigit (y, z)
  Character.Unencoded y ->
    fmap Character.Unencoded . filter Parse.isLiteralCharacter $ QC.shrink y
