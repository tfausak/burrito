{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main
  )
where

import qualified Burrito
import qualified Burrito.Type.LitChar as LitChar
import qualified Burrito.Type.Expression as Expression
import qualified Burrito.Type.Literal as Literal
import qualified Burrito.Type.Modifier as Modifier
import qualified Burrito.Type.Name as Name
import qualified Burrito.Type.NonEmpty as NonEmpty
import qualified Burrito.Type.Operator as Operator
import qualified Burrito.Type.Template as Template
import qualified Burrito.Type.Token as Token
import qualified Burrito.Type.VarChar as VarChar
import qualified Burrito.Type.Variable as Variable
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified GHC.Exts as Exts
import qualified GHC.Stack as Stack
import qualified Test.Hspec as Test
import qualified Test.QuickCheck as QC

main :: IO ()
main = Test.hspec . Test.describe "Burrito" $ do

  Test.it "round trips" . QC.property $ \ (Template template) ->
    fmap Burrito.render (Burrito.parse (Burrito.render template)) == Just (Burrito.render template)

  Test.it "accepts empty templates" $ do
    test "" [] ""

  Test.it "ignores extra variables" $ do
    test "" ["extra" =: "ignored"] ""

  Test.it "accepts ascii literals" $ do
    test "!" [] "!"
    test "#" [] "#"
    test "$" [] "$"
    test "&" [] "&"
    test "(" [] "("
    test ")" [] ")"
    test "*" [] "*"
    test "+" [] "+"
    test "," [] ","
    test "-" [] "-"
    test "." [] "."
    test "/" [] "/"
    test "0" [] "0"
    test "9" [] "9"
    test ":" [] ":"
    test ";" [] ";"
    test "=" [] "="
    test "?" [] "?"
    test "@" [] "@"
    test "A" [] "A"
    test "Z" [] "Z"
    test "[" [] "["
    test "]" [] "]"
    test "_" [] "_"
    test "a" [] "a"
    test "z" [] "z"
    test "~" [] "~"

  Test.it "accepts unicode literals" $ do
    test "\xa0" [] "%C2%A0"
    test "\xd7ff" [] "%ED%9F%BF"
    test "\xf900" [] "%EF%A4%80"
    test "\xfdcf" [] "%EF%B7%8F"
    test "\xfdf0" [] "%EF%B7%B0"
    test "\xffef" [] "%EF%BF%AF"
    test "\x10000" [] "%F0%90%80%80"
    test "\x1fffd" [] "%F0%9F%BF%BD"
    test "\x20000" [] "%F0%A0%80%80"
    test "\x2fffd" [] "%F0%AF%BF%BD"
    test "\x30000" [] "%F0%B0%80%80"
    test "\x3fffd" [] "%F0%BF%BF%BD"
    test "\x40000" [] "%F1%80%80%80"
    test "\x4fffd" [] "%F1%8F%BF%BD"
    test "\x50000" [] "%F1%90%80%80"
    test "\x5fffd" [] "%F1%9F%BF%BD"
    test "\x60000" [] "%F1%A0%80%80"
    test "\x6fffd" [] "%F1%AF%BF%BD"
    test "\x70000" [] "%F1%B0%80%80"
    test "\x7fffd" [] "%F1%BF%BF%BD"
    test "\x80000" [] "%F2%80%80%80"
    test "\x8fffd" [] "%F2%8F%BF%BD"
    test "\x90000" [] "%F2%90%80%80"
    test "\x9fffd" [] "%F2%9F%BF%BD"
    test "\xa0000" [] "%F2%A0%80%80"
    test "\xafffd" [] "%F2%AF%BF%BD"
    test "\xb0000" [] "%F2%B0%80%80"
    test "\xbfffd" [] "%F2%BF%BF%BD"
    test "\xc0000" [] "%F3%80%80%80"
    test "\xcfffd" [] "%F3%8F%BF%BD"
    test "\xd0000" [] "%F3%90%80%80"
    test "\xdfffd" [] "%F3%9F%BF%BD"
    test "\xe1000" [] "%F3%A1%80%80"
    test "\xefffd" [] "%F3%AF%BF%BD"

  Test.it "accepts private literals" $ do
    test "\xe000" [] "%EE%80%80"
    test "\xf8ff" [] "%EF%A3%BF"
    test "\xf0000" [] "%F3%B0%80%80"
    test "\xffffd" [] "%F3%BF%BF%BD"
    test "\x100000" [] "%F4%80%80%80"
    test "\x10fffd" [] "%F4%8F%BF%BD"

  Test.it "passes percent encoded literals through" $ do
    test "%00" [] "%00"

  Test.it "normalizes percent encodings to uppercase" $ do
    test "%AA" [] "%AA"
    test "%Aa" [] "%AA"
    test "%aA" [] "%AA"
    test "%aa" [] "%AA"

  Test.it "rejects invalid percent encodings" $ do
    test "%" [] Failure
    test "%0" [] Failure
    test "%0z" [] Failure
    test "%z" [] Failure

  Test.it "does not decode percent encoded literals" $ do
    test "%30" [] "%30"

  Test.it "rejects invalid literals" $ do
    test " " [] Failure
    test "\"" [] Failure
    test "'" [] Failure
    test "%" [] Failure
    test "<" [] Failure
    test ">" [] Failure
    test "\\" [] Failure
    test "^" [] Failure
    test "`" [] Failure
    test "{" [] Failure
    test "|" [] Failure
    test "}" [] Failure

  Test.it "rejects empty variable names" $ do
    test "{}" [] Failure
    test "{,}" [] Failure
    test "{a,,b}" [] Failure
    test "{+}" [] Failure
    test "{:1}" [] Failure
    test "{*}" [] Failure

  Test.it "accepts uppercase variable names" $ do
    test "{AZ}" [] ""

  Test.it "accepts lowercase variable names" $ do
    test "{az}" [] ""

  Test.it "accepts decimal variable names" $ do
    test "{09}" [] ""

  Test.it "accepts underscores in variable names" $ do
    test "{_a}" [] ""
    test "{a_}" [] ""
    test "{_}" [] ""

  Test.it "accepts dots in variable names" $ do
    test "{A.A}" [] ""
    test "{a.a}" [] ""
    test "{0.0}" [] ""
    test "{_._}" [] ""
    test "{%aa.%aa}" [] ""

  Test.it "rejects invalid dots in variable names" $ do
    test "{.}" [] Failure
    test "{a.}" [] Failure
    test "{+.a}" [] Failure
    test "{a..b}" [] Failure

  Test.it "accepts percent encoded variable names" $ do
    test "{%00}" [] ""

  -- It's unclear if percent encoded triplets in variable names should be case
  -- sensitive or not. Section 2.3 says: "Variable names are case-sensitive
  -- because the name might be expanded within a case-sensitive URI component."
  -- But the HEXDIG rule in section 1.5 says: "; case-insensitive". I think
  -- it's safe to assume they are case sensitive because they can appear in an
  -- expansion. For example:
  --
  --    render "{;%aa}" [("%aa", "A")] ==> ";%aa=A"
  --
  -- However percent encoded triplets that only differ by case would be decoded
  -- into the same octet anyway.
  Test.it "does not normalize percent encoded variable names" $ do
    test "{%AA}" ["%AA" =: "upper-upper"] "upper-upper"
    test "{%Aa}" ["%Aa" =: "upper-lower"] "upper-lower"
    test "{%aA}" ["%aA" =: "lower-upper"] "lower-upper"
    test "{%aa}" ["%aa" =: "lower-lower"] "lower-lower"

  Test.it "rejects invalid percent encoded variable names" $ do
    test "{%}" [] Failure
    test "{%0}" [] Failure
    test "{%0z}" [] Failure
    test "{%z}" [] Failure

  Test.it "rejects invalid variable names" $ do
    test "{!}" [] Failure

  Test.it "rejects invalid expressions" $ do
    test "{" [] Failure
    test "{{}" [] Failure
    test "}" [] Failure
    test "{}}" [] Failure

  Test.it "accepts multiple variables in one expression" $ do
    test "{a,b}" [] ""
    test "{a,b,c,d}" [] ""
    test "{a,a}" [] ""

  Test.it "accepts prefix modifiers" $ do
    test "{a:5}" [] ""
    test "{a:67}" [] ""
    test "{a:801}" [] ""
    test "{a:234}" [] ""
    test "{a:9999}" [] ""

  Test.it "applies prefix modifiers" $ do
    let values = ["a" =: "abcdefghijklmnopqrstuvwxyz"] :: Values
    test "{a:1}" values "a"
    test "{a:5}" values "abcde"
    test "{a:10}" values "abcdefghij"
    test "{a:15}" values "abcdefghijklmno"
    test "{a:20}" values "abcdefghijklmnopqrst"
    test "{a:25}" values "abcdefghijklmnopqrstuvwxy"
    test "{a:30}" values "abcdefghijklmnopqrstuvwxyz"

  Test.it "rejects invalid prefix modifiers" $ do
    test "{a:}" [] Failure
    test "{a:0}" [] Failure
    test "{a:10000}" [] Failure
    test "{a:-1}" [] Failure

  Test.it "accepts explode modifiers" $ do
    test "{a*}" [] ""

  Test.it "rejects both prefix and explode modifiers" $ do
    test "{a:1*}" [] Failure
    test "{a*:1}" [] Failure

  Test.it "accepts different modifiers on different variables" $ do
    test "{a,b:1,c*}" [] ""

  Test.it "accepts allowed operators" $ do
    test "{+a}" [] ""
    test "{#a}" [] ""
    test "{.a}" [] ""
    test "{/a}" [] ""
    test "{;a}" [] ""
    test "{?a}" [] ""
    test "{&a}" [] ""

  Test.it "rejects reserved operators" $ do
    test "{=a}" [] Failure
    test "{,a}" [] Failure
    test "{!a}" [] Failure
    test "{@a}" [] Failure
    test "{|a}" [] Failure

  Test.it "rejects multiple operators" $ do
    test "{+#a}" [] Failure

  Test.it "rejects different operators for different variables" $ do
    test "{+a,#b}" [] Failure

  Test.it "accepts operators and modifiers" $ do
    test "{+a:1}" [] ""
    test "{#a*}" [] ""

  Test.it "accepts multiple variables with an operator" $ do
    test "{+a,b}" [] ""
    test "{#a,b}" [] ""
    test "{.a,b}" [] ""
    test "{/a,b}" [] ""
    test "{;a,b}" [] ""
    test "{?a,b}" [] ""
    test "{&a,b}" [] ""

  Test.it "accepts multiple expressions" $ do
    test "{a}{b}" [] ""
    test "{a}{b}{c}{d}" [] ""
    test "{a}{a}" [] ""

  Test.it "rejects nested expressions" $ do
    test "{{}}" [] Failure
    test "{a{b}}" [] Failure
    test "{{a}b}" [] Failure
    test "{a{b}c}" [] Failure

  Test.it "accepts literals and expressions together" $ do
    test "a{b}" [] "a"
    test "{a}b" [] "b"
    test "a{b}c" [] "ac"
    test "{a}b{c}" [] "b"

  Test.it "handles missing values" $ do
    test "{a}" [] ""
    test "{+a}" [] ""
    test "{#a}" [] ""
    test "{.a}" [] ""
    test "{/a}" [] ""
    test "{;a}" [] ""
    test "{?a}" [] ""
    test "{&a}" [] ""

  Test.it "handles empty list values" $ do
    let values = ["a" =: emptyList] :: Values
    test "{a}" values ""
    test "{+a}" values ""
    test "{#a}" values ""
    test "{.a}" values ""
    test "{/a}" values ""
    test "{;a}" values ""
    test "{?a}" values ""
    test "{&a}" values ""

  Test.it "handles empty dictionary values" $ do
    let values = ["a" =: emptyDictionary] :: Values
    test "{a}" values ""
    test "{+a}" values ""
    test "{#a}" values ""
    test "{.a}" values ""
    test "{/a}" values ""
    test "{;a}" values ""
    test "{?a}" values ""
    test "{&a}" values ""

  Test.it "handles empty string values" $ do
    let values = ["a" =: ""] :: Values
    test "{a}" values ""
    test "{+a}" values ""
    test "{#a}" values "#"
    test "{.a}" values "."
    test "{/a}" values "/"
    test "{;a}" values ";a"
    test "{?a}" values "?a="
    test "{&a}" values "&a="

  Test.it "handles nonempty string values" $ do
    let values = ["a" =: "A"] :: Values
    test "{a}" values "A"
    test "{+a}" values "A"
    test "{#a}" values "#A"
    test "{.a}" values ".A"
    test "{/a}" values "/A"
    test "{;a}" values ";a=A"
    test "{?a}" values "?a=A"
    test "{&a}" values "&a=A"

  Test.it "handles a mix of defined and undefined values" $ do
    let values = ["b" =: "B"] :: Values
    test "{a,b}" values "B"
    test "{+a,b}" values "B"
    test "{#a,b}" values "#B"
    test "{.a,b}" values ".B"
    test "{/a,b}" values "/B"
    test "{;a,b}" values ";b=B"
    test "{?a,b}" values "?b=B"
    test "{&a,b}" values "&b=B"

  Test.it "handles multiple empty string values" $ do
    let values = ["a" =: ""] :: Values
    test "{a,a}" values ","
    test "{+a,a}" values ","
    test "{#a,a}" values "#,"
    test "{.a,a}" values ".."
    test "{/a,a}" values "//"
    test "{;a,a}" values ";a;a"
    test "{?a,a}" values "?a=&a="
    test "{&a,a}" values "&a=&a="

  Test.it "handles multiple non-empty string values" $ do
    let values = ["a" =: "A", "b" =: "B"] :: Values
    test "{a,b}" values "A,B"
    test "{+a,b}" values "A,B"
    test "{#a,b}" values "#A,B"
    test "{.a,b}" values ".A.B"
    test "{/a,b}" values "/A/B"
    test "{;a,b}" values ";a=A;b=B"
    test "{?a,b}" values "?a=A&b=B"
    test "{&a,b}" values "&a=A&b=B"

  Test.it "escapes characters in composite dictionaries" $ do
    let values = ["a" =: ["K! \xa0\xd7ff\x10000" =: "V! \xa0\xd7ff\x10000"]] :: Values
    test "{a*}" values "K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
    test "{+a*}" values "K!%20%C2%A0%ED%9F%BF%F0%90%80%80=V!%20%C2%A0%ED%9F%BF%F0%90%80%80"
    test "{#a*}" values "#K!%20%C2%A0%ED%9F%BF%F0%90%80%80=V!%20%C2%A0%ED%9F%BF%F0%90%80%80"
    test "{.a*}" values ".K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
    test "{/a*}" values "/K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
    test "{;a*}" values ";K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
    test "{?a*}" values "?K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"
    test "{&a*}" values "&K%21%20%C2%A0%ED%9F%BF%F0%90%80%80=V%21%20%C2%A0%ED%9F%BF%F0%90%80%80"

  Test.it "prefers the first variable" $ do
    test "{a}" ["a" =: "A", "a" =: "B"] "A"
    test "{a}" ["a" =: "B", "a" =: "A"] "B"

  Test.describe "passes test from rfc" $ do

    Test.it "section 1.1" $ do
      test "http://example.com/~{username}/" ["username" =: "fred"] "http://example.com/~fred/"
      test "http://example.com/~{username}/" ["username" =: "mark"] "http://example.com/~mark/"
      test "http://example.com/dictionary/{term:1}/{term}" ["term" =: "cat"] "http://example.com/dictionary/c/cat"
      test "http://example.com/dictionary/{term:1}/{term}" ["term" =: "dog"] "http://example.com/dictionary/d/dog"
      test "http://example.com/search{?q,lang}" ["q" =: "cat", "lang" =: "en"] "http://example.com/search?q=cat&lang=en"
      test "http://example.com/search{?q,lang}" ["q" =: "chien", "lang" =: "fr"] "http://example.com/search?q=chien&lang=fr"
      test "http://www.example.com/foo{?query,number}" ["query" =: "mycelium", "number" =: "100"] "http://www.example.com/foo?query=mycelium&number=100"
      test "http://www.example.com/foo{?query,number}" ["number" =: "100"] "http://www.example.com/foo?number=100"
      test "http://www.example.com/foo{?query,number}" [] "http://www.example.com/foo"

    Test.describe "section 1.2" $ do
      let
        values =
          [ "empty" =: ""
          , "hello" =: "Hello World!"
          , "keys" =: ["semi" =: ";", "dot" =: ".", "comma" =: ","]
          , "list" =: ["red", "green", "blue"]
          , "path" =: "/foo/bar"
          , "var" =: "value"
          , "x" =: "1024"
          , "y" =: "768"
          ] :: Values

      Test.it "level 1" $ do
        test "{var}" values "value"
        test "{hello}" values "Hello%20World%21"

      Test.it "level 2" $ do
        test "{+var}" values "value"
        test "{+hello}" values "Hello%20World!"
        test "{+path}/here" values "/foo/bar/here"
        test "here?ref={+path}" values "here?ref=/foo/bar"
        test "X{#var}" values "X#value"
        test "X{#hello}" values "X#Hello%20World!"

      Test.it "level 3" $ do
        test "map?{x,y}" values "map?1024,768"
        test "{x,hello,y}" values "1024,Hello%20World%21,768"
        test "{+x,hello,y}" values "1024,Hello%20World!,768"
        test "{+path,x}/here" values "/foo/bar,1024/here"
        test "{#x,hello,y}" values "#1024,Hello%20World!,768"
        test "{#path,x}/here" values "#/foo/bar,1024/here"
        test "X{.var}" values "X.value"
        test "X{.x,y}" values "X.1024.768"
        test "{/var}" values "/value"
        test "{/var,x}/here" values "/value/1024/here"
        test "{;x,y}" values ";x=1024;y=768"
        test "{;x,y,empty}" values ";x=1024;y=768;empty"
        test "{?x,y}" values "?x=1024&y=768"
        test "{?x,y,empty}" values "?x=1024&y=768&empty="
        test "?fixed=yes{&x}" values "?fixed=yes&x=1024"
        test "{&x,y,empty}" values "&x=1024&y=768&empty="

      Test.it "level 4" $ do
        test "{var:3}" values "val"
        test "{var:30}" values "value"
        test "{list}" values "red,green,blue"
        test "{list*}" values "red,green,blue"
        test "{keys}" values "semi,%3B,dot,.,comma,%2C"
        test "{keys*}" values "semi=%3B,dot=.,comma=%2C"
        test "{+path:6}/here" values "/foo/b/here"
        test "{+list}" values "red,green,blue"
        test "{+list*}" values "red,green,blue"
        test "{+keys}" values "semi,;,dot,.,comma,,"
        test "{+keys*}" values "semi=;,dot=.,comma=,"
        test "{#path:6}/here" values "#/foo/b/here"
        test "{#list}" values "#red,green,blue"
        test "{#list*}" values "#red,green,blue"
        test "{#keys}" values "#semi,;,dot,.,comma,,"
        test "{#keys*}" values "#semi=;,dot=.,comma=,"
        test "X{.var:3}" values "X.val"
        test "X{.list}" values "X.red,green,blue"
        test "X{.list*}" values "X.red.green.blue"
        test "X{.keys}" values "X.semi,%3B,dot,.,comma,%2C"
        test "X{.keys*}" values "X.semi=%3B.dot=..comma=%2C"
        test "{/var:1,var}" values "/v/value"
        test "{/list}" values "/red,green,blue"
        test "{/list*}" values "/red/green/blue"
        test "{/list*,path:4}" values "/red/green/blue/%2Ffoo"
        test "{/keys}" values "/semi,%3B,dot,.,comma,%2C"
        test "{/keys*}" values "/semi=%3B/dot=./comma=%2C"
        test "{;hello:5}" values ";hello=Hello"
        test "{;list}" values ";list=red,green,blue"
        test "{;list*}" values ";list=red;list=green;list=blue"
        test "{;keys}" values ";keys=semi,%3B,dot,.,comma,%2C"
        test "{;keys*}" values ";semi=%3B;dot=.;comma=%2C"
        test "{?var:3}" values "?var=val"
        test "{?list}" values "?list=red,green,blue"
        test "{?list*}" values "?list=red&list=green&list=blue"
        test "{?keys}" values "?keys=semi,%3B,dot,.,comma,%2C"
        test "{?keys*}" values "?semi=%3B&dot=.&comma=%2C"
        test "{&var:3}" values "&var=val"
        test "{&list}" values "&list=red,green,blue"
        test "{&list*}" values "&list=red&list=green&list=blue"
        test "{&keys}" values "&keys=semi,%3B,dot,.,comma,%2C"
        test "{&keys*}" values "&semi=%3B&dot=.&comma=%2C"

    Test.it "section 2.4.1" $ do
      let values = ["var" =: "value", "semi" =: ";"] :: Values
      test "{var}" values "value"
      test "{var:20}" values "value"
      test "{var:3}" values "val"
      test "{semi}" values "%3B"
      test "{semi:2}" values "%3B"

    Test.it "section 2.4.2" $ do
      let values = ["year" =: ["1965", "2000", "2012"], "dom" =: ["example", "com"]] :: Values
      test "find{?year*}" values "find?year=1965&year=2000&year=2012"
      test "www{.dom*}" values "www.example.com"

    Test.describe "section 3.1" $ do
      let
        values =
          [ "base" =: "http://example.com/home/"
          , "count" =: ["one", "two", "three"]
          , "dom" =: ["example", "com"]
          , "dub" =: "me/too"
          , "empty_keys" =: emptyDictionary
          , "empty" =: ""
          , "half" =: "50%"
          , "hello" =: "Hello World!"
          , "keys" =: ["semi" =: ";", "dot" =: ".", "comma" =: ","]
          , "list" =: ["red", "green", "blue"]
          , "path" =: "/foo/bar"
          , "v" =: "6"
          , "var" =: "value"
          , "who" =: "fred"
          , "x" =: "1024"
          , "y" =: "768"
          ] :: Values

      Test.it "subsection 1" $ do
        test "{count}" values "one,two,three"
        test "{count*}" values "one,two,three"
        test "{/count}" values "/one,two,three"
        test "{/count*}" values "/one/two/three"
        test "{;count}" values ";count=one,two,three"
        test "{;count*}" values ";count=one;count=two;count=three"
        test "{?count}" values "?count=one,two,three"
        test "{?count*}" values "?count=one&count=two&count=three"
        test "{&count*}" values "&count=one&count=two&count=three"

      Test.it "subsection 2" $ do
        test "{var}" values "value"
        test "{hello}" values "Hello%20World%21"
        test "{half}" values "50%25"
        test "O{empty}X" values "OX"
        test "O{undef}X" values "OX"
        test "{x,y}" values "1024,768"
        test "{x,hello,y}" values "1024,Hello%20World%21,768"
        test "?{x,empty}" values "?1024,"
        test "?{x,undef}" values "?1024"
        test "?{undef,y}" values "?768"
        test "{var:3}" values "val"
        test "{var:30}" values "value"
        test "{list}" values "red,green,blue"
        test "{list*}" values "red,green,blue"
        test "{keys}" values "semi,%3B,dot,.,comma,%2C"
        test "{keys*}" values "semi=%3B,dot=.,comma=%2C"

      Test.it "subsection 3" $ do
        test "{+var}" values "value"
        test "{+hello}" values "Hello%20World!"
        test "{+half}" values "50%25"
        test "{base}index" values "http%3A%2F%2Fexample.com%2Fhome%2Findex"
        test "{+base}index" values "http://example.com/home/index"
        test "O{+empty}X" values "OX"
        test "O{+undef}X" values "OX"
        test "{+path}/here" values "/foo/bar/here"
        test "here?ref={+path}" values "here?ref=/foo/bar"
        test "up{+path}{var}/here" values "up/foo/barvalue/here"
        test "{+x,hello,y}" values "1024,Hello%20World!,768"
        test "{+path,x}/here" values "/foo/bar,1024/here"
        test "{+path:6}/here" values "/foo/b/here"
        test "{+list}" values "red,green,blue"
        test "{+list*}" values "red,green,blue"
        test "{+keys}" values "semi,;,dot,.,comma,,"
        test "{+keys*}" values "semi=;,dot=.,comma=,"

      Test.it "subsection 4" $ do
        test "{#var}" values "#value"
        test "{#hello}" values "#Hello%20World!"
        test "{#half}" values "#50%25"
        test "foo{#empty}" values "foo#"
        test "foo{#undef}" values "foo"
        test "{#x,hello,y}" values "#1024,Hello%20World!,768"
        test "{#path,x}/here" values "#/foo/bar,1024/here"
        test "{#path:6}/here" values "#/foo/b/here"
        test "{#list}" values "#red,green,blue"
        test "{#list*}" values "#red,green,blue"
        test "{#keys}" values "#semi,;,dot,.,comma,,"
        test "{#keys*}" values "#semi=;,dot=.,comma=,"

      Test.it "subsection 5" $ do
        test "{.who}" values ".fred"
        test "{.who,who}" values ".fred.fred"
        test "{.half,who}" values ".50%25.fred"
        test "www{.dom*}" values "www.example.com"
        test "X{.var}" values "X.value"
        test "X{.empty}" values "X."
        test "X{.undef}" values "X"
        test "X{.var:3}" values "X.val"
        test "X{.list}" values "X.red,green,blue"
        test "X{.list*}" values "X.red.green.blue"
        test "X{.keys}" values "X.semi,%3B,dot,.,comma,%2C"
        test "X{.keys*}" values "X.semi=%3B.dot=..comma=%2C"
        test "X{.empty_keys}" values "X"
        test "X{.empty_keys*}" values "X"

      Test.it "subsection 6" $ do
        test "{/who}" values "/fred"
        test "{/who,who}" values "/fred/fred"
        test "{/half,who}" values "/50%25/fred"
        test "{/who,dub}" values "/fred/me%2Ftoo"
        test "{/var}" values "/value"
        test "{/var,empty}" values "/value/"
        test "{/var,undef}" values "/value"
        test "{/var,x}/here" values "/value/1024/here"
        test "{/var:1,var}" values "/v/value"
        test "{/list}" values "/red,green,blue"
        test "{/list*}" values "/red/green/blue"
        test "{/list*,path:4}" values "/red/green/blue/%2Ffoo"
        test "{/keys}" values "/semi,%3B,dot,.,comma,%2C"
        test "{/keys*}" values "/semi=%3B/dot=./comma=%2C"

      Test.it "subsection 7" $ do
        test "{;who}" values ";who=fred"
        test "{;half}" values ";half=50%25"
        test "{;empty}" values ";empty"
        test "{;v,empty,who}" values ";v=6;empty;who=fred"
        test "{;v,bar,who}" values ";v=6;who=fred"
        test "{;x,y}" values ";x=1024;y=768"
        test "{;x,y,empty}" values ";x=1024;y=768;empty"
        test "{;x,y,undef}" values ";x=1024;y=768"
        test "{;hello:5}" values ";hello=Hello"
        test "{;list}" values ";list=red,green,blue"
        test "{;list*}" values ";list=red;list=green;list=blue"
        test "{;keys}" values ";keys=semi,%3B,dot,.,comma,%2C"
        test "{;keys*}" values ";semi=%3B;dot=.;comma=%2C"

      Test.it "subsection 8" $ do
        test "{?who}" values "?who=fred"
        test "{?half}" values "?half=50%25"
        test "{?x,y}" values "?x=1024&y=768"
        test "{?x,y,empty}" values "?x=1024&y=768&empty="
        test "{?x,y,undef}" values "?x=1024&y=768"
        test "{?var:3}" values "?var=val"
        test "{?list}" values "?list=red,green,blue"
        test "{?list*}" values "?list=red&list=green&list=blue"
        test "{?keys}" values "?keys=semi,%3B,dot,.,comma,%2C"
        test "{?keys*}" values "?semi=%3B&dot=.&comma=%2C"

      Test.it "subsection 9" $ do
        test "{&who}" values "&who=fred"
        test "{&half}" values "&half=50%25"
        test "?fixed=yes{&x}" values "?fixed=yes&x=1024"
        test "{&x,y,empty}" values "&x=1024&y=768&empty="
        test "{&x,y,undef}" values "&x=1024&y=768"
        test "{&var:3}" values "&var=val"
        test "{&list}" values "&list=red,green,blue"
        test "{&list*}" values "&list=red&list=green&list=blue"
        test "{&keys}" values "&keys=semi,%3B,dot,.,comma,%2C"
        test "{&keys*}" values "&semi=%3B&dot=.&comma=%2C"

  Test.it "handles simple expansion" $ do
    test "{a}" [] ""
    test "{a}" ["a" =: emptyList] ""
    test "{a}" ["a" =: emptyDictionary] ""
    test "{a}" ["a" =: ""] ""
    test "{a}" ["a" =: "A"] "A"
    test "{a}" ["a" =: "~"] "~"
    test "{a}" ["a" =: "%"] "%25"
    test "{a}" ["a" =: "?"] "%3F"
    test "{a}" ["a" =: "&"] "%26"
    test "{a}" ["a" =: "\xa0"] "%C2%A0"
    test "{a}" ["a" =: "\xd7ff"] "%ED%9F%BF"
    test "{a}" ["a" =: "\x10000"] "%F0%90%80%80"
    test "{a}" ["a" =: ["A"]] "A"
    test "{a}" ["a" =: ["A", "B"]] "A,B"
    test "{a}" ["a" =: ["%"]] "%25"
    test "{a}" ["a" =: ["\xa0"]] "%C2%A0"
    test "{a}" ["a" =: ["A" =: "1"]] "A,1"
    test "{a}" ["a" =: ["A" =: "1", "B" =: "2"]] "A,1,B,2"
    test "{a}" ["a" =: ["A" =: "%"]] "A,%25"
    test "{a}" ["a" =: ["A" =: "\xa0"]] "A,%C2%A0"
    test "{a}" ["a" =: ["%" =: "1"]] "%25,1"
    test "{a*}" [] ""
    test "{a*}" ["a" =: ""] ""
    test "{a*}" ["a" =: "A"] "A"
    test "{a*}" ["a" =: emptyList] ""
    test "{a*}" ["a" =: ["A"]] "A"
    test "{a*}" ["a" =: ["A", "B"]] "A,B"
    test "{a*}" ["a" =: emptyDictionary] ""
    test "{a*}" ["a" =: ["A" =: "1"]] "A=1"
    test "{a*}" ["a" =: ["A" =: "1", "B" =: "2"]] "A=1,B=2"
    test "{a:1}" [] ""
    test "{a:1}" ["a" =: ""] ""
    test "{a:1}" ["a" =: "A"] "A"
    test "{a:1}" ["a" =: "AB"] "A"
    test "{a:1}" ["a" =: "%B"] "%25"
    test "{a:1}" ["a" =: "\xa0\&B"] "%C2%A0"
    test "{a:1}" ["a" =: "\xd7ff\&B"] "%ED%9F%BF"
    test "{a:1}" ["a" =: "\x10000\&B"] "%F0%90%80%80"
    test "{a:1}" ["a" =: emptyList] ""
    test "{a:1}" ["a" =: ["AB"]] "AB"
    test "{a:1}" ["a" =: ["AB", "CD"]] "AB,CD"
    test "{a:1}" ["a" =: emptyDictionary] ""
    test "{a:1}" ["a" =: ["AB" =: "12"]] "AB,12"
    test "{a:1}" ["a" =: ["AB" =: "12", "CD" =: "34"]] "AB,12,CD,34"
    test "{a,a}" [] ""
    test "{a,a}" ["a" =: emptyList] ""
    test "{a,a}" ["a" =: emptyDictionary] ""
    test "{a,a}" ["a" =: ""] ","
    test "{a,b}" ["a" =: ""] ""
    test "{a,b}" ["b" =: ""] ""
    test "{%aa}" ["%aa" =: "A"] "A"
    test "{%aa}" ["%aa" =: ["A", "B"]] "A,B"
    test "{%aa}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "A,1,B,2"
    test "{%aa*}" ["%aa" =: "A"] "A"
    test "{%aa*}" ["%aa" =: ["A", "B"]] "A,B"
    test "{%aa*}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "A=1,B=2"

  Test.it "handles reserved expansion" $ do
    test "{+a}" [] ""
    test "{+a}" ["a" =: emptyList] ""
    test "{+a}" ["a" =: emptyDictionary] ""
    test "{+a}" ["a" =: ""] ""
    test "{+a}" ["a" =: "A"] "A"
    test "{+a}" ["a" =: "~"] "~"
    test "{+a}" ["a" =: "%"] "%25"
    test "{+a}" ["a" =: "?"] "?"
    test "{+a}" ["a" =: "&"] "&"
    test "{+a}" ["a" =: "\xa0"] "%C2%A0"
    test "{+a}" ["a" =: "\xd7ff"] "%ED%9F%BF"
    test "{+a}" ["a" =: "\x10000"] "%F0%90%80%80"
    test "{+a}" ["a" =: ["A"]] "A"
    test "{+a}" ["a" =: ["A", "B"]] "A,B"
    test "{+a}" ["a" =: ["%"]] "%25"
    test "{+a}" ["a" =: ["\xa0"]] "%C2%A0"
    test "{+a}" ["a" =: ["A" =: "1"]] "A,1"
    test "{+a}" ["a" =: ["A" =: "1", "B" =: "2"]] "A,1,B,2"
    test "{+a}" ["a" =: ["A" =: "%"]] "A,%25"
    test "{+a}" ["a" =: ["A" =: "\xa0"]] "A,%C2%A0"
    test "{+a}" ["a" =: ["%" =: "1"]] "%25,1"
    test "{+a*}" [] ""
    test "{+a*}" ["a" =: ""] ""
    test "{+a*}" ["a" =: "A"] "A"
    test "{+a*}" ["a" =: emptyList] ""
    test "{+a*}" ["a" =: ["A"]] "A"
    test "{+a*}" ["a" =: ["A", "B"]] "A,B"
    test "{+a*}" ["a" =: emptyDictionary] ""
    test "{+a*}" ["a" =: ["A" =: "1"]] "A=1"
    test "{+a*}" ["a" =: ["A" =: "1", "B" =: "2"]] "A=1,B=2"
    test "{+a:1}" [] ""
    test "{+a:1}" ["a" =: ""] ""
    test "{+a:1}" ["a" =: "A"] "A"
    test "{+a:1}" ["a" =: "AB"] "A"
    test "{+a:1}" ["a" =: "%B"] "%25"
    test "{+a:1}" ["a" =: "\xa0\&B"] "%C2%A0"
    test "{+a:1}" ["a" =: "\xd7ff\&B"] "%ED%9F%BF"
    test "{+a:1}" ["a" =: "\x10000\&B"] "%F0%90%80%80"
    test "{+a:1}" ["a" =: emptyList] ""
    test "{+a:1}" ["a" =: ["AB"]] "AB"
    test "{+a:1}" ["a" =: ["AB", "CD"]] "AB,CD"
    test "{+a:1}" ["a" =: emptyDictionary] ""
    test "{+a:1}" ["a" =: ["AB" =: "12"]] "AB,12"
    test "{+a:1}" ["a" =: ["AB" =: "12", "CD" =: "34"]] "AB,12,CD,34"
    test "{+a,a}" [] ""
    test "{+a,a}" ["a" =: emptyList] ""
    test "{+a,a}" ["a" =: emptyDictionary] ""
    test "{+a,a}" ["a" =: ""] ","
    test "{+a,b}" ["a" =: ""] ""
    test "{+a,b}" ["b" =: ""] ""
    test "{+%aa}" ["%aa" =: "A"] "A"
    test "{+%aa}" ["%aa" =: ["A", "B"]] "A,B"
    test "{+%aa}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "A,1,B,2"
    test "{+%aa*}" ["%aa" =: "A"] "A"
    test "{+%aa*}" ["%aa" =: ["A", "B"]] "A,B"
    test "{+%aa*}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "A=1,B=2"

  Test.it "handles fragment expansion" $ do
    test "{#a}" [] ""
    test "{#a}" ["a" =: emptyList] ""
    test "{#a}" ["a" =: emptyDictionary] ""
    test "{#a}" ["a" =: ""] "#"
    test "{#a}" ["a" =: "A"] "#A"
    test "{#a}" ["a" =: "~"] "#~"
    test "{#a}" ["a" =: "%"] "#%25"
    test "{#a}" ["a" =: "?"] "#?"
    test "{#a}" ["a" =: "&"] "#&"
    test "{#a}" ["a" =: "\xa0"] "#%C2%A0"
    test "{#a}" ["a" =: "\xd7ff"] "#%ED%9F%BF"
    test "{#a}" ["a" =: "\x10000"] "#%F0%90%80%80"
    test "{#a}" ["a" =: ["A"]] "#A"
    test "{#a}" ["a" =: ["A", "B"]] "#A,B"
    test "{#a}" ["a" =: ["%"]] "#%25"
    test "{#a}" ["a" =: ["\xa0"]] "#%C2%A0"
    test "{#a}" ["a" =: ["A" =: "1"]] "#A,1"
    test "{#a}" ["a" =: ["A" =: "1", "B" =: "2"]] "#A,1,B,2"
    test "{#a}" ["a" =: ["A" =: "%"]] "#A,%25"
    test "{#a}" ["a" =: ["A" =: "\xa0"]] "#A,%C2%A0"
    test "{#a}" ["a" =: ["%" =: "1"]] "#%25,1"
    test "{#a*}" [] ""
    test "{#a*}" ["a" =: ""] "#"
    test "{#a*}" ["a" =: "A"] "#A"
    test "{#a*}" ["a" =: emptyList] ""
    test "{#a*}" ["a" =: ["A"]] "#A"
    test "{#a*}" ["a" =: ["A", "B"]] "#A,B"
    test "{#a*}" ["a" =: emptyDictionary] ""
    test "{#a*}" ["a" =: ["A" =: "1"]] "#A=1"
    test "{#a*}" ["a" =: ["A" =: "1", "B" =: "2"]] "#A=1,B=2"
    test "{#a:1}" [] ""
    test "{#a:1}" ["a" =: ""] "#"
    test "{#a:1}" ["a" =: "A"] "#A"
    test "{#a:1}" ["a" =: "AB"] "#A"
    test "{#a:1}" ["a" =: "%B"] "#%25"
    test "{#a:1}" ["a" =: "\xa0\&B"] "#%C2%A0"
    test "{#a:1}" ["a" =: "\xd7ff\&B"] "#%ED%9F%BF"
    test "{#a:1}" ["a" =: "\x10000\&B"] "#%F0%90%80%80"
    test "{#a:1}" ["a" =: emptyList] ""
    test "{#a:1}" ["a" =: ["AB"]] "#AB"
    test "{#a:1}" ["a" =: ["AB", "CD"]] "#AB,CD"
    test "{#a:1}" ["a" =: emptyDictionary] ""
    test "{#a:1}" ["a" =: ["AB" =: "12"]] "#AB,12"
    test "{#a:1}" ["a" =: ["AB" =: "12", "CD" =: "34"]] "#AB,12,CD,34"
    test "{#a,a}" [] ""
    test "{#a,a}" ["a" =: emptyList] ""
    test "{#a,a}" ["a" =: emptyDictionary] ""
    test "{#a,a}" ["a" =: ""] "#,"
    test "{#a,b}" ["a" =: ""] "#"
    test "{#a,b}" ["b" =: ""] "#"
    test "{#%aa}" ["%aa" =: "A"] "#A"
    test "{#%aa}" ["%aa" =: ["A", "B"]] "#A,B"
    test "{#%aa}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "#A,1,B,2"
    test "{#%aa*}" ["%aa" =: "A"] "#A"
    test "{#%aa*}" ["%aa" =: ["A", "B"]] "#A,B"
    test "{#%aa*}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "#A=1,B=2"

  Test.it "handles Test.it expansion" $ do
    test "{.a}" [] ""
    test "{.a}" ["a" =: emptyList] ""
    test "{.a}" ["a" =: emptyDictionary] ""
    test "{.a}" ["a" =: ""] "."
    test "{.a}" ["a" =: "A"] ".A"
    test "{.a}" ["a" =: "~"] ".~"
    test "{.a}" ["a" =: "%"] ".%25"
    test "{.a}" ["a" =: "?"] ".%3F"
    test "{.a}" ["a" =: "&"] ".%26"
    test "{.a}" ["a" =: "\xa0"] ".%C2%A0"
    test "{.a}" ["a" =: "\xd7ff"] ".%ED%9F%BF"
    test "{.a}" ["a" =: "\x10000"] ".%F0%90%80%80"
    test "{.a}" ["a" =: ["A"]] ".A"
    test "{.a}" ["a" =: ["A", "B"]] ".A,B"
    test "{.a}" ["a" =: ["%"]] ".%25"
    test "{.a}" ["a" =: ["\xa0"]] ".%C2%A0"
    test "{.a}" ["a" =: ["A" =: "1"]] ".A,1"
    test "{.a}" ["a" =: ["A" =: "1", "B" =: "2"]] ".A,1,B,2"
    test "{.a}" ["a" =: ["A" =: "%"]] ".A,%25"
    test "{.a}" ["a" =: ["A" =: "\xa0"]] ".A,%C2%A0"
    test "{.a}" ["a" =: ["%" =: "1"]] ".%25,1"
    test "{.a*}" [] ""
    test "{.a*}" ["a" =: ""] "."
    test "{.a*}" ["a" =: "A"] ".A"
    test "{.a*}" ["a" =: emptyList] ""
    test "{.a*}" ["a" =: ["A"]] ".A"
    test "{.a*}" ["a" =: ["A", "B"]] ".A.B"
    test "{.a*}" ["a" =: emptyDictionary] ""
    test "{.a*}" ["a" =: ["A" =: "1"]] ".A=1"
    test "{.a*}" ["a" =: ["A" =: "1", "B" =: "2"]] ".A=1.B=2"
    test "{.a:1}" [] ""
    test "{.a:1}" ["a" =: ""] "."
    test "{.a:1}" ["a" =: "A"] ".A"
    test "{.a:1}" ["a" =: "AB"] ".A"
    test "{.a:1}" ["a" =: "%B"] ".%25"
    test "{.a:1}" ["a" =: "\xa0\&B"] ".%C2%A0"
    test "{.a:1}" ["a" =: "\xd7ff\&B"] ".%ED%9F%BF"
    test "{.a:1}" ["a" =: "\x10000\&B"] ".%F0%90%80%80"
    test "{.a:1}" ["a" =: emptyList] ""
    test "{.a:1}" ["a" =: ["AB"]] ".AB"
    test "{.a:1}" ["a" =: ["AB", "CD"]] ".AB,CD"
    test "{.a:1}" ["a" =: emptyDictionary] ""
    test "{.a:1}" ["a" =: ["AB" =: "12"]] ".AB,12"
    test "{.a:1}" ["a" =: ["AB" =: "12", "CD" =: "34"]] ".AB,12,CD,34"
    test "{.a,a}" [] ""
    test "{.a,a}" ["a" =: emptyList] ""
    test "{.a,a}" ["a" =: emptyDictionary] ""
    test "{.a,a}" ["a" =: ""] ".."
    test "{.a,b}" ["a" =: ""] "."
    test "{.a,b}" ["b" =: ""] "."
    test "{.%aa}" ["%aa" =: "A"] ".A"
    test "{.%aa}" ["%aa" =: ["A", "B"]] ".A,B"
    test "{.%aa}" ["%aa" =: ["A" =: "1", "B" =: "2"]] ".A,1,B,2"
    test "{.%aa*}" ["%aa" =: "A"] ".A"
    test "{.%aa*}" ["%aa" =: ["A", "B"]] ".A.B"
    test "{.%aa*}" ["%aa" =: ["A" =: "1", "B" =: "2"]] ".A=1.B=2"

  Test.it "handles segment expansion" $ do
    test "{/a}" [] ""
    test "{/a}" ["a" =: emptyList] ""
    test "{/a}" ["a" =: emptyDictionary] ""
    test "{/a}" ["a" =: ""] "/"
    test "{/a}" ["a" =: "A"] "/A"
    test "{/a}" ["a" =: "~"] "/~"
    test "{/a}" ["a" =: "%"] "/%25"
    test "{/a}" ["a" =: "?"] "/%3F"
    test "{/a}" ["a" =: "&"] "/%26"
    test "{/a}" ["a" =: "\xa0"] "/%C2%A0"
    test "{/a}" ["a" =: "\xd7ff"] "/%ED%9F%BF"
    test "{/a}" ["a" =: "\x10000"] "/%F0%90%80%80"
    test "{/a}" ["a" =: ["A"]] "/A"
    test "{/a}" ["a" =: ["A", "B"]] "/A,B"
    test "{/a}" ["a" =: ["%"]] "/%25"
    test "{/a}" ["a" =: ["\xa0"]] "/%C2%A0"
    test "{/a}" ["a" =: ["A" =: "1"]] "/A,1"
    test "{/a}" ["a" =: ["A" =: "1", "B" =: "2"]] "/A,1,B,2"
    test "{/a}" ["a" =: ["A" =: "%"]] "/A,%25"
    test "{/a}" ["a" =: ["A" =: "\xa0"]] "/A,%C2%A0"
    test "{/a}" ["a" =: ["%" =: "1"]] "/%25,1"
    test "{/a*}" [] ""
    test "{/a*}" ["a" =: ""] "/"
    test "{/a*}" ["a" =: "A"] "/A"
    test "{/a*}" ["a" =: emptyList] ""
    test "{/a*}" ["a" =: ["A"]] "/A"
    test "{/a*}" ["a" =: ["A", "B"]] "/A/B"
    test "{/a*}" ["a" =: emptyDictionary] ""
    test "{/a*}" ["a" =: ["A" =: "1"]] "/A=1"
    test "{/a*}" ["a" =: ["A" =: "1", "B" =: "2"]] "/A=1/B=2"
    test "{/a:1}" [] ""
    test "{/a:1}" ["a" =: ""] "/"
    test "{/a:1}" ["a" =: "A"] "/A"
    test "{/a:1}" ["a" =: "AB"] "/A"
    test "{/a:1}" ["a" =: "%B"] "/%25"
    test "{/a:1}" ["a" =: "\xa0\&B"] "/%C2%A0"
    test "{/a:1}" ["a" =: "\xd7ff\&B"] "/%ED%9F%BF"
    test "{/a:1}" ["a" =: "\x10000\&B"] "/%F0%90%80%80"
    test "{/a:1}" ["a" =: emptyList] ""
    test "{/a:1}" ["a" =: ["AB"]] "/AB"
    test "{/a:1}" ["a" =: ["AB", "CD"]] "/AB,CD"
    test "{/a:1}" ["a" =: emptyDictionary] ""
    test "{/a:1}" ["a" =: ["AB" =: "12"]] "/AB,12"
    test "{/a:1}" ["a" =: ["AB" =: "12", "CD" =: "34"]] "/AB,12,CD,34"
    test "{/a,a}" [] ""
    test "{/a,a}" ["a" =: emptyList] ""
    test "{/a,a}" ["a" =: emptyDictionary] ""
    test "{/a,a}" ["a" =: ""] "//"
    test "{/a,b}" ["a" =: ""] "/"
    test "{/a,b}" ["b" =: ""] "/"
    test "{/%aa}" ["%aa" =: "A"] "/A"
    test "{/%aa}" ["%aa" =: ["A", "B"]] "/A,B"
    test "{/%aa}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "/A,1,B,2"
    test "{/%aa*}" ["%aa" =: "A"] "/A"
    test "{/%aa*}" ["%aa" =: ["A", "B"]] "/A/B"
    test "{/%aa*}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "/A=1/B=2"

  Test.it "handles parameter expansion" $ do
    test "{;a}" [] ""
    test "{;a}" ["a" =: emptyList] ""
    test "{;a}" ["a" =: emptyDictionary] ""
    test "{;a}" ["a" =: ""] ";a"
    test "{;a}" ["a" =: "A"] ";a=A"
    test "{;a}" ["a" =: "~"] ";a=~"
    test "{;a}" ["a" =: "%"] ";a=%25"
    test "{;a}" ["a" =: "?"] ";a=%3F"
    test "{;a}" ["a" =: "&"] ";a=%26"
    test "{;a}" ["a" =: "\xa0"] ";a=%C2%A0"
    test "{;a}" ["a" =: "\xd7ff"] ";a=%ED%9F%BF"
    test "{;a}" ["a" =: "\x10000"] ";a=%F0%90%80%80"
    test "{;a}" ["a" =: ["A"]] ";a=A"
    test "{;a}" ["a" =: ["A", "B"]] ";a=A,B"
    test "{;a}" ["a" =: ["%"]] ";a=%25"
    test "{;a}" ["a" =: ["\xa0"]] ";a=%C2%A0"
    test "{;a}" ["a" =: ["A" =: "1"]] ";a=A,1"
    test "{;a}" ["a" =: ["A" =: "1", "B" =: "2"]] ";a=A,1,B,2"
    test "{;a}" ["a" =: ["A" =: "%"]] ";a=A,%25"
    test "{;a}" ["a" =: ["A" =: "\xa0"]] ";a=A,%C2%A0"
    test "{;a}" ["a" =: ["%" =: "1"]] ";a=%25,1"
    test "{;a*}" [] ""
    test "{;a*}" ["a" =: ""] ";a"
    test "{;a*}" ["a" =: "A"] ";a=A"
    test "{;a*}" ["a" =: emptyList] ""
    test "{;a*}" ["a" =: ["A"]] ";a=A"
    test "{;a*}" ["a" =: ["A", "B"]] ";a=A;a=B"
    test "{;a*}" ["a" =: emptyDictionary] ""
    test "{;a*}" ["a" =: ["A" =: "1"]] ";A=1"
    test "{;a*}" ["a" =: ["A" =: "1", "B" =: "2"]] ";A=1;B=2"
    test "{;a:1}" [] ""
    test "{;a:1}" ["a" =: ""] ";a"
    test "{;a:1}" ["a" =: "A"] ";a=A"
    test "{;a:1}" ["a" =: "AB"] ";a=A"
    test "{;a:1}" ["a" =: "%B"] ";a=%25"
    test "{;a:1}" ["a" =: "\xa0\&B"] ";a=%C2%A0"
    test "{;a:1}" ["a" =: "\xd7ff\&B"] ";a=%ED%9F%BF"
    test "{;a:1}" ["a" =: "\x10000\&B"] ";a=%F0%90%80%80"
    test "{;a:1}" ["a" =: emptyList] ""
    test "{;a:1}" ["a" =: ["AB"]] ";a=AB"
    test "{;a:1}" ["a" =: ["AB", "CD"]] ";a=AB,CD"
    test "{;a:1}" ["a" =: emptyDictionary] ""
    test "{;a:1}" ["a" =: ["AB" =: "12"]] ";a=AB,12"
    test "{;a:1}" ["a" =: ["AB" =: "12", "CD" =: "34"]] ";a=AB,12,CD,34"
    test "{;a,a}" [] ""
    test "{;a,a}" ["a" =: emptyList] ""
    test "{;a,a}" ["a" =: emptyDictionary] ""
    test "{;a,a}" ["a" =: ""] ";a;a"
    test "{;a,b}" ["a" =: ""] ";a"
    test "{;a,b}" ["b" =: ""] ";b"
    test "{;%aa}" ["%aa" =: "A"] ";%aa=A"
    test "{;%aa}" ["%aa" =: ["A", "B"]] ";%aa=A,B"
    test "{;%aa}" ["%aa" =: ["A" =: "1", "B" =: "2"]] ";%aa=A,1,B,2"
    test "{;%aa*}" ["%aa" =: "A"] ";%aa=A"
    test "{;%aa*}" ["%aa" =: ["A", "B"]] ";%aa=A;%aa=B"
    test "{;%aa*}" ["%aa" =: ["A" =: "1", "B" =: "2"]] ";A=1;B=2"

  Test.it "handles query expansion" $ do
    test "{?a}" [] ""
    test "{?a}" ["a" =: emptyList] ""
    test "{?a}" ["a" =: emptyDictionary] ""
    test "{?a}" ["a" =: ""] "?a="
    test "{?a}" ["a" =: "A"] "?a=A"
    test "{?a}" ["a" =: "~"] "?a=~"
    test "{?a}" ["a" =: "%"] "?a=%25"
    test "{?a}" ["a" =: "?"] "?a=%3F"
    test "{?a}" ["a" =: "&"] "?a=%26"
    test "{?a}" ["a" =: "\xa0"] "?a=%C2%A0"
    test "{?a}" ["a" =: "\xd7ff"] "?a=%ED%9F%BF"
    test "{?a}" ["a" =: "\x10000"] "?a=%F0%90%80%80"
    test "{?a}" ["a" =: ["A"]] "?a=A"
    test "{?a}" ["a" =: ["A", "B"]] "?a=A,B"
    test "{?a}" ["a" =: ["%"]] "?a=%25"
    test "{?a}" ["a" =: ["\xa0"]] "?a=%C2%A0"
    test "{?a}" ["a" =: ["A" =: "1"]] "?a=A,1"
    test "{?a}" ["a" =: ["A" =: "1", "B" =: "2"]] "?a=A,1,B,2"
    test "{?a}" ["a" =: ["A" =: "%"]] "?a=A,%25"
    test "{?a}" ["a" =: ["A" =: "\xa0"]] "?a=A,%C2%A0"
    test "{?a}" ["a" =: ["%" =: "1"]] "?a=%25,1"
    test "{?a*}" [] ""
    test "{?a*}" ["a" =: ""] "?a="
    test "{?a*}" ["a" =: "A"] "?a=A"
    test "{?a*}" ["a" =: emptyList] ""
    test "{?a*}" ["a" =: ["A"]] "?a=A"
    test "{?a*}" ["a" =: ["A", "B"]] "?a=A&a=B"
    test "{?a*}" ["a" =: emptyDictionary] ""
    test "{?a*}" ["a" =: ["A" =: "1"]] "?A=1"
    test "{?a*}" ["a" =: ["A" =: "1", "B" =: "2"]] "?A=1&B=2"
    test "{?a:1}" [] ""
    test "{?a:1}" ["a" =: ""] "?a="
    test "{?a:1}" ["a" =: "A"] "?a=A"
    test "{?a:1}" ["a" =: "AB"] "?a=A"
    test "{?a:1}" ["a" =: "%B"] "?a=%25"
    test "{?a:1}" ["a" =: "\xa0\&B"] "?a=%C2%A0"
    test "{?a:1}" ["a" =: "\xd7ff\&B"] "?a=%ED%9F%BF"
    test "{?a:1}" ["a" =: "\x10000\&B"] "?a=%F0%90%80%80"
    test "{?a:1}" ["a" =: emptyList] ""
    test "{?a:1}" ["a" =: ["AB"]] "?a=AB"
    test "{?a:1}" ["a" =: ["AB", "CD"]] "?a=AB,CD"
    test "{?a:1}" ["a" =: emptyDictionary] ""
    test "{?a:1}" ["a" =: ["AB" =: "12"]] "?a=AB,12"
    test "{?a:1}" ["a" =: ["AB" =: "12", "CD" =: "34"]] "?a=AB,12,CD,34"
    test "{?a,a}" [] ""
    test "{?a,a}" ["a" =: emptyList] ""
    test "{?a,a}" ["a" =: emptyDictionary] ""
    test "{?a,a}" ["a" =: ""] "?a=&a="
    test "{?a,b}" ["a" =: ""] "?a="
    test "{?a,b}" ["b" =: ""] "?b="
    test "{?%aa}" ["%aa" =: "A"] "?%aa=A"
    test "{?%aa}" ["%aa" =: ["A", "B"]] "?%aa=A,B"
    test "{?%aa}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "?%aa=A,1,B,2"
    test "{?%aa*}" ["%aa" =: "A"] "?%aa=A"
    test "{?%aa*}" ["%aa" =: ["A", "B"]] "?%aa=A&%aa=B"
    test "{?%aa*}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "?A=1&B=2"

  Test.it "handles continuation expansion" $ do
    test "{&a}" [] ""
    test "{&a}" ["a" =: emptyList] ""
    test "{&a}" ["a" =: emptyDictionary] ""
    test "{&a}" ["a" =: ""] "&a="
    test "{&a}" ["a" =: "A"] "&a=A"
    test "{&a}" ["a" =: "~"] "&a=~"
    test "{&a}" ["a" =: "%"] "&a=%25"
    test "{&a}" ["a" =: "?"] "&a=%3F"
    test "{&a}" ["a" =: "&"] "&a=%26"
    test "{&a}" ["a" =: "\xa0"] "&a=%C2%A0"
    test "{&a}" ["a" =: "\xd7ff"] "&a=%ED%9F%BF"
    test "{&a}" ["a" =: "\x10000"] "&a=%F0%90%80%80"
    test "{&a}" ["a" =: ["A"]] "&a=A"
    test "{&a}" ["a" =: ["A", "B"]] "&a=A,B"
    test "{&a}" ["a" =: ["%"]] "&a=%25"
    test "{&a}" ["a" =: ["\xa0"]] "&a=%C2%A0"
    test "{&a}" ["a" =: ["A" =: "1"]] "&a=A,1"
    test "{&a}" ["a" =: ["A" =: "1", "B" =: "2"]] "&a=A,1,B,2"
    test "{&a}" ["a" =: ["A" =: "%"]] "&a=A,%25"
    test "{&a}" ["a" =: ["A" =: "\xa0"]] "&a=A,%C2%A0"
    test "{&a}" ["a" =: ["%" =: "1"]] "&a=%25,1"
    test "{&a*}" [] ""
    test "{&a*}" ["a" =: ""] "&a="
    test "{&a*}" ["a" =: "A"] "&a=A"
    test "{&a*}" ["a" =: emptyList] ""
    test "{&a*}" ["a" =: ["A"]] "&a=A"
    test "{&a*}" ["a" =: ["A", "B"]] "&a=A&a=B"
    test "{&a*}" ["a" =: emptyDictionary] ""
    test "{&a*}" ["a" =: ["A" =: "1"]] "&A=1"
    test "{&a*}" ["a" =: ["A" =: "1", "B" =: "2"]] "&A=1&B=2"
    test "{&a:1}" [] ""
    test "{&a:1}" ["a" =: ""] "&a="
    test "{&a:1}" ["a" =: "A"] "&a=A"
    test "{&a:1}" ["a" =: "AB"] "&a=A"
    test "{&a:1}" ["a" =: "%B"] "&a=%25"
    test "{&a:1}" ["a" =: "\xa0\&B"] "&a=%C2%A0"
    test "{&a:1}" ["a" =: "\xd7ff\&B"] "&a=%ED%9F%BF"
    test "{&a:1}" ["a" =: "\x10000\&B"] "&a=%F0%90%80%80"
    test "{&a:1}" ["a" =: emptyList] ""
    test "{&a:1}" ["a" =: ["AB"]] "&a=AB"
    test "{&a:1}" ["a" =: ["AB", "CD"]] "&a=AB,CD"
    test "{&a:1}" ["a" =: emptyDictionary] ""
    test "{&a:1}" ["a" =: ["AB" =: "12"]] "&a=AB,12"
    test "{&a:1}" ["a" =: ["AB" =: "12", "CD" =: "34"]] "&a=AB,12,CD,34"
    test "{&a,a}" [] ""
    test "{&a,a}" ["a" =: emptyList] ""
    test "{&a,a}" ["a" =: emptyDictionary] ""
    test "{&a,a}" ["a" =: ""] "&a=&a="
    test "{&a,b}" ["a" =: ""] "&a="
    test "{&a,b}" ["b" =: ""] "&b="
    test "{&%aa}" ["%aa" =: "A"] "&%aa=A"
    test "{&%aa}" ["%aa" =: ["A", "B"]] "&%aa=A,B"
    test "{&%aa}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "&%aa=A,1,B,2"
    test "{&%aa*}" ["%aa" =: "A"] "&%aa=A"
    test "{&%aa*}" ["%aa" =: ["A", "B"]] "&%aa=A&%aa=B"
    test "{&%aa*}" ["%aa" =: ["A" =: "1", "B" =: "2"]] "&A=1&B=2"

  Test.describe "supports quasi quotes" $ do

    Test.it "as expressions" $ do
      let qq x y = Just x `Test.shouldBe` Burrito.parse y
      qq [Burrito.uriTemplate||] ""
      qq [Burrito.uriTemplate|a|] "a"
      qq [Burrito.uriTemplate|%00|] "%00"
      qq [Burrito.uriTemplate|{a}|] "{a}"
      qq [Burrito.uriTemplate|{a,b}|] "{a,b}"
      qq [Burrito.uriTemplate|{a}{b}|] "{a}{b}"
      qq [Burrito.uriTemplate|{a*}|] "{a*}"
      qq [Burrito.uriTemplate|{a:1}|] "{a:1}"
      qq [Burrito.uriTemplate|{+a}|] "{+a}"
      qq [Burrito.uriTemplate|{#a}|] "{#a}"
      qq [Burrito.uriTemplate|{.a}|] "{.a}"
      qq [Burrito.uriTemplate|{/a}|] "{/a}"
      qq [Burrito.uriTemplate|{;a}|] "{;a}"
      qq [Burrito.uriTemplate|{?a}|] "{?a}"
      qq [Burrito.uriTemplate|{&a}|] "{&a}"

test
  :: Stack.HasCallStack
  => String
  -> Values
  -> Expected
  -> IO ()
test input values output =
  fmap (Burrito.expand (fmap (fmap unwrapValue) values)) (Burrito.parse input)
  `Test.shouldBe` expectedToMaybe output

data Expected
  = Failure
  | Success String
  deriving (Eq, Show)

instance String.IsString Expected where
  fromString = Success

expectedToMaybe :: Expected -> Maybe String
expectedToMaybe expected = case expected of
  Failure -> Nothing
  Success string -> Just string

type Values = [(String, Value)]

newtype Value = Value
  { unwrapValue :: Burrito.Value
  } deriving (Eq, Show)

instance Exts.IsList Value where
  type Item Value = Item
  fromList items =
    Value $ case Either.partitionEithers $ fmap unwrapItem items of
      (strings, []) -> Burrito.listValue strings
      ([], tuples) -> Burrito.dictionaryValue tuples
      (_, _) -> error $ "fromList " <> show items <> " :: Value"
  toList value = error $ "toList " <> show value

instance String.IsString Value where
  fromString = Value . Burrito.stringValue

emptyList :: Value
emptyList = Value $ Burrito.listValue []

emptyDictionary :: Value
emptyDictionary = Value $ Burrito.dictionaryValue []

newtype Item = Item
  { unwrapItem :: Either String (String, String)
  } deriving (Eq, Show)

instance String.IsString Item where
  fromString = Item . Left

class Pair a where
  type K a
  type V a
  (=:) :: K a -> V a -> a

instance Pair (a, b) where
  type K (a, b) = a
  type V (a, b) = b
  (=:) = (,)

instance Pair Item where
  type K Item = String
  type V Item = String
  key =: value = Item $ Right (key, value)

newtype Template = Template
  { unwrapTemplate :: Burrito.Template
  } deriving (Eq)

instance Show Template where
  show (Template t) = show (Burrito.render t, t)

instance QC.Arbitrary Template where
  arbitrary = Template <$> QC.arbitrary
  shrink = fmap Template . QC.shrink . unwrapTemplate

instance QC.Arbitrary Template.Template where
  arbitrary = Template.Template <$> QC.arbitrary
  shrink = fmap Template.Template . QC.shrink . Template.tokens

instance QC.Arbitrary Token.Token where
  arbitrary = QC.oneof
    [ Token.Expression <$> QC.arbitrary
    , Token.Literal <$> QC.arbitrary
    ]
  shrink token = case token of
    Token.Expression expression -> Token.Expression <$> QC.shrink expression
    Token.Literal literal -> Token.Literal <$> QC.shrink literal

instance QC.Arbitrary Expression.Expression where
  arbitrary = Expression.Expression <$> QC.arbitrary <*> QC.arbitrary
  shrink expression = uncurry Expression.Expression <$> QC.shrink
    ( Expression.operator expression
    , Expression.variables expression
    )

instance QC.Arbitrary Operator.Operator where
  arbitrary = QC.elements
    [ Operator.Ampersand
    , Operator.FullStop
    , Operator.None
    , Operator.NumberSign
    , Operator.PlusSign
    , Operator.QuestionMark
    , Operator.Semicolon
    , Operator.Solidus
    ]
  shrink operator = case operator of
    Operator.None -> []
    _ -> [Operator.None]

instance (QC.Arbitrary a) => QC.Arbitrary (NonEmpty.NonEmpty a) where
  arbitrary = NonEmpty.NonEmpty <$> QC.arbitrary <*> QC.arbitrary
  shrink nonEmpty = uncurry NonEmpty.NonEmpty <$> QC.shrink
    ( NonEmpty.first nonEmpty
    , NonEmpty.rest nonEmpty
    )

instance QC.Arbitrary Variable.Variable where
  arbitrary = Variable.Variable <$> QC.arbitrary <*> QC.arbitrary
  shrink variable = uncurry Variable.Variable <$> QC.shrink
    ( Variable.modifier variable
    , Variable.name variable
    )

instance QC.Arbitrary Modifier.Modifier where
  arbitrary = QC.oneof
    [ pure Modifier.Asterisk
    , do
      maxLength <- QC.choose (1, 9999)
      maybe QC.discard pure $ Modifier.makeColon maxLength
    , pure Modifier.None
    ]
  shrink modifier = case modifier of
    Modifier.Asterisk -> [Modifier.None]
    Modifier.Colon int -> Maybe.mapMaybe Modifier.makeColon $ QC.shrink int
    Modifier.None -> []

instance QC.Arbitrary Name.Name where
  arbitrary = Name.Name <$> QC.arbitrary <*> QC.arbitrary
  shrink name = uncurry Name.Name <$> QC.shrink (Name.first name, Name.rest name)

instance QC.Arbitrary VarChar.VarChar where
  arbitrary = QC.oneof
    [ do
      hi <- QC.suchThat QC.arbitrary Char.isHexDigit
      lo <- QC.suchThat QC.arbitrary Char.isHexDigit
      maybe QC.discard pure $ VarChar.makeEncoded hi lo
    , do
      char <- QC.suchThat QC.arbitrary VarChar.isVarchar
      maybe QC.discard pure $ VarChar.makeUnencoded char
    ]
  shrink varChar = case varChar of
    VarChar.Encoded hi lo -> uncurry VarChar.Encoded <$> QC.shrink (hi, lo)
    VarChar.Unencoded char -> Maybe.mapMaybe VarChar.makeUnencoded $ QC.shrink char

instance QC.Arbitrary Literal.Literal where
  arbitrary = Literal.Literal <$> QC.arbitrary
  shrink = fmap Literal.Literal . QC.shrink . Literal.characters

instance QC.Arbitrary LitChar.LitChar where
  arbitrary = QC.oneof
    [ LitChar.Encoded <$> QC.arbitrary
    , do
      char <- QC.suchThat QC.arbitrary LitChar.isLiteral
      maybe QC.discard pure $ LitChar.makeUnencoded char
    ]
  shrink character = case character of
    LitChar.Encoded word8 -> LitChar.Encoded <$> QC.shrink word8
    LitChar.Unencoded char -> Maybe.mapMaybe LitChar.makeUnencoded $ QC.shrink char
