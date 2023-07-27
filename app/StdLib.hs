{-# LANGUAGE OverloadedStrings #-}
module StdLib (stdlib) where

import Data.Text (Text)

stdlib :: [Text]
stdlib = [
	--Arithmatic
	  "(def ++ (fn (x) (+ x 1)))"
	, "(def -- (fn (x) (- x 1)))"
	--Logic
	, "(def >= (fn (x y) (or (> x y) (= x y))))"
	, "(def <= (fn (x y) (or (< x y) (= x y))))"
	, "(def xor (fn (x y) (and (or x y) (not (and  x y)))))"
	--List
	, "(def list-ref (fn (l i) (if (= i 0) (fst l) (list-ref (rst l) (-- i)))))"

	]
