(import /pantagruel/parser)

(let
  [head [{:kind :sym :text "f"} {:kind :sym :text "u"} {:kind ::} {:kind :sym :text "U"} {:kind :.}]
   line {:kind :line}
   body [{:kind :sym :text "x"} {:kind :binary-operator :text "+"} {:kind :sym :text "y"} {:kind :.}

         {:kind :case} {:kind :sym :text "o"} {:kind :...}
         {:kind :sym :text "y"} {:kind :..} {:kind :sym :text "y"} {:kind :comma}
         {:kind :sym :text "z"} {:kind :..} {:kind :unary-operator :text "~"} {:kind :sym :text "z"} {:kind :.}

         {:kind :some} {:kind :sym :text "u"} {:kind ::} {:kind :sym :text "U"} {:kind :..} {:kind :sym :text "x"} {:kind :.}]
   where {:kind :where}
   res (parser/parse (array/concat @[]
                                   head line body
                                   where
                                   [{:kind :sym :text "g"} {:kind :.} line]))]
  (print (dyn :yydebug))
  (pp res))
