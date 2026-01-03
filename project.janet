(declare-project
  :name "pantagruel"
  :dependencies
  [{:repo "https://github.com/andrewchambers/janet-yacc.git" :tag "main"}
   "testament"
   "spork"])

(declare-source
  :source ["pantagruel.janet" "pantagruel/"])

(declare-executable
  :entry "pantagruel.janet"
  :name "pant")
