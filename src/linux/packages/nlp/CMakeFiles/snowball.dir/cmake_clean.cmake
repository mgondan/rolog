file(REMOVE_RECURSE
  "nlp.aux"
  "nlp.blg"
  "nlp.idx"
  "nlp.ilg"
  "nlp.ind"
  "nlp.log"
  "nlp.out"
  "nlp.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/snowball.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
