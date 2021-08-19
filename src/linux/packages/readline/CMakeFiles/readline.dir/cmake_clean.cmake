file(REMOVE_RECURSE
  "readline.aux"
  "readline.blg"
  "readline.idx"
  "readline.ilg"
  "readline.ind"
  "readline.log"
  "readline.out"
  "readline.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/readline.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
