file(REMOVE_RECURSE
  "libedit.aux"
  "libedit.blg"
  "libedit.idx"
  "libedit.ilg"
  "libedit.ind"
  "libedit.log"
  "libedit.out"
  "libedit.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/libedit.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
