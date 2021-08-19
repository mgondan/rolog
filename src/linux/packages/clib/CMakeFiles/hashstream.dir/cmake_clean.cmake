file(REMOVE_RECURSE
  "clib.aux"
  "clib.blg"
  "clib.idx"
  "clib.ilg"
  "clib.ind"
  "clib.log"
  "clib.out"
  "clib.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/hashstream.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
