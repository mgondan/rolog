file(REMOVE_RECURSE
  "semweb.aux"
  "semweb.blg"
  "semweb.idx"
  "semweb.ilg"
  "semweb.ind"
  "semweb.log"
  "semweb.out"
  "semweb.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/semweb.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
