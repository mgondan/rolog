file(REMOVE_RECURSE
  "archive.aux"
  "archive.blg"
  "archive.idx"
  "archive.ilg"
  "archive.ind"
  "archive.log"
  "archive.out"
  "archive.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/archive.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
