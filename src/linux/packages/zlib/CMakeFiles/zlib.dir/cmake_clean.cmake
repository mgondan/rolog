file(REMOVE_RECURSE
  "zlib.aux"
  "zlib.blg"
  "zlib.idx"
  "zlib.ilg"
  "zlib.ind"
  "zlib.log"
  "zlib.out"
  "zlib.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/zlib.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
