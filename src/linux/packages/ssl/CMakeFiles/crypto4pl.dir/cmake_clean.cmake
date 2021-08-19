file(REMOVE_RECURSE
  "ssl.aux"
  "ssl.blg"
  "ssl.idx"
  "ssl.ilg"
  "ssl.ind"
  "ssl.log"
  "ssl.out"
  "ssl.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/crypto4pl.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
