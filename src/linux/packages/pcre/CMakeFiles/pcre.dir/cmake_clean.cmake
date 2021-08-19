file(REMOVE_RECURSE
  "pcre.aux"
  "pcre.blg"
  "pcre.idx"
  "pcre.ilg"
  "pcre.ind"
  "pcre.log"
  "pcre.out"
  "pcre.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/pcre.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
