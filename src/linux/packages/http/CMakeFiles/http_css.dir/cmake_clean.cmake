file(REMOVE_RECURSE
  "http.aux"
  "http.blg"
  "http.idx"
  "http.ilg"
  "http.ind"
  "http.log"
  "http.out"
  "http.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/http_css.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
