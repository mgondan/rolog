file(REMOVE_RECURSE
  "pengines.aux"
  "pengines.blg"
  "pengines.idx"
  "pengines.ilg"
  "pengines.ind"
  "pengines.log"
  "pengines.out"
  "pengines.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/pengines_http.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
