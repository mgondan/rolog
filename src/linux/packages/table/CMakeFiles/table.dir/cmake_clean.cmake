file(REMOVE_RECURSE
  "table.aux"
  "table.blg"
  "table.idx"
  "table.ilg"
  "table.ind"
  "table.log"
  "table.out"
  "table.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/table.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
