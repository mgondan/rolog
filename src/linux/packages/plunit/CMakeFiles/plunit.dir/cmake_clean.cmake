file(REMOVE_RECURSE
  "plunit.aux"
  "plunit.blg"
  "plunit.idx"
  "plunit.ilg"
  "plunit.ind"
  "plunit.log"
  "plunit.out"
  "plunit.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/plunit.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
