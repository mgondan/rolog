file(REMOVE_RECURSE
  "pldoc.aux"
  "pldoc.blg"
  "pldoc.idx"
  "pldoc.ilg"
  "pldoc.ind"
  "pldoc.log"
  "pldoc.out"
  "pldoc.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/pldoc.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
