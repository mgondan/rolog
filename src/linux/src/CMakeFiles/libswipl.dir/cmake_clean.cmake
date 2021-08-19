file(REMOVE_RECURSE
  "libswipl.a"
  "libswipl.pdb"
)

# Per-language clean rules from dependency scanning.
foreach(lang C)
  include(CMakeFiles/libswipl.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
