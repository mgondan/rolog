file(REMOVE_RECURSE
  "sgml.aux"
  "sgml.blg"
  "sgml.idx"
  "sgml.ilg"
  "sgml.ind"
  "sgml.log"
  "sgml.out"
  "sgml.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/sgml.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
