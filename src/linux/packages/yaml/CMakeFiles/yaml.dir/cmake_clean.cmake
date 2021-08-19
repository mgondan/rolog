file(REMOVE_RECURSE
  "yaml.aux"
  "yaml.blg"
  "yaml.idx"
  "yaml.ilg"
  "yaml.ind"
  "yaml.log"
  "yaml.out"
  "yaml.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/yaml.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
