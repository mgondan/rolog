file(REMOVE_RECURSE
  "utf8proc.aux"
  "utf8proc.blg"
  "utf8proc.idx"
  "utf8proc.ilg"
  "utf8proc.ind"
  "utf8proc.log"
  "utf8proc.out"
  "utf8proc.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/utf8proc.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
