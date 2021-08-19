file(REMOVE_RECURSE
  "language_server.aux"
  "language_server.blg"
  "language_server.idx"
  "language_server.ilg"
  "language_server.ind"
  "language_server.log"
  "language_server.out"
  "language_server.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/language_server.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
