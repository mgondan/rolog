file(REMOVE_RECURSE
  "protobufs.aux"
  "protobufs.blg"
  "protobufs.idx"
  "protobufs.ilg"
  "protobufs.ind"
  "protobufs.log"
  "protobufs.out"
  "protobufs.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/protoc-gen-swipl-subdir2.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
