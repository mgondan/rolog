file(REMOVE_RECURSE
  "paxos.aux"
  "paxos.blg"
  "paxos.idx"
  "paxos.ilg"
  "paxos.ind"
  "paxos.log"
  "paxos.out"
  "paxos.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/paxos.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
