file(REMOVE_RECURSE
  "redis.aux"
  "redis.blg"
  "redis.idx"
  "redis.ilg"
  "redis.ind"
  "redis.log"
  "redis.out"
  "redis.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/redis.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
