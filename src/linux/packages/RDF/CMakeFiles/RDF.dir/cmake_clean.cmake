file(REMOVE_RECURSE
  "rdf2pl.aux"
  "rdf2pl.blg"
  "rdf2pl.idx"
  "rdf2pl.ilg"
  "rdf2pl.ind"
  "rdf2pl.log"
  "rdf2pl.out"
  "rdf2pl.toc"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/RDF.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
