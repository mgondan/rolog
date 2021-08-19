file(REMOVE_RECURSE
  "CMakeFiles/chr_compile"
  "chr.pl"
  "chr_translate.pl"
  "chr_translate_bootstrap1.pl"
  "chr_translate_bootstrap2.pl"
  "guard_entailment.pl"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/chr_compile.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
