task test, "Run all tests":
  exec r"nim --r --verbosity:0 --nimcache:.nimcache c -o:bin/tests test/tests.nim"

task build, "Compile interpreter":
  exec r"nim --verbosity:0 --nimcache:.nimcache c -o:bin/nii src/nii.nim"

task run, "Run interpreter":
  exec r"nim --r --verbosity:0 --nimcache:.nimcache c -o:bin/nii src/nii.nim"
