task test, "Run all tests":
  exec r"nim --r --verbosity:0 --nimcache:.nimcache c -o:bin/tests test/tests.nim"

task build, "Build CLIs":
  exec r"nim --verbosity:0 --nimcache:.nimcache c -o:bin/nii src/nii.nim"
  exec r"nim --verbosity:0 --nimcache:.nimcache c -o:bin/nic src/nic.nim"

task run, "Run interpreter":
  exec r"nim --r --verbosity:0 --nimcache:.nimcache c -o:bin/nii src/nii.nim"
