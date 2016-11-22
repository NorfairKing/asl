source scripts/lib.sh

c () {
  cloc --quiet $@
}

print_colored_text BLUE "Build system:\n"
c \
  orchestra/src

print_colored_text BLUE "System code:\n"
c \
  asl/src

print_colored_text BLUE "System tests:\n"
c \
  asl/test

print_colored_text BLUE "Report code:\n"
c \
  reports
