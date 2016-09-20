source scripts/lib.sh
pedantic () {
  stack clean
  stack build --pedantic 
}

check "Pedantic checking" pedantic
