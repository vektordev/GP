ghc -O2 ./Main -threaded
echo "To run it: ./Main --testrun +RTS -N*numCores* >> log 2>> err"
echo "Syntax to initialize custom Pool: ./Main --start gain min name --iterations n +RTS [...]"
#echo "To generate dot graph: dot -O -Tpng *file*.dot"
