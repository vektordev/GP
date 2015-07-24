ghc ./GRPCore -threaded
echo "To run it: ./GRPCore +RTS -N*numCores* [> logfile 2> errorlog]"
echo "To generate dot graph: dot -O -Tpng ancestry.dot"
