SOURCES = main.hs ChessBoard.hs Position.hs Color.hs ChessRules.hs GameTree.hs Decisions.hs Tree.hs

main : $(SOURCES)
	ghc -O2 main.hs -o main

