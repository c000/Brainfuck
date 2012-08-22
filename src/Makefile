all: Main

Parser: Parser.hs
	ghc -O2 $<

Main: Main.hs Parser
	ghc -O2 $<
